(* An application behind a reverse proxy that ends TLS. The proxy is at
   127.0.0.1. It tells the client, the scheme and the host of each request in
   the fields X-Forwarded-For, X-Forwarded-Proto and X-Forwarded-Host. The
   middleware give each request an ID and a log line, answer the health probes,
   send the http requests to https, and give a page for an exception.

   Run dune exec examples/proxy_headers/main.exe. In these requests, curl is the
   proxy: it sends the fields that the proxy adds. Without X-Forwarded-For, the
   application reads no proxy field. First, set these shell variables:

   F=X-Forwarded-For:203.0.113.7 P=X-Forwarded-Proto:https
   H=X-Forwarded-Host:app.test

   A request that came over https gets 200 with Strict-Transport-Security and
   X-Request-Id. The body shows the client, the URL and the request ID:

   curl -i -H $F -H $P -H $H 127.0.0.1:8080/

   The application keeps the request ID that the client sends:

   curl -i -H X-Request-Id:abc-123 -H $F -H $P -H $H 127.0.0.1:8080/

   A request that came over http gets 307 to https://app.test/docs?page=2:

   curl -i -H $F -H X-Forwarded-Proto:http -H $H '127.0.0.1:8080/docs?page=2'

   An exception of the handler gets an error page (500) with the request ID. The
   page does not show the exception. The server prints a report:

   curl -i -H $F -H $P -H $H 127.0.0.1:8080/boom

   The probes of the load balancer come directly, with no proxy fields, thus
   over http. The health checks are before the redirect, thus a probe gets 200
   and not 307. /ready has only the checks of the tag "ready". /health has all
   the checks, and a line for each check:

   curl -i 127.0.0.1:8080/ready

   curl -i 127.0.0.1:8080/health

   The server prints one log line for each request, also for the 307. *)

open Weft
open Caravan
open Caravan_middleware

(* Stand-ins for the database and the cache of the application: true when they
   work. [latency] is the time of a query to the database. A real check sends a
   small query, with weft I/O. *)
type backends =
  {database: bool Atomic.t; cache: bool Atomic.t; latency: Span.t Atomic.t}

let backends () =
  { database= Atomic.make true
  ; cache= Atomic.make true
  ; latency= Atomic.make Span.zero }

(* A check that raises gives its failure status, Unhealthy by default. Without
   the database, the application cannot work: Unhealthy gives 503. Without the
   cache, the application is slow but it works: Degraded gives 200.

   A check that does not end in its timeout also gives its failure status. The
   timeout of the database check is 800 ms, below the timeout of the probe (1 s
   by default in Kubernetes). Thus a slow database gives a 503 before the probe
   stops. *)
let checks b =
  let needs flag name () =
    if Atomic.get flag then Health.Healthy else failwith ("no " ^ name)
  in
  let query () =
    Time.sleep Mono (Atomic.get b.latency) ;
    needs b.database "database" ()
  in
  [ Health.check ~timeout:(Span.ms 800) ~tags:["ready"] "database" query
  ; Health.check ~failure:Degraded "cache" (needs b.cache "cache") ]

let ready check = List.mem "ready" (Health.tags check)

(* [request_ids ()] gives the IDs "req-1", "req-2", and so on: the example uses
   them because they are easy to read. These IDs are unique only in one process.
   In production, give a function that makes a UUID.

   The application accepts the ID of the X-Request-Id field of a request. This
   is safe only because the trusted proxy sets or overwrites this field. If the
   proxy only passes the field through, a client chooses the ID of its log lines
   and of its error page. A client that reaches the application directly also
   chooses its ID: keep the port of the application on a private network, or
   give ~accept a check of the peer. *)
let request_ids () =
  let n = Atomic.make 0 in
  fun _req -> Printf.sprintf "req-%d" (Atomic.fetch_and_add n 1 + 1)

let id req = Option.value (Request_id.find req) ~default:"-"

(* The client that Forwarded found: the address in X-Forwarded-For, not the
   address of the proxy. *)
let client req =
  match Forwarded.client req with
  | Some (Ip a) ->
      a
  | Some Unix ->
      "unix"
  | Some Unknown | None ->
      "unknown"

let home req =
  Response.string
    (Printf.sprintf "client %s\nurl %s://%s%s\nrequest %s\n" (client req)
       (Request.scheme req)
       (Option.value (Request.authority req) ~default:"-")
       (Request.path req) (id req) )

(* The page tells the request ID: a user can give it to the support team. *)
let error_page req _exn =
  Response.string ~status:Status.internal_server_error
    (Printf.sprintf "Something went wrong (request %s).\n" (id req))

let routes req =
  match Request.path_only req with
  | "/" ->
      home req
  | "/boom" ->
      failwith "the handler failed"
  | _ ->
      Handler.not_found req

(* [trusted] tells the address of the proxy. Trust only the proxy: a client that
   you trust can choose its address, its scheme and its host.

   The proxy must append the address of its peer to X-Forwarded-For. Forwarded
   takes only the last element (the default ~limit is 1). Thus it ignores the
   addresses that a client puts before it. The proxy must also set or overwrite
   X-Forwarded-Proto and X-Forwarded-Host.

   The proxy (or a host filter before Https_redirect) must refuse each host that
   is not yours. If not, a client chooses the host of a redirect.

   The middleware are in the order of the README. Forwarded is first, thus the
   others get the client, the scheme and the host of the proxy fields. With
   ~host:true, it reads X-Forwarded-Host. Request_id and Access_log come next,
   thus each response has an ID and a log line, also a redirect.

   Health is before Https_redirect, thus a probe over http gets 200. The proxy
   must not forward /health and /ready to the public: the detail of /health
   names the checks, and these names tell an attacker about the system.

   Hsts adds its field only to the responses of https requests. Hsts is before
   Error_page, thus an error page gets the field too. Error_page gets the
   exceptions of the routes. *)
let make ~trusted ~backends ~log ~report =
  let checks = checks backends in
  Handler.compose
    [ Forwarded.middleware ~host:true ~trusted ()
    ; Request_id.middleware ~make:(request_ids ()) ()
    ; (* The line has the full target, with its query: do not put tokens in
         URLs. *)
      Access_log.middleware ~log:(fun e -> log (Access_log.line e)) ()
    ; Health.middleware ~filter:ready ~path:"/ready" checks
    ; Health.middleware ~detail:true ~path:"/health" checks
    ; (* The field is max-age=31536000, for this host only. Give
         ~include_subdomains:true (and ~preload:true for the preload list of the
         browsers) only when each subdomain has HTTPS. *)
      Hsts.middleware ()
    ; (* After HTTPS works, give ~status:Status.permanent_redirect (308). *)
      Https_redirect.middleware ()
    ; Error_page.middleware
        ~report:(fun req e _bt ->
          report
            (Printf.sprintf "request %s: %s" (id req) (Printexc.to_string e)) )
        ~page:error_page () ]
    routes
