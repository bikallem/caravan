(* A gateway sketch with the client of caravan: the requests to /api/** go to
   one upstream server. This is a sketch, not a reverse proxy. It has no Via
   field, no change of Location, no retry, no load balancing, no cache and no
   WebSocket.

   The upstream comes from the configuration (UPSTREAM in main.ml), never from
   the request. Thus a client cannot send the gateway to another server (no
   SSRF). But each path of the upstream is reachable under /api. The gateway
   sends plain HTTP: keep the upstream on this host or on a private network.

   The gateway removes the usual forwarding fields of the client, and adds none.
   If it kept them, an upstream that trusts the address of the gateway would
   also trust a forged client. The upstream gets the other fields of the client,
   also Cookie and Authorization.

   First, run an upstream server on 127.0.0.1:8081. For example, in a directory
   with files:

   python3 -m http.server 8081 --bind 127.0.0.1

   Run dune exec examples/client/main.exe. Set UPSTREAM=host:port for another
   upstream.

   /api/x goes to the upstream as /x, with the query. Python lists the files of
   its directory:

   curl -i '127.0.0.1:8080/api/?sort=name'

   The other paths get 404:

   curl -i 127.0.0.1:8080/files/

   The router refuses a segment .. with 400, thus a path cannot leave /api. Curl
   needs --path-as-is to send it:

   curl -i --path-as-is 127.0.0.1:8080/api/../admin

   Stop the upstream server. Then the gateway gives 502 Bad Gateway, and main
   writes the error of the client to stderr:

   curl -i 127.0.0.1:8080/api/

   The gateway removes the hop-by-hop fields of the request and of the response,
   for example Connection and Proxy-Authorization. The upstream gets its own
   authority in the Host field, not the Host of the client. The two bodies go as
   they arrive: the gateway keeps no full body in memory. The max_body of the
   server configuration (8 MiB by default) limits the request body: a larger
   body gets 413.

   A time limit of the client before the response head gives 504 Gateway
   Timeout. For example: a lookup of the upstream name that takes more than 5 s,
   no response head 10 s after the request, or a write of the request body that
   waits more than 30 s. A failure after the response head ends the connection,
   thus the client gets a truncated body. When the client leaves, the server
   cancels the handler, and the cancel stops the upstream request. *)

open Weft
open Caravan
module C = Caravan_client

(* The client of main.ml and of the test: 5 s for the lookup and the connect, 10
   s from the end of the request to the head of the response, and the defaults
   for the reads and writes of the bodies (30 s). *)
let client_config =
  {C.Config.default with connect_timeout= Span.s 5; response_timeout= Span.s 10}

(* The routes of the upstream are under this prefix. *)
let prefix = "/api"

(* [upstream_path req] is the path and the query of [req] with no prefix. The
   code does not use Caravan_router.rest: it gives the decoded segments with no
   query. The gateway gives the raw path to the upstream, thus %2F stays %2F. *)
let upstream_path req =
  let path = Request.path req and n = String.length prefix in
  let rest = String.sub path n (String.length path - n) in
  if String.starts_with ~prefix:"/" rest then rest else "/" ^ rest

(* The usual forwarding fields: they tell the client and its URL to a server
   behind a proxy. *)
let forwarding =
  [ "Forwarded"
  ; "X-Forwarded-For"
  ; "X-Forwarded-Host"
  ; "X-Forwarded-Proto"
  ; "X-Forwarded-Port"
  ; "X-Forwarded-Prefix"
  ; "X-Real-IP"
  ; "X-Client-IP"
  ; "True-Client-IP" ]

let request_headers req =
  List.fold_left
    (fun h name -> Headers.remove name h)
    (Headers.remove_hop_by_hop (Request.headers req))
    forwarding

(* [head_length headers] is the Content-Length of [headers] if it is one value
   of 1 to 18 ASCII digits. Else it is None. *)
let head_length headers =
  match Headers.find_all "content-length" headers with
  | [v]
    when String.length v >= 1
         && String.length v <= 18
         && String.for_all (function '0' .. '9' -> true | _ -> false) v ->
      Some (int_of_string v)
  | _ ->
      None

(* The body of the response to the client. The client gives a body of length 0
   to a HEAD response. Thus for HEAD, the gateway gives a body with the
   head_length of the upstream, or with no length: the server sends no body for
   HEAD, and it takes the Content-Length from the body. *)
let response_body req resp =
  let body = C.body resp in
  if Request.meth req = HEAD then (
    Body.close body ;
    Body.of_writer ?length:(head_length (Response.headers resp)) ignore )
  else body

let forward ~report ~client ~upstream req =
  let upstream_req =
    Request.make ~authority:upstream ~headers:(request_headers req)
      ~body:(Request.body req) (Request.meth req) (upstream_path req)
  in
  match C.request client upstream_req with
  | resp ->
      (* The server reads the upstream body while it sends it. *)
      Response.make ~status:(Response.status resp)
        ~headers:(Headers.remove_hop_by_hop (Response.headers resp))
        (response_body req resp)
  | exception (C.Error e as exn) -> (
      report (Printexc.to_string exn) ;
      match e with
      | Timeout _ ->
          Response.string ~status:Status.gateway_timeout
            "The upstream server did not answer in time.\n"
      | _ ->
          Response.string ~status:Status.bad_gateway
            "The upstream server is not available.\n" )

(* [make ~report ~client ~upstream] sends /api/** to [upstream] (host:port) with
   [client]. Before a 502 or a 504, it gives the error of the client to
   [report]. It raises Invalid_argument if [upstream] is not host:port. *)
let make ~report ~client ~upstream =
  if not (Request.is_valid_authority upstream) then
    invalid_arg ("not an upstream: " ^ upstream) ;
  let forward = forward ~report ~client ~upstream in
  Caravan_router.(handler [any (prefix ^ "/**") forward])
