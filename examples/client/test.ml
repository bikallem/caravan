(* The tests of the client example. Each test runs the gateway through the real
   server and client of caravan, in the simulator of weft. The upstream is a
   route of the same test server: the gateway sends its requests to
   upstream.test, a name for 127.0.0.1, and the test handler gives each request
   for upstream.test to the upstream handler. The gateway needs a client that
   the run makes, thus the test handler reads the gateway from a cell. *)

open Windtrap
open Weft
open Caravan
module T = Caravan_testing
module C = Caravan_client
module App = Example_client.App

let status = Testable.make ~pp:Status.pp ~equal:Status.equal

let mib = 1024 * 1024

(* The events of one run: those of the upstream handler, and the reports of the
   gateway. *)
type events =
  { started: unit Ivar.t  (** /wait started. *)
  ; cancelled: unit Ivar.t  (** /wait got a cancel. *)
  ; got_first: unit Ivar.t  (** /upload got the first line. *)
  ; release: unit Ivar.t  (** /stream can send its second line. *)
  ; reports: string list ref  (** The texts of report, the last first. *) }

let first = "first\n"

let second = "second\n"

(* [read_exactly body n] reads [n] bytes of [body]. *)
let read_exactly body n =
  let buf = Bytes.create n in
  let rec go off =
    if off < n then
      match Body.read body buf off (n - off) with
      | 0 ->
          failwith "the body ended early"
      | k ->
          go (off + k)
  in
  go 0 ; Bytes.to_string buf

(* [echo_head req] is the request line, the authority and the fields of [req],
   one on each line, with the field names in lowercase. *)
let echo_head req =
  let fields =
    List.map
      (fun (k, v) -> String.lowercase_ascii k ^ ": " ^ v)
      (Headers.to_list (Request.headers req))
  in
  Response.string
    ( String.concat "\n"
        ( (Method.to_string (Request.meth req) ^ " " ^ Request.path req)
        :: ("authority: " ^ Option.value ~default:"none" (Request.authority req))
        :: fields )
    ^ "\n" )

let upstream_handler up req =
  match Request.path_only req with
  | "/big" ->
      Response.string (String.make mib 'a')
  | "/echo" ->
      let body = Request.body req in
      Response.stream ?length:(Body.length body) (fun w -> Body.write_to body w)
  | "/upload" ->
      (* The first line, before the client sends the second. *)
      let body = Request.body req in
      let head = read_exactly body (String.length first) in
      Ivar.fill up.got_first () ;
      Response.string (head ^ Body.to_string body)
  | "/stream" ->
      Response.stream (fun w ->
          Body.Writer.write_string w first ;
          Body.Writer.flush w ;
          sync (Ivar.read up.release) ;
          Body.Writer.write_string w second )
  | "/proxy-auth" ->
      Response.string
        ~headers:(Headers.of_list [("Proxy-Authenticate", "Basic")])
        "ok\n"
  | "/slow" ->
      Time.sleep Mono (Span.s 60) ;
      Response.string "late\n"
  | "/wait" ->
      Ivar.fill up.started () ;
      ( try Time.sleep Mono (Span.s 3600)
        with Fiber.Cancelled _ as e -> Ivar.fill up.cancelled () ; raise e ) ;
      Response.string "late\n"
  | "/sink" ->
      (* It reads nothing of the request body. *)
      Time.sleep Mono (Span.s 3600) ;
      Response.string "late\n"
  | "/broken" ->
      Response.stream (fun w ->
          Body.Writer.write_string w first ;
          Body.Writer.flush w ;
          failwith "broken upstream" )
  | _ ->
      echo_head req

let upstream_name = "upstream.test"

(* [run ?manual ?client_config ?upstream f] runs the gateway to [upstream]
   (upstream.test by default) in the test server, and calls [f] with the run and
   its events. [client_config] is for the client of the test. *)
let run ?manual ?client_config ?(upstream = upstream_name) f =
  let up =
    { started= Ivar.create ()
    ; cancelled= Ivar.create ()
    ; got_first= Ivar.create ()
    ; release= Ivar.create ()
    ; reports= ref [] }
  in
  let report text = up.reports := text :: !(up.reports) in
  let app = ref Handler.not_found in
  T.run ?manual ?client_config
    (fun req ->
      if Request.authority req = Some upstream_name then upstream_handler up req
      else !app req )
    (fun t ->
      Weft_sim.set_dns (T.sim t) upstream_name (Addresses ["127.0.0.1"]) ;
      Weft_sim.set_dns (T.sim t) "app.test" (Addresses ["127.0.0.1"]) ;
      C.run ~config:App.client_config (fun client ->
          app := App.make ~report ~client ~upstream ;
          f t up ) )

let lines (r : T.reply) = String.split_on_char '\n' r.body

let has_line line r = List.mem line (lines r)

let header name (r : T.reply) = Headers.find name r.headers

let () =
  exit
  @@ Windtrap.run "client"
       [ test "/api/?sort=name goes to the upstream as /?sort=name" (fun () ->
             let r = run (fun t _ -> T.get t "/api/?sort=name") in
             equal status Status.ok r.status ;
             equal string "GET /?sort=name" (List.hd (lines r)) )
       ; test "/api/x goes to the upstream as /x, with the query" (fun () ->
             let r = run (fun t _ -> T.get t "/api/docs/a.txt?x=1") in
             equal string "GET /docs/a.txt?x=1" (List.hd (lines r)) ;
             let r = run (fun t _ -> T.get t "/api") in
             equal string "GET /" (List.hd (lines r)) ;
             let r = run (fun t _ -> T.get t "/api?x=1") in
             equal string "GET /?x=1" (List.hd (lines r)) )
       ; test "the upstream gets the raw path: %2F stays %2F" (fun () ->
             let r = run (fun t _ -> T.get t "/api/a%2Fb?x=1") in
             equal string "GET /a%2Fb?x=1" (List.hd (lines r)) )
       ; test "the other paths get 404" (fun () ->
             let r = run (fun t _ -> T.get t "/files/") in
             equal status Status.not_found r.status )
       ; test "a segment .. gets 400" (fun () ->
             let r = run (fun t _ -> T.get t "/api/../admin") in
             equal status Status.bad_request r.status )
       ; test "no upstream server: 502" (fun () ->
             (* Nothing listens on the port 81. *)
             let r, reports =
               run ~upstream:"127.0.0.1:81" (fun t up ->
                   let r = T.get t "/api/" in
                   (r, !(up.reports)) )
             in
             equal status Status.bad_gateway r.status ;
             equal (list string)
               ["Caravan_client.Error: cannot connect: Connection refused"]
               reports )
       ; test "the upstream gets its own authority, not the Host of the client"
           (fun () ->
             let r =
               run (fun t _ ->
                   T.fetch t (Request.make ~authority:"app.test" GET "/api/") )
             in
             is_true (has_line "authority: upstream.test" r) )
       ; test "the hop-by-hop fields of the request do not go to the upstream"
           (fun () ->
             let headers =
               Headers.of_list
                 [ ("Connection", "close")
                 ; ("Proxy-Authorization", "Basic eDp5")
                 ; ("X-Keep", "1") ]
             in
             let r =
               run (fun t _ ->
                   T.fetch t (Request.make ~headers GET "/api/fields") )
             in
             equal string "GET /fields" (List.hd (lines r)) ;
             is_true (has_line "x-keep: 1" r) ;
             is_false
               (List.exists
                  (fun l ->
                    String.starts_with ~prefix:"connection:" l
                    || String.starts_with ~prefix:"proxy-authorization:" l )
                  (lines r) ) )
       ; test "no forwarding field goes to the upstream, but Cookie goes"
           (fun () ->
             let headers =
               Headers.of_list
                 [ ("Forwarded", "for=203.0.113.7")
                 ; ("X-Forwarded-For", "203.0.113.7")
                 ; ("X-Forwarded-Host", "admin.test")
                 ; ("X-Forwarded-Proto", "https")
                 ; ("X-Forwarded-Port", "443")
                 ; ("X-Forwarded-Prefix", "/admin")
                 ; ("X-Real-IP", "203.0.113.7")
                 ; ("X-Client-IP", "203.0.113.7")
                 ; ("True-Client-IP", "203.0.113.7")
                 ; ("Cookie", "id=1")
                 ; ("Authorization", "Basic eDp5") ]
             in
             let r =
               run (fun t _ ->
                   T.fetch t (Request.make ~headers GET "/api/fields") )
             in
             is_true (has_line "cookie: id=1" r) ;
             is_true (has_line "authorization: Basic eDp5" r) ;
             is_false
               (List.exists
                  (fun l ->
                    List.exists
                      (fun name -> String.starts_with ~prefix:name l)
                      [ "forwarded:"
                      ; "x-forwarded-for:"
                      ; "x-forwarded-host:"
                      ; "x-forwarded-proto:"
                      ; "x-forwarded-port:"
                      ; "x-forwarded-prefix:"
                      ; "x-real-ip:"
                      ; "x-client-ip:"
                      ; "true-client-ip:" ] )
                  (lines r) ) )
       ; test "the hop-by-hop fields of the response do not come back"
           (fun () ->
             let r = run (fun t _ -> T.get t "/api/proxy-auth") in
             equal string "ok\n" r.body ;
             equal (option string) None (header "proxy-authenticate" r) )
       ; test "a GET of 1 MiB goes through, with its length" (fun () ->
             let r = run (fun t _ -> T.get t "/api/big") in
             equal status Status.ok r.status ;
             equal string (String.make mib 'a') r.body ;
             equal (option string) (Some "1048576") (header "content-length" r) )
       ; test "HEAD gives the Content-Length of the GET, and no body" (fun () ->
             let r =
               run (fun t _ -> T.fetch t (Request.make HEAD "/api/big"))
             in
             equal status Status.ok r.status ;
             equal string "" r.body ;
             equal (option string) (Some "1048576") (header "content-length" r) )
       ; test "a POST of 1 MiB goes through" (fun () ->
             let data = String.init mib (fun i -> Char.chr (i mod 251)) in
             let r =
               run (fun t _ ->
                   let body = Body.of_string data in
                   T.fetch t (Request.make ~body POST "/api/echo") )
             in
             equal status Status.ok r.status ;
             is_true (String.equal data r.body) )
       ; test "a request body over 8 MiB gets 413" (fun () ->
             let data = String.make ((8 * mib) + 1) 'a' in
             let r =
               run (fun t _ ->
                   let body = Body.of_string data in
                   T.fetch t (Request.make ~body POST "/api/echo") )
             in
             equal status Status.content_too_large r.status )
       ; test "the response body goes as it arrives" (fun () ->
             run (fun t up ->
                 T.with_response t (Request.make GET "/api/stream") (fun resp ->
                     let body = C.body resp in
                     (* The upstream waits: the gateway sent the first line
                        before the end of the upstream body. *)
                     equal string first
                       (read_exactly body (String.length first)) ;
                     Ivar.fill up.release () ;
                     equal string second (Body.to_string body) ) ) )
       ; test "the request body goes as it arrives" (fun () ->
             let r =
               run (fun t up ->
                   let body =
                     Body.of_writer (fun w ->
                         Body.Writer.write_string w first ;
                         Body.Writer.flush w ;
                         (* The upstream got the first line before the end of
                            the request body. *)
                         sync (Ivar.read up.got_first) ;
                         Body.Writer.write_string w second )
                   in
                   T.fetch t (Request.make ~body POST "/api/upload") )
             in
             equal string (first ^ second) r.body )
       ; test "no response head in 10 s: 504" (fun () ->
             run ~manual:true (fun t up ->
                 let replied = ref false in
                 let p =
                   T.background t (fun () ->
                       let r = T.get t "/api/slow" in
                       replied := true ;
                       r )
                 in
                 (* The upstream sleeps 60 s: no reply before 10 s. *)
                 T.advance t (Span.ms 9999) ;
                 is_false !replied ;
                 T.advance t (Span.ms 1) ;
                 is_true !replied ;
                 let r = Promise.await p in
                 equal status Status.gateway_timeout r.status ;
                 equal (list string)
                   ["Caravan_client.Error: response timeout"]
                   !(up.reports) ) )
       ; test "no answer to the lookup in 5 s: 504" (fun () ->
             let upstream = "silent.test:80" in
             run ~manual:true ~upstream (fun t up ->
                 (* The DNS server never answers for silent.test. *)
                 Weft_sim.set_dns (T.sim t) "silent.test" No_answer ;
                 let p = T.background t (fun () -> T.get t "/api/") in
                 T.advance t (Span.ms 4999) ;
                 equal (list string) [] !(up.reports) ;
                 T.advance t (Span.ms 1) ;
                 equal status Status.gateway_timeout (Promise.await p).status ;
                 equal (list string)
                   ["Caravan_client.Error: connect timeout"]
                   !(up.reports) ) )
       ; test "an upstream that reads no request body: 504 after 30 s"
           (fun () ->
             (* The test client waits longer than the gateway. *)
             let client_config =
               {C.Config.default with write_timeout= Span.s 3600}
             in
             run ~manual:true ~client_config (fun t up ->
                 let p =
                   T.background t (fun () ->
                       let body = Body.of_string (String.make (4 * mib) 'a') in
                       T.fetch t (Request.make ~body POST "/api/sink") )
                 in
                 T.advance t (Span.ms 29999) ;
                 equal (list string) [] !(up.reports) ;
                 T.advance t (Span.ms 1) ;
                 equal status Status.gateway_timeout (Promise.await p).status ;
                 equal (list string)
                   ["Caravan_client.Error: write timeout"]
                   !(up.reports) ) )
       ; test "a failure after the response head: a truncated body" (fun () ->
             run (fun t _ ->
                 ( match T.get t "/api/broken" with
                 | _ ->
                     fail "the body is complete"
                 | exception C.Error Truncated ->
                     () ) ;
                 T.settle t ;
                 is_true
                   (List.exists
                      (fun (e, _) -> e = Failure "broken upstream")
                      (T.take_errors t) ) ) )
       ; test "a client that leaves cancels the upstream request" (fun () ->
             (* Manual time: the response timeout of the gateway cannot end the
                upstream request. *)
             run ~manual:true (fun t up ->
                 Fiber.scope (fun s ->
                     Fiber.fork s (fun () -> ignore (T.get t "/api/wait")) ;
                     sync (Ivar.read up.started) ;
                     (* The client leaves. *)
                     Fiber.cancel s Exit ) ;
                 (* The upstream handler got its cancel, with no move of the
                    time. If not, the wait is a Weft_sim.Deadlock. *)
                 sync (Ivar.read up.cancelled) ) )
       ; test "make refuses an upstream that is not host:port" (fun () ->
             T.run Handler.not_found (fun t ->
                 let client = T.client t in
                 raises_match
                   (function Invalid_argument _ -> true | _ -> false)
                   (fun () ->
                     let upstream = "evil.example/x" in
                     let make () = App.make ~report:ignore ~client ~upstream in
                     ignore (make () : Handler.t) ) ) ) ]
