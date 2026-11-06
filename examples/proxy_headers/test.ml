(* The tests of the proxy_headers example. Each test runs the handler through
   the real server and client of caravan, in the simulator of weft. The client
   of the simulator is the proxy: its address is "client". *)

open Windtrap
open Weft
open Caravan
open Caravan_middleware
module T = Caravan_testing
module App = Example_proxy_headers.App

let status = Testable.make ~pp:Status.pp ~equal:Status.equal

(* The test trusts the address of the client of the simulator, as main trusts
   127.0.0.1. *)
let the_proxy = function Forwarded.Ip "client" -> true | _ -> false

(* [run ~trusted ~backends f] runs [f] with the application. It gives the result
   of [f], the log lines and the reports, in their order. *)
let run ?(trusted = the_proxy) ?(backends = App.backends ()) f =
  let logs = ref [] and reports = ref [] in
  let app =
    App.make ~trusted ~backends
      ~log:(fun l -> logs := l :: !logs)
      ~report:(fun l -> reports := l :: !reports)
  in
  let v = T.run app f in
  (v, List.rev !logs, List.rev !reports)

(* The fields of the proxy for a request that came over [proto]: the $F, $P (or
   X-Forwarded-Proto:http) and $H of the header comment. *)
let proxy proto =
  [ ("X-Forwarded-For", "203.0.113.7")
  ; ("X-Forwarded-Proto", proto)
  ; ("X-Forwarded-Host", "app.test") ]

let get ?(fields = []) t path = T.get ~headers:(Headers.of_list fields) t path

(* [call ~fields path] is the reply to a GET of [path] in its own run. *)
let call ?trusted ?backends ?fields path =
  let r, _, _ = run ?trusted ?backends (fun t -> get ?fields t path) in
  r

let header name (r : T.reply) = Headers.find name r.headers

(* [without_duration line] is the log line with no duration field (the field
   before the last). The test does not depend on the time of a request. *)
let without_duration line =
  match List.rev (String.split_on_char ' ' line) with
  | last :: _duration :: rest ->
      String.concat " " (List.rev (last :: rest))
  | _ ->
      line

let () =
  exit
  @@ Windtrap.run "proxy_headers"
       [ test "over https: 200, HSTS, an ID, and the client of the proxy"
           (fun () ->
             let r = call ~fields:(proxy "https") "/" in
             equal status Status.ok r.status ;
             equal string
               "client 203.0.113.7\nurl https://app.test/\nrequest req-1\n"
               r.body ;
             equal (option string) (Some "max-age=31536000")
               (header "strict-transport-security" r) ;
             equal (option string) (Some "req-1") (header "x-request-id" r) )
       ; test "the client ID goes in the response, the log and the error page"
           (fun () ->
             let fields = ("X-Request-Id", "abc-123") :: proxy "https" in
             let (r, boom), logs, _ =
               run (fun t ->
                   let r = get ~fields t "/" in
                   (r, get ~fields t "/boom") )
             in
             equal (option string) (Some "abc-123") (header "x-request-id" r) ;
             equal string
               "client 203.0.113.7\nurl https://app.test/\nrequest abc-123\n"
               r.body ;
             equal string "Something went wrong (request abc-123).\n" boom.body ;
             equal (list string)
               [ "203.0.113.7 abc-123 \"GET / HTTP/1.1\" 200 57 -"
               ; "203.0.113.7 abc-123 \"GET /boom HTTP/1.1\" 500 40 -" ]
               (List.map without_duration logs) )
       ; test "the client is the last address of X-Forwarded-For" (fun () ->
             (* A client put 6.6.6.6, and the proxy appended its peer. *)
             let fields =
               ("X-Forwarded-For", "6.6.6.6, 203.0.113.7")
               :: List.tl (proxy "https")
             in
             let r = call ~fields "/" in
             equal string
               "client 203.0.113.7\nurl https://app.test/\nrequest req-1\n"
               r.body )
       ; test "over http: 307 to https://app.test with the path and the query"
           (fun () ->
             let r = call ~fields:(proxy "http") "/docs?page=2" in
             equal status Status.temporary_redirect r.status ;
             equal (option string) (Some "https://app.test/docs?page=2")
               (header "location" r) ;
             equal (option string) None (header "strict-transport-security" r) ;
             equal (option string) (Some "req-1") (header "x-request-id" r) ;
             equal string "" r.body )
       ; test "an exception: the error page with the ID, and one report"
           (fun () ->
             let r, _, reports =
               run (fun t -> get ~fields:(proxy "https") t "/boom")
             in
             equal status Status.internal_server_error r.status ;
             equal string "Something went wrong (request req-1).\n" r.body ;
             (* Hsts is before Error_page. *)
             equal (option string) (Some "max-age=31536000")
               (header "strict-transport-security" r) ;
             equal (list string)
               ["request req-1: Failure(\"the handler failed\")"]
               reports )
       ; test "a probe with no proxy fields gets 200 from /ready, over http"
           (fun () ->
             let r = call "/ready" in
             equal status Status.ok r.status ;
             equal string "Healthy\n" r.body ;
             equal (option string) None (header "strict-transport-security" r) )
       ; test "/health has a line for each check" (fun () ->
             let r = call "/health" in
             equal status Status.ok r.status ;
             equal string "Healthy\ndatabase: Healthy\ncache: Healthy\n" r.body )
       ; test "no cache: Degraded gives 200, and /ready has no cache check"
           (fun () ->
             let backends = App.backends () in
             Atomic.set backends.cache false ;
             let r = call ~backends "/health" in
             equal status Status.ok r.status ;
             equal string
               "Degraded\ndatabase: Healthy\ncache: Degraded (exception)\n"
               r.body ;
             let r = call ~backends "/ready" in
             equal status Status.ok r.status ;
             equal string "Healthy\n" r.body )
       ; test "no database: the check fails, and the probes get 503" (fun () ->
             let backends = App.backends () in
             Atomic.set backends.database false ;
             let r = call ~backends "/health" in
             equal status Status.service_unavailable r.status ;
             equal string
               ( "Unhealthy\ndatabase: Unhealthy (exception)\n"
               ^ "cache: Healthy\n" )
               r.body ;
             let r = call ~backends "/ready" in
             equal status Status.service_unavailable r.status ;
             equal string "Unhealthy\n" r.body )
       ; test "a slow database: 503 after 800 ms, before the probe stops at 1 s"
           (fun () ->
             let backends = App.backends () in
             Atomic.set backends.latency (Span.s 5) ;
             let (r, elapsed), _, _ =
               run ~backends (fun t ->
                   let start = Time.now Mono in
                   let r = get t "/ready" in
                   (r, Time.diff Mono (Time.now Mono) start) )
             in
             equal status Status.service_unavailable r.status ;
             is_true ~msg:"at least 800 ms"
               (Span.compare elapsed (Span.ms 800) >= 0) ;
             is_true ~msg:"less than 1 s" (Span.compare elapsed (Span.s 1) < 0) ;
             let r = call ~backends "/health" in
             equal string
               ( "Unhealthy\ndatabase: Unhealthy (timeout)\n"
               ^ "cache: Healthy\n" )
               r.body )
       ; test "the log has one line for each request, also for the 307"
           (fun () ->
             let (), logs, _ =
               run (fun t ->
                   ignore (get ~fields:(proxy "https") t "/") ;
                   ignore (get ~fields:(proxy "http") t "/docs?page=2") ;
                   ignore (get t "/ready") )
             in
             equal (list string)
               [ "203.0.113.7 req-1 \"GET / HTTP/1.1\" 200 55 -"
               ; "203.0.113.7 req-2 \"GET /docs?page=2 HTTP/1.1\" 307 0 -"
               ; "client req-3 \"GET /ready HTTP/1.1\" 200 8 -" ]
               (List.map without_duration logs) )
       ; test "with no X-Forwarded-For, Forwarded reads no proxy field"
           (fun () ->
             let r =
               call
                 ~fields:
                   [ ("X-Forwarded-Proto", "https")
                   ; ("X-Forwarded-Host", "app.test") ]
                 "/"
             in
             equal status Status.temporary_redirect r.status ;
             equal (option string) (Some "https://127.0.0.1/")
               (header "location" r) )
       ; test "a peer that is not trusted keeps its address, scheme and host"
           (fun () ->
             let r, logs, _ =
               run
                 ~trusted:(fun _ -> false)
                 (fun t -> get ~fields:(proxy "https") t "/")
             in
             equal status Status.temporary_redirect r.status ;
             equal (option string) (Some "https://127.0.0.1/")
               (header "location" r) ;
             equal (list string)
               ["client req-1 \"GET / HTTP/1.1\" 307 0 -"]
               (List.map without_duration logs) ) ]
