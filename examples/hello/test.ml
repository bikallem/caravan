(* The tests of the hello example. Each test runs the handler through the real
   server and client of caravan, in the simulator of weft: the time is virtual,
   thus the test of a 5 s wait takes no real time. *)

open Windtrap
open Weft
open Caravan
module T = Caravan_testing
module App = Example_hello.App

let status = Testable.make ~pp:Status.pp ~equal:Status.equal

let span = Testable.make ~pp:Span.pp ~equal:(fun a b -> Span.compare a b = 0)

(* [timed f] is the value of [f ()] and the virtual time that it took. *)
let timed f =
  let t0 = Time.now Mono in
  let v = f () in
  (v, Time.diff Mono (Time.now Mono) t0)

(* [handler ()] is the handler of a new application. *)
let handler () = (App.make ~log:ignore).handler

let request path body =
  Request.make ~body:(Body.of_string body) Method.POST path

(* [timed_lines t path] reads the body of a GET of [path] as it arrives. It
   gives each line and the time from the start of the request to its arrival. *)
let timed_lines t path =
  let t0 = Time.now Mono in
  T.with_response t (Request.make GET path) (fun resp ->
      let body =
        match Response.kind resp with
        | Body b ->
            b
        | Upgrade _ ->
            fail "not a body"
      in
      let buf = Bytes.create 4096 and line = Buffer.create 16 in
      let rec read lines =
        match Body.read body buf 0 (Bytes.length buf) with
        | 0 ->
            List.rev lines
        | n ->
            let at = Time.diff Mono (Time.now Mono) t0 in
            let lines = ref lines in
            Bytes.iter
              (function
                | '\n' ->
                    lines := (Buffer.contents line, at) :: !lines ;
                    Buffer.clear line
                | c ->
                    Buffer.add_char line c )
              (Bytes.sub buf 0 n) ;
            read !lines
      in
      read [] )

(* The simulator as a world of type Weft.OS, for Server.run. [sim] keeps the
   simulator of the last run, thus a test can send a signal. Caravan_testing.run
   takes no services. Thus the test of app.reporter uses Server.run in this
   world, as main does in the world of the OS. *)
module Sim_os = struct
  let sim = ref None

  let run ?slack:_ ?domains ?signals ?watchdog:_ main =
    Weft_sim.run ?domains ?signals (fun s ->
        sim := Some s ;
        (* A guard of 10 min of virtual time: a test that never stops the server
           fails with Fiber.Timeout, and does not hang. *)
        Fiber.with_timeout Mono (Span.s 600) main )
end

let () =
  exit
  @@ Windtrap.run "hello"
       [ test "GET /hello gives the text" (fun () ->
             let r = T.run (handler ()) (fun t -> T.get t "/hello") in
             equal status Status.ok r.status ;
             equal string "Hello from caravan!\n" r.body )
       ; test "POST /echo gives the same bytes and the same length" (fun () ->
             let r =
               T.run (handler ()) (fun t ->
                   T.fetch t (request "/echo" "some text") )
             in
             equal status Status.ok r.status ;
             equal string "some text" r.body ;
             equal (option string) (Some "9")
               (Headers.find "content-length" r.headers) )
       ; test "POST /echo gives its own Content-Type and nosniff" (fun () ->
             let req =
               Request.make
                 ~headers:(Headers.of_list [("content-type", "text/html")])
                 ~body:(Body.of_string "<script>")
                 Method.POST "/echo"
             in
             let r = T.run (handler ()) (fun t -> T.fetch t req) in
             equal (option string) (Some "application/octet-stream")
               (Headers.find "content-type" r.headers) ;
             equal (option string) (Some "nosniff")
               (Headers.find "x-content-type-options" r.headers) )
       ; test "GET /stream gives 5 lines, 200 ms apart" (fun () ->
             let lines =
               T.run (handler ()) (fun t -> timed_lines t "/stream")
             in
             equal
               (list (pair string span))
               [ ("event 1", Span.ms 0)
               ; ("event 2", Span.ms 200)
               ; ("event 3", Span.ms 400)
               ; ("event 4", Span.ms 600)
               ; ("event 5", Span.ms 800) ]
               lines )
       ; test "GET /slow ends after 5 s" (fun () ->
             let r, took =
               T.run (handler ()) (fun t -> timed (fun () -> T.get t "/slow"))
             in
             equal string "Sorry for the wait.\n" r.body ;
             equal span (Span.s 5) took )
       ; test "a stop of the server ends the stream at once with a last line"
           (fun () ->
             let r =
               T.run ~stop_on_signals:true ~config:App.config (handler ())
                 (fun t ->
                   let p = T.background t (fun () -> T.get t "/stream") in
                   (* Between the lines 2 and 3, the server gets Ctrl-C. *)
                   Time.sleep Mono (Span.ms 300) ;
                   T.signal t Signal.SIGINT ;
                   Promise.await p )
             in
             equal string "event 1\nevent 2\nthe server stops\n" r.body )
       ; test "a request in progress gets 5 s to finish at a stop" (fun () ->
             (* The check of App.config pins the grace at 5 s. The signal comes
                1 ms after the start of /slow, thus the request shows that 5 s
                is enough. *)
             equal span (Span.s 5) App.config.shutdown_grace ;
             let r, took =
               T.run ~stop_on_signals:true ~config:App.config (handler ())
                 (fun t ->
                   timed (fun () ->
                       let p = T.background t (fun () -> T.get t "/slow") in
                       Time.sleep Mono (Span.ms 1) ;
                       T.signal t Signal.SIGINT ;
                       Promise.await p ) )
             in
             equal status Status.ok r.status ;
             equal string "Sorry for the wait.\n" r.body ;
             equal span (Span.s 5) took )
       ; test "an unknown path gives 404" (fun () ->
             let r = T.run (handler ()) (fun t -> T.get t "/nope") in
             equal status Status.not_found r.status )
       ; test "a body larger than max_body gives 413" (fun () ->
             let r =
               T.run ~config:App.config (handler ()) (fun t ->
                   T.fetch t
                     (request "/echo" (String.make ((1024 * 1024) + 1) 'x')) )
             in
             equal status Status.content_too_large r.status )
       ; test "Server.run starts and stops app.reporter" (fun () ->
             let logs = ref [] in
             let log line = logs := line :: !logs in
             let app = App.make ~log in
             (* The network of the simulator is virtual: no real port. *)
             let config = {App.config with addr= Tcp ("127.0.0.1", 80)} in
             Server.run ~config ~services:[app.reporter]
               ~on_start:(fun _server ->
                 Caravan_client.run (fun c ->
                     let r = Caravan_client.get c "http://127.0.0.1/hello" in
                     ignore (Body.to_string (Caravan_client.body r)) ) ;
                 Time.sleep Mono (Span.s 61) ;
                 equal (list string) ["1 request"] !logs ;
                 Weft_sim.signal (Option.get !Sim_os.sim) Signal.SIGTERM )
               (module Sim_os)
               app.handler ;
             equal (list string) ["1 request in total"; "1 request"] !logs )
       ; test "the reporter logs the count of requests each minute" (fun () ->
             let logs = ref [] in
             let log line = logs := line :: !logs in
             let app = App.make ~log in
             T.run ~manual:true app.handler (fun t ->
                 Fiber.scope (fun sw ->
                     App.reporter_start ~log app.requests sw ;
                     ignore (T.get t "/hello") ;
                     T.advance t (Span.s 60) ;
                     equal (list string) ["1 request"] !logs ;
                     ignore (T.get t "/hello") ;
                     ignore (T.get t "/nope") ;
                     T.advance t (Span.s 59) ;
                     equal (list string) ["1 request"] !logs ;
                     T.advance t (Span.s 1) ;
                     equal (list string) ["3 requests"; "1 request"] !logs ) ) )
       ; test "the reporter logs the total at its stop" (fun () ->
             let logs = ref [] in
             let log line = logs := line :: !logs in
             let app = App.make ~log in
             T.run app.handler (fun t ->
                 ignore (T.get t "/hello") ;
                 ignore (T.get t "/nope") ) ;
             App.reporter_stop ~log app.requests ~grace:Span.zero ;
             equal (list string) ["2 requests in total"] !logs ) ]
