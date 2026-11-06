(* The tests of the testing example. This file is the example: each test shows
   one function of caravan.testing, and its comment gives the result.

   Caravan_testing.run serves a handler on 127.0.0.1:80 of a new simulator of
   weft, and calls the function of the test with a client. The time is virtual:
   a sleep of 5 s takes no real time. Each run makes a new counter, thus no
   state goes from one test to the next. *)

open Windtrap
open Weft
open Caravan
module T = Caravan_testing
module C = Caravan_client
module App = Example_testing.App

let status = Testable.make ~pp:Status.pp ~equal:Status.equal

let span = Testable.make ~pp:Span.pp ~equal:Span.equal

let post path = Request.make POST path

(* [timed f] is the value of [f ()] and the virtual time that it took. *)
let timed f =
  let t0 = Time.now Mono in
  let v = f () in
  (v, Time.diff Mono (Time.now Mono) t0)

(* [in_background t f] runs [f] with T.background. The cell gets the value of
   [f] at its end: the test can look at it with no wait. *)
let in_background t f =
  let cell = ref None in
  let p = T.background t (fun () -> cell := Some (f ())) in
  (cell, p)

let body t path = (T.get t path).body

let () =
  exit
  @@ Windtrap.run "testing"
       [ (* get sends a GET and reads the full body. Result: 200, and the count
            is 0. *)
         test "run and get: a new counter is at 0" (fun () ->
             let r = T.run (App.make ()) (fun t -> T.get t "/count") in
             equal status Status.ok r.status ;
             equal string "count: 0\n" r.body )
       ; (* fetch sends a request of Request.make. Result: "count: 1", then
            "count: 2". *)
         test "fetch: two POSTs" (fun () ->
             let a, b =
               T.run (App.make ()) (fun t ->
                   let a = T.fetch t (post "/count") in
                   let b = T.fetch t (post "/count") in
                   (a.body, b.body) )
             in
             equal string "count: 1\n" a ;
             equal string "count: 2\n" b )
       ; (* with_response gives the response before its body, and closes the
            body at its end. Result: the status and the type, with no read of
            the body. *)
         test "with_response: the status, with no read of the body" (fun () ->
             let s, typ =
               T.run (App.make ()) (fun t ->
                   T.with_response t (Request.make GET "/count") (fun resp ->
                       let headers = Response.headers resp in
                       ( Response.status resp
                       , Headers.find "content-type" headers ) ) )
             in
             equal status Status.ok s ;
             equal (option string) (Some "text/plain; charset=utf-8") typ )
       ; (* request gives the response before its body. The body is a stream:
            the test must read it to its end, or close it. Result: 200, the body
            "count: 0" in reads of at most 4 bytes, and a second response that
            the test closes with no read. *)
         test "request: read the body as a stream, or close it" (fun () ->
             let s, text, next =
               T.run (App.make ()) (fun t ->
                   let resp = T.request t (Request.make GET "/count") in
                   let b = C.body resp in
                   let buf = Bytes.create 4 in
                   let rec read acc =
                     match Body.read b buf 0 4 with
                     | 0 ->
                         acc
                     | n ->
                         read (acc @ [Bytes.sub_string buf 0 n])
                   in
                   let text = read [] in
                   let closed = T.request t (Request.make GET "/count") in
                   Body.close (C.body closed) ;
                   (Response.status resp, text, T.get t "/count") )
             in
             equal status Status.ok s ;
             equal (list string) ["coun"; "t: 0"; "\n"] text ;
             equal string "count: 0\n" next.body )
       ; (* client is the client of the run, for a URL with a scheme and a host.
            authority is the host of the server. Result: "127.0.0.1", and 200
            for http://127.0.0.1/count. *)
         test "client and authority: a GET of a full URL" (fun () ->
             let authority, s, text =
               T.run (App.make ()) (fun t ->
                   let url = "http://" ^ T.authority t ^ "/count" in
                   let resp = C.get (T.client t) url in
                   ( T.authority t
                   , Response.status resp
                   , Body.to_string (C.body resp) ) )
             in
             equal string "127.0.0.1" authority ;
             equal status Status.ok s ;
             equal string "count: 0\n" text )
       ; (* server is the server of the run. Result: no connection before the
            first request. The client keeps its connection after a GET: 1. *)
         test "server: the open connections" (fun () ->
             let before, after =
               T.run (App.make ()) (fun t ->
                   let before = Server.open_connections (T.server t) in
                   ignore (T.get t "/count") ;
                   (before, Server.open_connections (T.server t)) )
             in
             equal int 0 before ; equal int 1 after )
       ; (* domains is the number of virtual domains. Result: 1 in run by
            default, 2 with ~domains:2, and 4 in race by default. *)
         test "domains: 1 in run, 4 in race" (fun () ->
             let handler = App.make () in
             equal int 1 (T.run handler T.domains) ;
             equal int 2 (T.run ~domains:2 handler T.domains) ;
             let domains = ref [] in
             T.race ~seeds:2 handler (fun t ->
                 domains := T.domains t :: !domains ) ;
             equal (list int) [4; 4] !domains )
       ; (* race runs the test for each seed from 1 to 20, on 4 virtual domains:
            each seed gives another order of the fibers. Here, 10 POSTs at the
            same time. Result: "count: 10" for each seed. Without the mutex of
            App.increment, the race fails.

            A failure raises Race_failed with the seed. Its text tells how to
            replay it. For example, for the seed 7, this runs only that seed:
            T.race ~replay:7 (fun req -> !app req) (fun t -> ...)

            race gives the same handler and function to each run. Thus the
            handler is in a cell. The function of the run makes the application,
            thus each run and each seed starts with new state, and a replay is
            exact. *)
         test "race: 10 POSTs at the same time give 10, for 20 seeds" (fun () ->
             let app = ref Handler.not_found in
             T.race ~seeds:20
               (fun req -> !app req)
               (fun t ->
                 app := App.make () ;
                 List.init 10 (fun _ ->
                     T.background t (fun () -> T.fetch t (post "/count")) )
                 |> List.iter (fun p -> ignore (Promise.await p)) ;
                 equal string "count: 10\n" (body t "/count") ) ;
             (* ~replay:7 runs only the seed 7. *)
             let seeds = ref [] in
             T.race ~replay:7
               (fun req -> !app req)
               (fun t ->
                 app := App.make () ;
                 seeds := T.seed t :: !seeds ) ;
             equal (list int) [7] !seeds )
       ; (* The first seed that fails stops race. Result: Race_failed with the
            seed 3 and its error, and 3 calls of the function, not 5. *)
         test "Race_failed: the seed and the error" (fun () ->
             let calls = ref 0 in
             raises_match
               (function
                 | T.Race_failed {seed= 3; error= Failure m; _} ->
                     m = "seed 3"
                 | _ ->
                     false )
               (fun () ->
                 T.race ~seeds:5 (App.make ()) (fun t ->
                     incr calls ;
                     if T.seed t = 3 then failwith "seed 3" ) ) ;
             equal int 3 !calls )
       ; (* In a manual run, only advance and advance_to move the time. The GET
            of /slow runs in the background while the test moves the time.
            Result: no reply after 4 s, the reply after 5 s. *)
         test "run ~manual, background and advance: /slow ends after 5 s"
           (fun () ->
             T.run ~manual:true (App.make ()) (fun t ->
                 let reply, _ = in_background t (fun () -> body t "/slow") in
                 T.advance t (Span.s 4) ;
                 equal (option string) None !reply ;
                 T.advance t (Span.s 1) ;
                 equal (option string) (Some "count: 0\n") !reply ) )
       ; (* advance_to moves the time to a deadline. Result: no reply 1 ns
            before 5 s, the reply at 5 s exactly. *)
         test "advance_to: /slow ends at 5 s exactly" (fun () ->
             T.run ~manual:true (App.make ()) (fun t ->
                 let t0 = Time.now Mono in
                 let at d = Option.get (Time.add Mono t0 d) in
                 let reply, _ = in_background t (fun () -> body t "/slow") in
                 T.advance_to t (at (Span.ns 4_999_999_999)) ;
                 equal (option string) None !reply ;
                 T.advance_to t (at (Span.s 5)) ;
                 equal (option string) (Some "count: 0\n") !reply ) )
       ; (* settle lets the fibers run, with no move of the time. Result: a GET
            of /count ends. A POST waits for the 1 ms of the store: it ends only
            after an advance of 1 ms. *)
         test "settle: a GET needs no time, a POST needs 1 ms" (fun () ->
             T.run ~manual:true (App.make ()) (fun t ->
                 let t0 = Time.now Mono in
                 let get, _ = in_background t (fun () -> body t "/count") in
                 T.settle t ;
                 equal (option string) (Some "count: 0\n") !get ;
                 let post, _ =
                   in_background t (fun () -> (T.fetch t (post "/count")).body)
                 in
                 T.settle t ;
                 equal (option string) None !post ;
                 T.advance t (Span.ms 1) ;
                 equal (option string) (Some "count: 1\n") !post ;
                 equal span (Span.ms 1) (Time.diff Mono (Time.now Mono) t0) ) )
       ; (* Promise.cancel of a background request is a client that leaves: the
            client closes the connection, and the handler gets a cancel. Result:
            the increment ends (~protect:true), the count is 1, and the next
            POST gives 2. *)
         test "a client that leaves does not stop an increment" (fun () ->
             (* Without ~protect:true in App.increment, the GET of /count after
                the cancel gets 500. *)
             T.run ~manual:true (App.make ()) (fun t ->
                 let _, p =
                   in_background t (fun () -> T.fetch t (post "/count"))
                 in
                 T.settle t ;
                 Promise.cancel p ;
                 T.advance t (Span.ms 1) ;
                 equal string "count: 1\n" (body t "/count") ;
                 let next, _ =
                   in_background t (fun () -> (T.fetch t (post "/count")).body)
                 in
                 T.advance t (Span.ms 1) ;
                 equal (option string) (Some "count: 2\n") !next ) )
       ; (* With ~stop_on_signals:true, a SIGTERM stops the server with the
            grace of the config, as Server.run does. Result: /slow (5 s) ends in
            a grace of 10 s, with Connection: close. *)
         test "stop_on_signals: a request ends in the grace" (fun () ->
             let config = {Config.default with shutdown_grace= Span.s 10} in
             let r =
               T.run ~manual:true ~stop_on_signals:true ~config (App.make ())
                 (fun t ->
                   let p = T.background t (fun () -> T.get t "/slow") in
                   T.settle t ;
                   T.signal t Signal.SIGTERM ;
                   T.advance t (Span.s 5) ;
                   Promise.await p )
             in
             equal string "count: 0\n" r.body ;
             equal (option string) (Some "close")
               (Headers.find "connection" r.headers) )
       ; (* A grace of 2 s is shorter than /slow. Result: at 2 s exactly, the
            server closes the connection, and the client gets Error Closed. The
            cancel of the handler is not an error of the server. *)
         test "stop_on_signals: the grace ends at its exact time" (fun () ->
             let config = {Config.default with shutdown_grace= Span.s 2} in
             T.run ~manual:true ~stop_on_signals:true ~config (App.make ())
               (fun t ->
                 let reply, _ =
                   in_background t (fun () ->
                       match T.get t "/slow" with
                       | r ->
                           Ok r.body
                       | exception Caravan_client.Error e ->
                           Error e )
                 in
                 T.settle t ;
                 let t0 = Time.now Mono in
                 T.signal t Signal.SIGTERM ;
                 T.advance_to t
                   (Option.get (Time.add Mono t0 (Span.ns 1_999_999_999))) ;
                 is_true ~msg:"no reply before 2 s" (!reply = None) ;
                 T.advance t (Span.ns 1) ;
                 is_true ~msg:"Closed at 2 s"
                   (!reply = Some (Error Caravan_client.Closed)) ;
                 equal int 0 (List.length (T.take_errors t)) ) )
       ; (* take_errors gives the errors of the server, and removes them.
            Result: 500, and the Failure "boom" of the handler. *)
         test "take_errors: GET /boom gives 500 and one error" (fun () ->
             let r, errors =
               T.run (App.make ()) (fun t ->
                   let r = T.get t "/boom" in
                   (r, T.take_errors t) )
             in
             equal status Status.internal_server_error r.status ;
             is_true (List.map fst errors = [Failure "boom"]) )
       ; (* An error that no take_errors took fails the run. Result:
            Unchecked_errors with the Failure "boom". *)
         test "an error that the test does not take: Unchecked_errors"
           (fun () ->
             raises_match
               (function
                 | T.Unchecked_errors [(Failure m, _)] ->
                     m = "boom"
                 | _ ->
                     false )
               (fun () ->
                 T.run (App.make ()) (fun t -> ignore (T.get t "/boom")) ) )
       ; (* The guard limits the time of the function of the test. Result:
            Guard_expired, because /slow takes 5 s and the guard is 2 s. *)
         test "Guard_expired: a guard of 2 s stops a GET of /slow" (fun () ->
             raises_match
               (function
                 | T.Guard_expired d -> Span.equal d (Span.s 2) | _ -> false )
               (fun () ->
                 T.run ~guard:(Span.s 2) (App.make ()) (fun t ->
                     T.get t "/slow" ) ) )
       ; (* Weft_sim.set_latency gives each packet a latency. In the simulator,
            a connect also takes one latency. Result with 10 ms: the first GET
            takes 30 ms (the connect, the request and the response). The second
            GET uses the same connection: 20 ms. *)
         test "sim and set_latency: the time of a GET" (fun () ->
             let first, second =
               T.run (App.make ()) (fun t ->
                   Weft_sim.set_latency (T.sim t) (Span.ms 10) ;
                   let _, first = timed (fun () -> T.get t "/count") in
                   let _, second = timed (fun () -> T.get t "/count") in
                   (first, second) )
             in
             equal span (Span.ms 30) first ;
             equal span (Span.ms 20) second )
       ; (* Weft_sim.set_dns adds a name. Result: a request for counter.test
            goes to the server. *)
         test "sim and set_dns: a request for a name" (fun () ->
             let r =
               T.run (App.make ()) (fun t ->
                   Weft_sim.set_dns (T.sim t) "counter.test"
                     (Addresses ["127.0.0.1"]) ;
                   T.fetch t
                     (Request.make ~authority:"counter.test" GET "/count") )
             in
             equal status Status.ok r.status ;
             equal string "count: 0\n" r.body ) ]
