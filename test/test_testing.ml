(* The tests of caravan.testing. Each test calls Caravan_testing.run, which
   makes its own simulator. *)

open Windtrap
open Weft
open Caravan
module T = Caravan_testing

let status = Testable.make ~pp:Status.pp ~equal:Status.equal

let ok s = Response.string s

let elapsed f =
  let t0 = Time.now Mono in
  let v = f () in
  (v, Time.diff Mono (Time.now Mono) t0)

let at_least ~msg d got = is_true ~msg (Span.compare got d >= 0)

(* A body that writes "x", then sleeps 1 h. [cancelled] is true after a cancel
   of the sleep. *)
let sleeping_stream cancelled _req =
  Response.stream (fun w ->
      Body.Writer.write_string w "x" ;
      Body.Writer.flush w ;
      try Time.sleep Mono (Span.s 3600)
      with Fiber.Cancelled _ as e -> Atomic.set cancelled true ; raise e )

let is_failure_x = function Failure m, _ -> m = "x" | _ -> false

let request_tests =
  group "Requests"
    [ test "get gives the reply" (fun () ->
          let r = T.run (fun _ -> ok "hi") (fun t -> T.get t "/") in
          equal status Status.ok r.status ;
          equal string "hi" r.body ;
          equal (option string) (Some "text/plain; charset=utf-8")
            (Headers.find "content-type" r.headers) )
    ; test "a request with no authority gets the authority of the server"
        (fun () ->
          let echo req =
            let body = Body.to_string (Request.body req) in
            ok (Option.value ~default:"-" (Request.authority req) ^ " " ^ body)
          in
          let r =
            T.run echo (fun t ->
                T.fetch t
                  (Request.make ~body:(Body.of_string "abc") Method.POST "/") )
          in
          equal string "127.0.0.1 abc" r.body ;
          equal string "127.0.0.1" (T.run echo (fun t -> T.authority t)) )
    ; test "with_response closes the body that the caller did not read"
        (fun () ->
          let chunk = String.make 1024 'x' in
          let long _ =
            Response.stream (fun w ->
                for _ = 1 to 10_000 do
                  Body.Writer.write_string w chunk ;
                  Body.Writer.flush w
                done )
          in
          let open_after =
            T.run long (fun t ->
                T.with_response t (Request.make Method.GET "/") (fun r ->
                    let buf = Bytes.create 16 in
                    ignore (Body.read (Caravan_client.body r) buf 0 16) ) ;
                Time.sleep Mono (Span.s 1) ;
                Server.open_connections (T.server t) )
          in
          equal int 0 open_after )
    ; test "two requests use one connection" (fun () ->
          let id req =
            ok (string_of_int (Conn_info.id (Option.get (Request.conn req))))
          in
          let a, b =
            T.run id (fun t ->
                let a = T.get t "/" in
                (a.body, (T.get t "/").body) )
          in
          equal string a b )
    ; test "get needs a path" (fun () ->
          T.run
            (fun _ -> ok "")
            (fun t ->
              raises_match
                (function Invalid_argument _ -> true | _ -> false)
                (fun () -> T.get t "x") ) )
    ; test "a name of set_dns goes to the server" (fun () ->
          let host req =
            ok (Option.value ~default:"-" (Request.authority req))
          in
          let r =
            T.run host (fun t ->
                Weft_sim.set_dns (T.sim t) "example.com"
                  (Addresses ["127.0.0.1"]) ;
                T.fetch t (Request.make ~authority:"example.com" Method.GET "/") )
          in
          equal string "example.com" r.body ) ]

let error_tests =
  let fails _ = failwith "x" in
  group "Errors"
    [ test "take_errors gives the errors of the handler" (fun () ->
          let r, errors =
            T.run fails (fun t ->
                let r = T.get t "/" in
                (r, T.take_errors t) )
          in
          equal status Status.internal_server_error r.status ;
          equal int 1 (List.length errors) ;
          is_true (List.for_all is_failure_x errors) )
    ; test "an error that no test took fails the run" (fun () ->
          raises_match
            (function T.Unchecked_errors [e] -> is_failure_x e | _ -> false)
            (fun () -> T.run fails (fun t -> ignore (T.get t "/"))) )
    ; test "on_error gets the errors" (fun () ->
          let seen = Atomic.make [] in
          let on_error e _ = Atomic.set seen (e :: Atomic.get seen) in
          T.run ~on_error fails (fun t ->
              ignore (T.get t "/") ;
              ignore (T.take_errors t) ) ;
          is_true (Atomic.get seen = [Failure "x"]) )
    ; test "an exception of on_error wins over the exception of the function"
        (fun () ->
          let on_error _ _ = raise Exit in
          raises_match
            (function Exit -> true | _ -> false)
            (fun () -> T.run ~on_error fails (fun t -> ignore (T.get t "/"))) )
    ; test "an exception of on_error wins also when the function returns"
        (fun () ->
          let on_error _ _ = raise Exit in
          raises_match
            (function Exit -> true | _ -> false)
            (fun () ->
              T.run ~on_error fails (fun t ->
                  try ignore (T.get t "/") with Caravan_client.Error _ -> () ) ) )
    ; test
        "an exception of on_error wins over an exception of the function after \
         the head" (fun () ->
          (* The cancel at the end of the run makes the producer raise Failure,
             thus on_error raises Exit after f raised Not_found. *)
          let producer _ =
            Response.stream (fun w ->
                Body.Writer.write_string w "x" ;
                Body.Writer.flush w ;
                try Time.sleep Mono (Span.s 3600)
                with Fiber.Cancelled _ -> failwith "late" )
          in
          let on_error e _ =
            match e with Failure m when m = "late" -> raise Exit | _ -> ()
          in
          raises_match
            (function Exit -> true | _ -> false)
            (fun () ->
              T.run ~on_error producer (fun t ->
                  ignore (T.request t (Request.make Method.GET "/")) ;
                  raise Not_found ) ) )
    ; test "the hook of a request body of the client" (fun () ->
          let f, log = Server_support.on_end_log () in
          let echo req = ok (Body.to_string (Request.body req)) in
          let r =
            T.run echo (fun t ->
                T.fetch t
                  (Request.make
                     ~body:(Body.on_end f (Body.of_string "abc"))
                     Method.POST "/" ) )
          in
          equal string "abc" r.body ;
          equal (list string) ["Ended 3"] (log ()) ;
          (* A GET with an empty body sends no body: its close runs the hook. *)
          let f, log = Server_support.on_end_log () in
          ignore
            (T.run echo (fun t ->
                 T.fetch t
                   (Request.make ~body:(Body.on_end f Body.empty) Method.GET "/") )
            ) ;
          equal (list string) ["Abandoned 0"] (log ()) )
    ; test "the config goes to the server" (fun () ->
          let config = {Config.default with max_body= Some 4} in
          let read req = ok (Body.to_string (Request.body req)) in
          let r =
            T.run ~config read (fun t ->
                T.fetch t
                  (Request.make
                     ~body:(Body.of_string "0123456789")
                     Method.POST "/" ) )
          in
          equal status Status.content_too_large r.status )
    ; test "run replaces the address of the config" (fun () ->
          let config = {Config.default with addr= Net.Unix "/no/such/dir/s"} in
          equal string "hi"
            (T.run ~config (fun _ -> ok "hi") (fun t -> (T.get t "/").body)) )
    ; test "an exception of Server.start goes out" (fun () ->
          let config = {Config.default with max_connections= 0} in
          raises_match
            (function Invalid_argument _ -> true | _ -> false)
            (fun () -> T.run ~config (fun _ -> ok "") ignore) )
    ; test "the printers" (fun () ->
          expect (Printexc.to_string (T.Guard_expired (Span.s 1)))
          @@ __POS_OF__
               {|Caravan_testing.Guard_expired: the function did not end before the guard of 1 s of virtual time|} ;
          expect (Printexc.to_string (T.Guard_expired (Span.ms 1500)))
          @@ __POS_OF__
               {|Caravan_testing.Guard_expired: the function did not end before the guard of 1500 ms of virtual time|} ;
          expect
            (Printexc.to_string
               (T.Unchecked_errors [(Failure "x", Printexc.get_callstack 0)]) )
          @@ __POS_OF__ {|Caravan_testing.Unchecked_errors: Failure("x")|} ;
          expect (Printexc.to_string (T.Guard_expired (Span.ns 7)))
          @@ __POS_OF__
               {|Caravan_testing.Guard_expired: the function did not end before the guard of 7 ns of virtual time|} ;
          expect (Printexc.to_string Exit) @@ __POS_OF__ {|Stdlib.Exit|} ) ]

let end_tests =
  group "The end of a run"
    [ test "the handlers that still run get a cancel when the function returns"
        (fun () ->
          let cancelled = Atomic.make false in
          T.run (sleeping_stream cancelled) (fun t ->
              ignore (T.request t (Request.make Method.GET "/")) ) ;
          is_true (Atomic.get cancelled) )
    ; test "the handlers that still run get a cancel when the function raises"
        (fun () ->
          let cancelled = Atomic.make false in
          raises_match
            (function Exit -> true | _ -> false)
            (fun () ->
              T.run (sleeping_stream cancelled) (fun t ->
                  ignore (T.request t (Request.make Method.GET "/")) ;
                  raise Exit ) ) ;
          is_true (Atomic.get cancelled) )
    ; test "the guard stops a function that waits for nothing" (fun () ->
          raises_match
            (function
              | T.Guard_expired d -> Span.equal d (Span.s 1) | _ -> false )
            (fun () ->
              T.run ~guard:(Span.s 1)
                (fun _ -> ok "")
                (fun _ -> sync (Ivar.read (Ivar.create ()))) ) )
    ; test "the default guard is 10 min" (fun () ->
          let wait d =
            T.run (fun _ -> ok "") (fun _ -> Time.sleep Mono d ; "v")
          in
          equal string "v" (wait (Span.s 599)) ;
          raises_match
            (function
              | T.Guard_expired d -> Span.equal d (Span.s 600) | _ -> false )
            (fun () -> wait (Span.s 601)) )
    ; test "a guard of Int64.max_int ns does not expire" (fun () ->
          equal string "v"
            (T.run ~guard:(Span.of_ns Int64.max_int)
               (fun _ -> ok "")
               (fun _ -> "v") ) )
    ; test "a timeout of the function is not the guard" (fun () ->
          raises_match
            (function Fiber.Timeout -> true | _ -> false)
            (fun () ->
              T.run
                (fun _ -> ok "")
                (fun _ ->
                  Fiber.with_timeout Mono (Span.ms 10) (fun () ->
                      sync (Ivar.read (Ivar.create ())) ) ) ) )
    ; test "the close after the function does not count for the guard"
        (fun () ->
          (* The response ends before the request body. The client then gives
             the body its close_grace (5 s) at the end of the run, after the
             guard of 1 s. *)
          let body =
            Body.of_writer (fun w ->
                Body.Writer.write_string w "a" ;
                Body.Writer.flush w ;
                Time.sleep Mono (Span.s 3600) )
          in
          let r =
            T.run ~guard:(Span.s 1)
              (fun _ -> ok "done")
              (fun t -> (T.fetch t (Request.make ~body Method.POST "/")).body)
          in
          equal string "done" r ) ]

let sim_tests =
  group "The simulator"
    [ test "the time is virtual" (fun () ->
          let slow _ =
            Time.sleep Mono (Span.s 20) ;
            ok "late"
          in
          let r, d = T.run slow (fun t -> elapsed (fun () -> T.get t "/")) in
          equal string "late" r.body ;
          at_least ~msg:"20 s" (Span.s 20) d )
    ; test "20 requests at the same time on 4 domains" (fun () ->
          let codes =
            T.run ~domains:4
              (fun _ -> ok "")
              (fun t ->
                List.init 20 (fun _ ->
                    T.background t (fun () -> (T.get t "/").status) )
                |> List.map Promise.await )
          in
          equal int 20 (List.length codes) ;
          is_true (List.for_all (Status.equal Status.ok) codes) )
    ; test "sim is the simulator of the run" (fun () ->
          let _, d =
            T.run
              (fun _ -> ok "")
              (fun t ->
                Weft_sim.set_latency (T.sim t) (Span.ms 100) ;
                elapsed (fun () -> T.get t "/") )
          in
          at_least ~msg:"200 ms" (Span.ms 200) d )
    ; test "a seed gives one order" (fun () ->
          let order seed =
            let seen = Atomic.make [] in
            let note req =
              let rec push () =
                let l = Atomic.get seen in
                if not (Atomic.compare_and_set seen l (Request.path req :: l))
                then push ()
              in
              push () ; ok ""
            in
            T.run ~seed ~domains:4 note (fun t ->
                List.init 8 (fun i ->
                    T.background t (fun () ->
                        ignore (T.get t ("/" ^ string_of_int i)) ) )
                |> List.iter Promise.await ) ;
            Atomic.get seen
          in
          let first = order 1 in
          equal (list string) first (order 1) ;
          is_true ~msg:"another seed gives another order"
            (List.exists (fun s -> order s <> first) (List.init 9 (( + ) 2))) )
    ]

(* The handler [note] records each path in [seen]. [race] gives the same handler
   to each seed, thus [order_of_gets] empties [seen] first. *)
let seen : string list Atomic.t = Atomic.make []

let rec push_seen p =
  let l = Atomic.get seen in
  if not (Atomic.compare_and_set seen l (p :: l)) then push_seen p

let note req =
  push_seen (Request.path req) ;
  ok ""

(* [order_of_gets t] starts 8 GETs at the same time, and gives the order in
   which the handler [note] saw them. *)
let order_of_gets t =
  Atomic.set seen [] ;
  List.init 8 (fun i ->
      T.background t (fun () -> ignore (T.get t ("/" ^ string_of_int i))) )
  |> List.iter Promise.await ;
  List.rev (Atomic.get seen)

let is_race_failed ~seed ~domains = function
  | T.Race_failed r ->
      r.seed = seed && r.domains = domains
  | _ ->
      false

let race_tests =
  group "race"
    [ test "each seed, with its seed and its domains" (fun () ->
          let runs = ref [] in
          T.race ~seeds:5 note (fun t ->
              runs := (T.seed t, T.domains t, order_of_gets t) :: !runs ) ;
          let runs = List.rev !runs in
          equal (list int) [1; 2; 3; 4; 5] (List.map (fun (s, _, _) -> s) runs) ;
          List.iter
            (fun (seed, domains, order) ->
              equal int 4 domains ;
              equal ~msg:(string_of_int seed) (list string)
                (T.run ~seed ~domains:4 note order_of_gets)
                order )
            runs ;
          (* A seed that has no effect gives one order for each seed. *)
          is_true ~msg:"the seeds give different orders"
            ( List.length
                (List.sort_uniq compare (List.map (fun (_, _, o) -> o) runs))
            > 1 ) )
    ; test "the first failure stops the race" (fun () ->
          let seeds = ref [] in
          raises_match
            (function
              | T.Race_failed {seed= 3; domains= 4; error= Failure m; _} ->
                  m = "seed 3"
              | _ ->
                  false )
            (fun () ->
              T.race ~seeds:10
                (fun _ -> ok "")
                (fun t ->
                  seeds := T.seed t :: !seeds ;
                  if T.seed t = 3 then failwith "seed 3" ) ) ;
          equal (list int) [1; 2; 3] (List.rev !seeds) ;
          (* The backtrace of the error stays. *)
          let recording = Printexc.backtrace_status () in
          Printexc.record_backtrace true ;
          Fun.protect
            ~finally:(fun () -> Printexc.record_backtrace recording)
            (fun () ->
              match
                T.race ~seeds:1 (fun _ -> ok "") (fun _ -> failwith "x")
              with
              | () ->
                  fail "no Race_failed"
              | exception T.Race_failed {backtrace; _} ->
                  is_true (Printexc.raw_backtrace_length backtrace > 0) ) )
    ; test "replay" (fun () ->
          let seeds = ref [] in
          T.race ~seeds:5 ~replay:4
            (fun _ -> ok "")
            (fun t -> seeds := T.seed t :: !seeds) ;
          equal (list int) [4] !seeds ;
          (* With replay, seeds has no effect. *)
          T.race ~seeds:0 ~replay:1 (fun _ -> ok "") ignore ;
          raises_match (is_race_failed ~seed:4 ~domains:2) (fun () ->
              T.race ~replay:4 ~domains:2
                (fun _ -> ok "")
                (fun _ -> failwith "x") ) )
    ; test "one seed on one domain" (fun () ->
          let runs = ref 0 in
          T.race ~seeds:1 ~domains:1 (fun _ -> ok "") (fun _ -> incr runs) ;
          equal int 1 !runs )
    ; test "bad counts" (fun () ->
          List.iter
            (fun f ->
              raises_match
                (function Invalid_argument _ -> true | _ -> false)
                f )
            [ (fun () -> T.race ~seeds:0 (fun _ -> ok "") ignore)
            ; (fun () -> T.race ~domains:0 (fun _ -> ok "") ignore) ] )
    ; test "the printers" (fun () ->
          expect
            (Printexc.to_string
               (T.Race_failed
                  { seed= 17
                  ; domains= 4
                  ; error= Failure "x"
                  ; backtrace= Printexc.get_callstack 0 } ) )
          @@ __POS_OF__
               {|Caravan_testing.Race_failed: seed 17 (4 domains), replay with ~replay:17: Failure("x")|} ;
          (* The printers of Weft_sim are in test/test_printers.ml: this program
             also links sim_support, whose printers come first. *)
          () )
    ; test "seed and domains of run" (fun () ->
          equal (pair int int) (0, 1)
            (T.run (fun _ -> ok "") (fun t -> (T.seed t, T.domains t))) ;
          equal (pair int int) (9, 3)
            (T.run ~seed:9 ~domains:3
               (fun _ -> ok "")
               (fun t -> (T.seed t, T.domains t)) ) ) ]

(* [ended iv] is [true] once [iv] is filled. *)
let ended iv = Option.is_some (Ivar.peek iv)

let manual_tests =
  group "Manual time"
    [ test "a sleep of 20 s ends at 20 s" (fun () ->
          let slow _ =
            Time.sleep Mono (Span.s 20) ;
            ok "late"
          in
          T.run ~manual:true slow (fun t ->
              let t0 = Time.now Mono in
              let done_ = Ivar.create () in
              let p =
                T.background t (fun () ->
                    let r = T.get t "/" in
                    ignore (Ivar.try_fill done_ ()) ;
                    r )
              in
              T.advance_to t
                (Option.get (Time.add Mono t0 (Span.ns (20_000_000_000 - 1)))) ;
              is_false ~msg:"at 20 s - 1 ns" (ended done_) ;
              T.advance t (Span.ns 1) ;
              is_true ~msg:"at 20 s" (ended done_) ;
              equal string "late" (Promise.await p).body ) )
    ; test "the 408 of a header timeout comes at 3 s" (fun () ->
          let config = {Config.default with header_timeout= Span.s 3} in
          T.run ~manual:true ~config
            (fun _ -> ok "")
            (fun t ->
              let c = Net.connect (Server.local_addr (T.server t)) in
              let t0 = Time.now Mono in
              Net.write c "GET / HTTP/1.1\r\nHo" ;
              let got = Ivar.create () in
              Fiber.scope (fun s ->
                  Fiber.fork_daemon s (fun () ->
                      let buf = Bytes.create 64 in
                      let n = Net.read c buf 0 64 in
                      ignore (Ivar.try_fill got (Bytes.sub_string buf 0 n)) ) ;
                  T.advance_to t
                    (Option.get
                       (Time.add Mono t0 (Span.ns (3_000_000_000 - 1))) ) ;
                  is_true ~msg:"no byte before 3 s" (Ivar.peek got = None) ;
                  T.advance t (Span.ns 1) ;
                  match Ivar.peek got with
                  | Some b ->
                      is_true ~msg:b
                        (String.starts_with ~prefix:"HTTP/1.1 408" b)
                  | None ->
                      fail "no 408 at 3 s" ) ;
              Net.close c ) )
    ; test "a wait for nothing is a deadlock" (fun () ->
          raises_match
            (function Weft_sim.Deadlock _ -> true | _ -> false)
            (fun () ->
              T.run ~manual:true
                (fun _ -> ok "")
                (fun _ -> sync (Ivar.read (Ivar.create ()))) ) )
    ; test "the checks of advance" (fun () ->
          let is_advance_error = function
            | Invalid_argument m ->
                String.starts_with ~prefix:"Caravan_testing.advance" m
            | _ ->
                false
          in
          T.run
            (fun _ -> ok "")
            (fun t ->
              raises_match is_advance_error (fun () -> T.advance t (Span.s 1)) ;
              raises_match is_advance_error (fun () ->
                  T.advance_to t (Time.now Mono) ) ;
              (* settle works in auto mode. *)
              T.settle t ) ;
          T.run ~manual:true
            (fun _ -> ok "")
            (fun t ->
              let t0 = Time.now Mono in
              T.advance t (Span.s 2) ;
              raises_match is_advance_error (fun () -> T.advance_to t t0) ;
              raises_match is_advance_error (fun () ->
                  T.advance t (Span.s (-1)) ) ;
              raises_match is_advance_error (fun () ->
                  T.advance t (Span.of_ns Int64.max_int) ) ;
              T.settle t ;
              (* An advance of zero, and to the time now, are valid. *)
              T.advance t Span.zero ;
              T.advance_to t (Time.now Mono) ) ;
          raises Exit (fun () ->
              T.run ~manual:true (fun _ -> ok "") (fun _ -> raise Exit) ) )
    ; test "the priority of the exceptions in a manual run" (fun () ->
          (* The exception of on_error wins over the exception of f, as in auto
             mode. *)
          raises Not_found (fun () ->
              T.run ~manual:true
                ~on_error:(fun _ _ -> raise Not_found)
                (fun _ -> failwith "h")
                (fun t ->
                  (try ignore (T.get t "/") with _ -> ()) ;
                  raise Exit ) ) )
    ; test "a request body that goes at the end of a manual run" (fun () ->
          let body =
            Body.of_writer (fun w ->
                Body.Writer.write_string w "a" ;
                Body.Writer.flush w ;
                Time.sleep Mono (Span.s 3600) )
          in
          equal string "done"
            (T.run ~manual:true
               (fun _ -> ok "done")
               (fun t -> (T.fetch t (Request.make ~body Method.POST "/")).body) ) )
    ; test "the guard of a manual run" (fun () ->
          raises_match
            (function
              | T.Guard_expired d -> Span.equal d (Span.s 1) | _ -> false )
            (fun () ->
              T.run ~manual:true ~guard:(Span.s 1)
                (fun _ -> ok "")
                (fun t ->
                  Fiber.scope (fun s ->
                      Fiber.fork_daemon s (fun () -> T.advance t (Span.s 2)) ;
                      sync (Ivar.read (Ivar.create ())) ) ) ) ;
          T.run ~manual:true
            (fun _ -> ok "")
            (fun t -> T.advance t (Span.s 3600)) )
    ; test "the end of a manual run cancels the handlers" (fun () ->
          let cancelled = Atomic.make false in
          T.run ~manual:true (sleeping_stream cancelled) (fun t ->
              ignore (T.request t (Request.make Method.GET "/")) ) ;
          is_true (Atomic.get cancelled) )
    ; test "an auto run gives request bodies their grace" (fun () ->
          let log = ref [] in
          let body =
            Body.on_end
              (fun how ~bytes ->
                log :=
                  ( (match how with Body.Ended -> "Ended" | _ -> "other")
                  , bytes )
                  :: !log )
              (Body.of_writer (fun w ->
                   Body.Writer.write_string w "a" ;
                   Body.Writer.flush w ;
                   Time.sleep Mono (Span.s 1) ;
                   Body.Writer.write_string w "b" ) )
          in
          ignore
            (T.run
               (fun _ -> ok "early")
               (fun t -> T.fetch t (Request.make ~body Method.POST "/")) ) ;
          equal (list (pair string int)) [("Ended", 2)] !log ) ]

(* [on_cancel k] waits on an Ivar that nothing fills. At a cancel, it calls [k],
   then raises the cancel again. *)
let on_cancel k =
  try sync (Ivar.read (Ivar.create ()))
  with Fiber.Cancelled _ as e -> k () ; raise e

(* [records_cancel cancelled _] is a handler that waits until a cancel, and then
   sets [cancelled]. *)
let records_cancel cancelled _ = on_cancel (fun () -> Atomic.set cancelled true)

let is_unawaited = function Weft_sim.Unawaited _ -> true | _ -> false

let background_tests =
  group "background"
    [ test "a background GET while the function does another GET" (fun () ->
          let echo req = ok (Request.path req) in
          let a, b =
            T.run echo (fun t ->
                let p = T.background t (fun () -> (T.get t "/a").body) in
                let b = (T.get t "/b").body in
                (Promise.await p, b) )
          in
          equal (pair string string) ("/a", "/b") (a, b) )
    ; test "the end of the function cancels a background request" (fun () ->
          (* In manual mode, a background fiber that the end does not cancel is
             a deadlock. *)
          let cancelled = Atomic.make false in
          T.run ~manual:true (records_cancel cancelled) (fun t ->
              ignore (T.background t (fun () -> T.get t "/")) ;
              T.settle t ) ;
          is_true ~msg:"the handler got a cancel" (Atomic.get cancelled) )
    ; test "the cancel at the end comes before the close of the client"
        (fun () ->
          (* If the client closed first, the request would fail, and the run
             would raise Weft_sim.Unawaited. Each seed gives one cancel. *)
          let cancels = Atomic.make 0 in
          T.race ~seeds:20
            (fun _ -> on_cancel (fun () -> Atomic.incr cancels))
            (fun t ->
              ignore (T.background t (fun () -> T.get t "/")) ;
              Time.sleep Mono (Span.ms 100) ) ;
          equal int 20 (Atomic.get cancels) )
    ; test "a failure that no fiber awaits" (fun () ->
          let f t =
            ignore (T.background t (fun () -> failwith "x")) ;
            T.settle t
          in
          raises_match is_unawaited (fun () -> T.run (fun _ -> ok "") f) ;
          raises_match
            (function
              | T.Race_failed {error= Weft_sim.Unawaited _; _} ->
                  true
              | _ ->
                  false )
            (fun () -> T.race ~seeds:1 (fun _ -> ok "") f) )
    ; test "a failure that the function awaits" (fun () ->
          let m =
            T.run
              (fun _ -> ok "")
              (fun t ->
                match
                  Promise.await (T.background t (fun () -> failwith "x"))
                with
                | () ->
                    "no failure"
                | exception Failure m ->
                    m )
          in
          equal string "x" m )
    ; test "a fiber that raises after its cancel" (fun () ->
          raises_match is_unawaited (fun () ->
              T.run
                (fun _ -> ok "")
                (fun t ->
                  ignore
                    (T.background t (fun () ->
                         on_cancel (fun () -> failwith "cleanup") ) ) ;
                  T.settle t ) ) )
    ; test "the function raises while a background request waits" (fun () ->
          let cancelled = Atomic.make false and waiting = Ivar.create () in
          let h req =
            ignore (Ivar.try_fill waiting ()) ;
            records_cancel cancelled req
          in
          raises Exit (fun () ->
              T.run h (fun t ->
                  ignore (T.background t (fun () -> T.get t "/")) ;
                  sync (Ivar.read waiting) ;
                  raise Exit ) ) ;
          is_true ~msg:"the handler got a cancel" (Atomic.get cancelled) )
    ; test "the guard expires while a background request waits" (fun () ->
          let cancelled = Atomic.make false in
          raises_match
            (function T.Guard_expired _ -> true | _ -> false)
            (fun () ->
              T.run ~guard:(Span.s 1) (records_cancel cancelled) (fun t ->
                  ignore (T.background t (fun () -> T.get t "/")) ;
                  Time.sleep Mono (Span.s 2) ) ) ;
          is_true ~msg:"the handler got a cancel" (Atomic.get cancelled) )
    ; test "the guard measures only the function" (fun () ->
          (* The background fiber ends 1 s after its cancel, after the guard. *)
          let v =
            T.run ~guard:(Span.s 2)
              (fun _ -> ok "")
              (fun t ->
                ignore
                  (T.background t (fun () ->
                       on_cancel (fun () ->
                           Fiber.shield (fun () -> Time.sleep Mono (Span.s 1)) ) )
                  ) ;
                Time.sleep Mono (Span.ms 1500) ;
                "f" )
          in
          equal string "f" v )
    ; test "a background GET in manual time" (fun () ->
          let slow _ =
            Time.sleep Mono (Span.s 20) ;
            ok "late"
          in
          let body =
            T.run ~manual:true slow (fun t ->
                let p = T.background t (fun () -> T.get t "/") in
                T.advance t (Span.s 20) ;
                (Promise.await p).body )
          in
          equal string "late" body )
    ; test "background after the end of the function" (fun () ->
          let closed = Atomic.make false in
          T.run
            (fun _ -> ok "")
            (fun t ->
              ignore
                (T.background t (fun () ->
                     on_cancel (fun () ->
                         match T.background t ignore with
                         | _ ->
                             ()
                         | exception Fiber.Scope_closed ->
                             Atomic.set closed true ) ) ) ;
              T.settle t ) ;
          is_true ~msg:"Scope_closed" (Atomic.get closed) ) ]

let () =
  exit
    (Windtrap.run "testing"
       [ request_tests
       ; error_tests
       ; end_tests
       ; sim_tests
       ; race_tests
       ; manual_tests
       ; background_tests ] )
