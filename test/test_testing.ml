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
                Fiber.scope (fun s ->
                    List.init 20 (fun _ ->
                        Fiber.fork_promise s (fun () -> (T.get t "/").status) )
                    |> List.map Promise.await ) )
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
                Fiber.scope (fun s ->
                    List.init 8 (fun i ->
                        Fiber.fork_promise s (fun () ->
                            ignore (T.get t ("/" ^ string_of_int i)) ) )
                    |> List.iter Promise.await ) ) ;
            Atomic.get seen
          in
          let first = order 1 in
          equal (list string) first (order 1) ;
          is_true ~msg:"another seed gives another order"
            (List.exists (fun s -> order s <> first) (List.init 9 (( + ) 2))) )
    ]

let () =
  exit
    (Windtrap.run "testing" [request_tests; error_tests; end_tests; sim_tests])
