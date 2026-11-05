(* The tests of caravan.jobs. They run in the simulator: the delays and the
   backoff need no real time, and the manual mode gives the exact time of each
   end. *)

open Windtrap
open Weft
module J = Caravan_jobs
module S = Sim_support

exception Boom

let exn_name = function Boom -> "Boom" | e -> Printexc.to_string e

let show_reason : J.reason -> string = function
  | Failed (e, _) ->
      "Failed " ^ exn_name e
  | Discarded (e, _) ->
      "Discarded " ^ exn_name e
  | Lost `Not_started ->
      "Lost Not_started"
  | Lost `Running ->
      "Lost Running"

let show (f : J.failure) =
  Printf.sprintf "%s %d %d %s" f.name f.id f.attempts (show_reason f.reason)

let show_state = function
  | `Created ->
      "Created"
  | `Running ->
      "Running"
  | `Stopping ->
      "Stopping"
  | `Stopped ->
      "Stopped"

let show_stats (s : J.stats) =
  Printf.sprintf "%s w%d d%d r%d ok%d re%d f%d x%d l%d" (show_state s.state)
    s.waiting s.delayed s.running s.succeeded s.retried s.failed s.discarded
    s.lost

let stats q = show_stats (J.stats q)

let show_result = function
  | Ok () ->
      "Ok"
  | Error `Full ->
      "Full"
  | Error `Stopped ->
      "Stopped"

let ok r = equal string "Ok" (show_result r)

(* A log of texts, in order. *)
let log () = ref []

let add l s = l := s :: !l

let lines l = List.rev !l

(* [queue ?workers ?capacity l] is a queue whose [on_error] adds each failure to
   [l]. *)
let queue ?workers ?capacity l =
  J.create ?workers ?capacity ~on_error:(fun f -> add l (show f)) ()

(* [with_queue q f] starts [q] in a scope, runs [f sw], then stops [q] with no
   grace. *)
let with_queue q f =
  Fiber.scope (fun sw ->
      J.start q sw ;
      let v = f sw in
      J.stop ~grace:Span.zero q ; v )

let since t0 = Time.diff Mono (Time.now Mono) t0

(* The backoff of [J.retry], computed in a second way: floats for the
   doubling. *)
let expected_backoff ~base ~max ~id ~n =
  let d =
    Int64.of_float
      (Float.min (Int64.to_float max)
         (Int64.to_float base *. (2. ** float_of_int (n - 1))) )
  in
  let j = Hashtbl.hash (id, n) mod 16 in
  Int64.sub d (Int64.mul (Int64.div d 100L) (Int64.of_int j))

(* The guard of the tests with long delays. *)
let long = Span.s 100_000

let invalid f =
  match f () with
  | _ ->
      fail "no Invalid_argument"
  | exception Invalid_argument _ ->
      ()

let fifo_tests =
  group "FIFO, workers, capacity"
    [ test "the jobs run in FIFO order, and drain waits for them" (fun () ->
          S.run (fun _ ->
              let l = log () and errors = log () in
              let q = queue ~workers:1 errors in
              let j = J.job ~name:"log" (fun i -> add l (string_of_int i)) in
              with_queue q (fun _ ->
                  List.iter (fun i -> ok (J.enqueue q j i)) [1; 2; 3] ;
                  J.drain q ;
                  equal (list string) ["1"; "2"; "3"] (lines l) ;
                  equal string "Running w0 d0 r0 ok3 re0 f0 x0 l0" (stats q) ) ;
              equal (list string) [] (lines errors) ;
              equal string "Stopped w0 d0 r0 ok3 re0 f0 x0 l0" (stats q) ) )
    ; test "exactly [workers] jobs run at the same time" (fun () ->
          S.manual (fun sim ->
              let errors = log () in
              let q = queue ~workers:2 errors in
              let gate = Ivar.create () in
              let j = J.job ~name:"gate" (fun () -> sync (Ivar.read gate)) in
              with_queue q (fun _ ->
                  for _ = 1 to 4 do
                    ok (J.enqueue q j ())
                  done ;
                  Weft_sim.settle sim ;
                  equal string "Running w2 d0 r2 ok0 re0 f0 x0 l0" (stats q) ;
                  Ivar.fill gate () ;
                  Weft_sim.settle sim ;
                  equal string "Running w0 d0 r0 ok4 re0 f0 x0 l0" (stats q) ) ;
              equal (list string) [] (lines errors) ) )
    ; test "the default: 4 workers" (fun () ->
          S.manual (fun sim ->
              let q = J.create () in
              let gate = Ivar.create () in
              let j = J.job ~name:"gate" (fun () -> sync (Ivar.read gate)) in
              with_queue q (fun _ ->
                  for _ = 1 to 5 do
                    ok (J.enqueue q j ())
                  done ;
                  Weft_sim.settle sim ;
                  equal string "Running w1 d0 r4 ok0 re0 f0 x0 l0" (stats q) ;
                  Ivar.fill gate () ;
                  Weft_sim.settle sim ) ) )
    ; test "the default capacity: 10_000 jobs" (fun () ->
          S.run (fun _ ->
              let q = J.create () in
              let j = J.job ~name:"j" ignore in
              for _ = 1 to 10_000 do
                ok (J.enqueue q j ())
              done ;
              equal string "Full" (show_result (J.enqueue q j ())) ;
              with_queue q (fun _ -> J.drain q) ;
              equal string "Stopped w0 d0 r0 ok10000 re0 f0 x0 l0" (stats q) ) )
    ; test "capacity: the jobs before the start wait, then they run" (fun () ->
          S.run (fun _ ->
              let l = log () and errors = log () in
              let q = queue ~capacity:2 errors in
              let j = J.job ~name:"log" (add l) in
              ok (J.enqueue q j "a") ;
              ok (J.enqueue q j "b") ;
              equal string "Full" (show_result (J.enqueue q j "c")) ;
              equal string "Created w2 d0 r0 ok0 re0 f0 x0 l0" (stats q) ;
              with_queue q (fun _ ->
                  J.drain q ;
                  equal (list string) ["a"; "b"] (lines l) ;
                  ok (J.enqueue q j "d") ;
                  J.drain q ) ;
              equal string "Stopped w0 d0 r0 ok3 re0 f0 x0 l0" (stats q) ) )
    ; test "capacity counts the delayed jobs, but not a retry" (fun () ->
          S.manual (fun sim ->
              let errors = log () in
              let q = queue ~workers:1 ~capacity:1 errors in
              let fails =
                J.job ~name:"fails"
                  ~retry:(J.retry ~base:(Span.s 1) ())
                  (fun () -> raise Boom)
              in
              let j = J.job ~name:"j" ignore in
              with_queue q (fun _ ->
                  ok (J.enqueue q fails ()) ;
                  Weft_sim.settle sim ;
                  (* The retry is delayed: it takes the place. *)
                  equal string "Running w0 d1 r0 ok0 re1 f0 x0 l0" (stats q) ;
                  equal string "Full" (show_result (J.enqueue q j ())) ;
                  equal string "Full"
                    (show_result (J.enqueue q ~delay:(Span.s 1) j ())) ) ;
              equal (list string) ["fails 0 1 Lost Not_started"] (lines errors) ) )
    ]

let delay_tests =
  group "Delays"
    [ test "a delayed job runs at its due time, in order" (fun () ->
          S.manual (fun sim ->
              let l = log () and errors = log () in
              let q = queue ~workers:1 errors in
              let j = J.job ~name:"log" (add l) in
              with_queue q (fun _ ->
                  let t0 = Time.now Mono in
                  ok (J.enqueue q ~delay:(Span.s 3600) j "h") ;
                  (* The timer waits for 1 h: each new job must wake it. *)
                  Weft_sim.settle sim ;
                  ok (J.enqueue q ~delay:(Span.s 10) j "a") ;
                  ok (J.enqueue q ~delay:(Span.s 10) j "b") ;
                  ok (J.enqueue q ~delay:(Span.s 5) j "c") ;
                  equal string "Running w0 d4 r0 ok0 re0 f0 x0 l0" (stats q) ;
                  S.holds_from sim ~t0 (Span.s 5) ~what:"c ran" (fun () ->
                      List.mem "c" !l ) ;
                  S.holds_from sim ~t0 (Span.s 10) ~what:"b ran" (fun () ->
                      List.mem "b" !l ) ;
                  equal (list string) ["c"; "a"; "b"] (lines l) ;
                  S.holds_from sim ~t0 (Span.s 3600) ~what:"h ran" (fun () ->
                      List.mem "h" !l ) ;
                  equal string "Running w0 d0 r0 ok4 re0 f0 x0 l0" (stats q) ) ;
              equal (list string) [] (lines errors) ) )
    ; test "a delay of zero runs the job" (fun () ->
          S.run (fun _ ->
              let l = log () and errors = log () in
              let q = queue errors in
              let j = J.job ~name:"log" (add l) in
              with_queue q (fun _ ->
                  ok (J.enqueue q ~delay:Span.zero j "z") ;
                  J.drain q ) ;
              equal (list string) ["z"] (lines l) ) )
    ; test "the jobs delayed before the start run after it" (fun () ->
          S.run ~guard:long (fun _ ->
              let l = log () and errors = log () in
              let q = queue errors in
              let j = J.job ~name:"log" (add l) in
              let t0 = Time.now Mono in
              ok (J.enqueue q ~delay:(Span.s 2) j "a") ;
              equal string "Created w0 d1 r0 ok0 re0 f0 x0 l0" (stats q) ;
              with_queue q (fun _ -> J.drain q) ;
              equal (list string) ["a"] (lines l) ;
              equal int64 2_000_000_000L (Span.to_ns (since t0)) ) ) ]

let retry_tests =
  group "Retries"
    [ test "a retry waits for the exact backoff" (fun () ->
          S.run ~guard:long (fun _ ->
              let times = log () and errors = log () in
              let q = queue errors in
              let tries = ref 0 in
              let t0 = Time.now Mono in
              let j =
                J.job ~name:"twice" (fun () ->
                    add times (Span.to_ns (since t0)) ;
                    incr tries ;
                    if !tries <= 2 then raise Boom )
              in
              with_queue q (fun _ ->
                  ok (J.enqueue q j ()) ;
                  J.drain q ) ;
              let b n =
                expected_backoff ~base:1_000_000_000L ~max:300_000_000_000L
                  ~id:0 ~n
              in
              equal (list int64) [0L; b 1; Int64.add (b 1) (b 2)] (lines times) ;
              (* The jitter: 0 to 15 % less than 1 s and 2 s. *)
              equal (list int64) [960_000_000L; 1_860_000_000L] [b 1; b 2] ;
              equal (list string) [] (lines errors) ;
              equal string "Stopped w0 d0 r0 ok1 re2 f0 x0 l0" (stats q) ) )
    ; test "the doubling at exactly max / 2 gives 2 * d, not max" (fun () ->
          (* max / 2 is 1 s (rounded down): the second wait is 2 s less its
             jitter, not max. *)
          S.run ~guard:long (fun _ ->
              let times = log () and errors = log () in
              let q = queue errors in
              let t0 = Time.now Mono in
              let max = 2_000_000_001L in
              let j =
                J.job ~name:"odd"
                  ~retry:
                    (J.retry ~attempts:3 ~base:(Span.s 1) ~max:(Span.of_ns max)
                       () )
                  (fun () ->
                    add times (Span.to_ns (since t0)) ;
                    raise Boom )
              in
              with_queue q (fun _ ->
                  ok (J.enqueue q j ()) ;
                  J.drain q ) ;
              let b n = expected_backoff ~base:1_000_000_000L ~max ~id:0 ~n in
              equal (list int64) [0L; b 1; Int64.add (b 1) (b 2)] (lines times) ;
              equal int64 1_860_000_000L (b 2) ) )
    ; test "the backoff stops at max, with no overflow" (fun () ->
          S.run ~guard:long (fun _ ->
              let times = log () and errors = log () in
              let q = queue errors in
              let t0 = Time.now Mono in
              let base = Span.s 1 and max = Span.s 300 in
              let j =
                (* The default base and max. *)
                J.job ~name:"always" ~retry:(J.retry ~attempts:41 ()) (fun () ->
                    add times (Span.to_ns (since t0)) ;
                    raise Boom )
              in
              with_queue q (fun _ ->
                  (* An id other than 0. *)
                  ok (J.enqueue q (J.job ~name:"first" ignore) ()) ;
                  ok (J.enqueue q j ()) ;
                  J.drain q ) ;
              let rec gaps = function
                | a :: (b :: _ as rest) ->
                    Int64.sub b a :: gaps rest
                | _ ->
                    []
              in
              equal (list int64)
                (List.init 40 (fun i ->
                     expected_backoff ~base:(Span.to_ns base)
                       ~max:(Span.to_ns max) ~id:1 ~n:(i + 1) ) )
                (gaps (lines times)) ;
              equal (list string) ["always 1 41 Failed Boom"] (lines errors) ;
              equal string "Stopped w0 d0 r0 ok1 re40 f1 x0 l0" (stats q) ) )
    ; test "on gives false: one attempt, Discarded, on_failure first" (fun () ->
          S.run ~guard:long (fun _ ->
              let l = log () in
              let q =
                J.create ~on_error:(fun f -> add l ("on_error " ^ show f)) ()
              in
              let j =
                J.job ~name:"d"
                  ~retry:(J.retry ~on:(function Boom -> false | _ -> true) ())
                  ~on_failure:(fun arg f ->
                    add l (Printf.sprintf "on_failure %s %s" arg (show f)) )
                  (fun arg -> if arg = "boom" then raise Boom else raise Exit)
              in
              with_queue q (fun _ ->
                  ok (J.enqueue q j "boom") ;
                  J.drain q ;
                  equal (list string)
                    [ "on_failure boom d 0 1 Discarded Boom"
                    ; "on_error d 0 1 Discarded Boom" ]
                    (lines l) ;
                  equal string "Running w0 d0 r0 ok0 re0 f0 x1 l0" (stats q) ;
                  (* Exit gets the retries. *)
                  l := [] ;
                  ok (J.enqueue q j "exit") ;
                  J.drain q ;
                  equal (list string)
                    [ "on_failure exit d 1 5 Failed Stdlib.Exit"
                    ; "on_error d 1 5 Failed Stdlib.Exit" ]
                    (lines l) ;
                  equal string "Running w0 d0 r0 ok0 re4 f1 x1 l0" (stats q) ) ) )
    ; test "on raises: the job is Failed, then the scope fails" (fun () ->
          S.run (fun _ ->
              let l = log () in
              let q = queue l in
              let j =
                J.job ~name:"j"
                  ~retry:(J.retry ~on:(fun _ -> failwith "on") ())
                  (fun () -> raise Boom)
              in
              raises (Failure "on") (fun () ->
                  Fiber.scope (fun sw ->
                      J.start q sw ;
                      ok (J.enqueue q j ()) ;
                      J.drain q ) ) ;
              equal (list string) ["j 0 1 Failed Boom"] (lines l) ;
              equal string "Stopped w0 d0 r0 ok0 re0 f1 x0 l0" (stats q) ) )
    ; test "on and on_error raise: the queue fails with the exception of on"
        (fun () ->
          S.run (fun _ ->
              let q = J.create ~on_error:(fun _ -> raise Exit) () in
              let j =
                J.job ~name:"j"
                  ~retry:(J.retry ~on:(fun _ -> failwith "on") ())
                  (fun () -> raise Boom)
              in
              raises (Failure "on") (fun () ->
                  Fiber.scope (fun sw ->
                      J.start q sw ;
                      ok (J.enqueue q j ()) ;
                      J.drain q ) ) ) )
    ; test "on can wait: it runs outside the lock" (fun () ->
          S.run (fun _ ->
              let l = log () in
              let q = queue l in
              let tries = ref 0 in
              let j =
                J.job ~name:"j"
                  ~retry:
                    (J.retry ~base:(Span.ms 1)
                       ~on:(fun _ -> Fiber.yield () ; true)
                       () )
                  (fun () ->
                    incr tries ;
                    if !tries = 1 then raise Boom )
              in
              with_queue q (fun _ ->
                  ok (J.enqueue q j ()) ;
                  J.drain q ) ;
              equal (list string) [] (lines l) ;
              equal string "Stopped w0 d0 r0 ok1 re1 f0 x0 l0" (stats q) ) )
    ; test "a retry after the end of the clock: Failed" (fun () ->
          S.manual (fun sim ->
              let l = log () in
              let q = queue l in
              let huge = Span.of_ns Int64.max_int in
              let j =
                J.job ~name:"j"
                  ~retry:(J.retry ~attempts:2 ~base:huge ~max:huge ())
                  (fun () -> raise Boom )
              in
              (* At 20 years, 96 % of the range is after the end. *)
              S.advance sim (Span.s (20 * 365 * 86_400)) ;
              with_queue q (fun _ ->
                  ok (J.enqueue q j ()) ;
                  Weft_sim.settle sim ) ;
              equal (list string) ["j 0 1 Failed Boom"] (lines l) ;
              equal string "Stopped w0 d0 r0 ok0 re0 f1 x0 l0" (stats q) ) )
    ; test "no_retry: one attempt, then Failed" (fun () ->
          S.run (fun _ ->
              let errors = log () in
              let q = queue errors in
              let j =
                J.job ~name:"once" ~retry:J.no_retry (fun () -> raise Boom)
              in
              with_queue q (fun _ ->
                  ok (J.enqueue q j ()) ;
                  J.drain q ) ;
              equal (list string) ["once 0 1 Failed Boom"] (lines errors) ;
              equal string "Stopped w0 d0 r0 ok0 re0 f1 x0 l0" (stats q) ) ) ]

let stop_tests =
  group "Stop"
    [ test "the default grace: 10 s" (fun () ->
          S.manual (fun sim ->
              let errors = log () in
              let q = queue errors in
              let long =
                J.job ~name:"long" (fun () -> Time.sleep Mono (Span.s 100))
              in
              Fiber.scope (fun sw ->
                  J.start q sw ;
                  ok (J.enqueue q long ()) ;
                  Weft_sim.settle sim ;
                  let t0 = Time.now Mono in
                  let c = S.call sw (fun () -> J.stop q) in
                  S.ends_at sim ~t0 (Span.s 10) c ) ;
              equal (list string) ["long 0 1 Lost Running"] (lines errors) ) )
    ; test "drain returns at the stop" (fun () ->
          S.manual (fun sim ->
              let errors = log () in
              let q = queue errors in
              let j = J.job ~name:"later" ignore in
              Fiber.scope (fun sw ->
                  J.start q sw ;
                  ok (J.enqueue q ~delay:(Span.s 3600) j ()) ;
                  let c = S.call sw (fun () -> J.drain q) in
                  is_false ~msg:"drain waits" (S.ended sim c) ;
                  J.stop q ;
                  is_true ~msg:"drain ended" (S.ended sim c) ;
                  S.await c ) ;
              equal (list string) ["later 0 0 Lost Not_started"] (lines errors) ) )
    ; test "a stop with no grace before the queue fiber runs: no job runs"
        (fun () ->
          S.run (fun _ ->
              let l = log () in
              let q = queue l in
              let j = J.job ~name:"j" (fun () -> add l "ran") in
              ok (J.enqueue q j ()) ;
              Fiber.scope (fun sw -> J.start q sw ; J.stop ~grace:Span.zero q) ;
              equal (list string) ["j 0 0 Lost Not_started"] (lines l) ) )
    ; test "stop returns when the running job ends, before the grace" (fun () ->
          S.manual (fun sim ->
              let errors = log () in
              let q = queue errors in
              let j =
                J.job ~name:"slow" (fun () -> Time.sleep Mono (Span.s 2))
              in
              Fiber.scope (fun sw ->
                  J.start q sw ;
                  ok (J.enqueue q j ()) ;
                  Weft_sim.settle sim ;
                  let t0 = Time.now Mono in
                  let c = S.call sw (fun () -> J.stop ~grace:(Span.s 10) q) in
                  Weft_sim.settle sim ;
                  equal string "Stopping w0 d0 r1 ok0 re0 f0 x0 l0" (stats q) ;
                  S.ends_at sim ~t0 (Span.s 2) c ) ;
              equal (list string) [] (lines errors) ;
              equal string "Stopped w0 d0 r0 ok1 re0 f0 x0 l0" (stats q) ) )
    ; test "the jobs that wait run in the grace" (fun () ->
          S.manual (fun sim ->
              let l = log () and errors = log () in
              let q = queue ~workers:1 errors in
              let slow =
                J.job ~name:"slow" (fun () -> Time.sleep Mono (Span.s 2))
              in
              let j = J.job ~name:"log" (add l) in
              Fiber.scope (fun sw ->
                  J.start q sw ;
                  ok (J.enqueue q slow ()) ;
                  ok (J.enqueue q j "waited") ;
                  Weft_sim.settle sim ;
                  let t0 = Time.now Mono in
                  let c = S.call sw (fun () -> J.stop ~grace:(Span.s 10) q) in
                  S.ends_at sim ~t0 (Span.s 2) c ) ;
              equal (list string) ["waited"] (lines l) ;
              equal (list string) [] (lines errors) ;
              equal string "Stopped w0 d0 r0 ok2 re0 f0 x0 l0" (stats q) ) )
    ; test "at the end of the grace: Lost Running, then Lost Not_started"
        (fun () ->
          S.manual (fun sim ->
              let errors = log () in
              let q = queue ~workers:1 errors in
              let long =
                J.job ~name:"long" (fun () -> Time.sleep Mono (Span.s 100))
              in
              Fiber.scope (fun sw ->
                  J.start q sw ;
                  ok (J.enqueue q long ()) ;
                  ok (J.enqueue q long ()) ;
                  Weft_sim.settle sim ;
                  let t0 = Time.now Mono in
                  let c = S.call sw (fun () -> J.stop ~grace:(Span.s 5) q) in
                  S.ends_at sim ~t0 (Span.s 5) c ) ;
              equal (list string)
                ["long 0 1 Lost Running"; "long 1 0 Lost Not_started"]
                (lines errors) ;
              equal string "Stopped w0 d0 r0 ok0 re0 f0 x0 l2" (stats q) ) )
    ; test "a job that ignores its cancel keeps stop waiting, then succeeds"
        (fun () ->
          S.manual (fun sim ->
              let errors = log () in
              let q = queue errors in
              let deaf =
                J.job ~name:"deaf" (fun () ->
                    Fiber.shield (fun () -> Time.sleep Mono (Span.s 100)) )
              in
              Fiber.scope (fun sw ->
                  J.start q sw ;
                  ok (J.enqueue q deaf ()) ;
                  Weft_sim.settle sim ;
                  let t0 = Time.now Mono in
                  let c = S.call sw (fun () -> J.stop ~grace:(Span.s 5) q) in
                  S.ends_at sim ~t0 (Span.s 100) c ) ;
              equal (list string) [] (lines errors) ;
              equal string "Stopped w0 d0 r0 ok1 re0 f0 x0 l0" (stats q) ) )
    ; test "a job that is due at the stop runs in the grace" (fun () ->
          S.run (fun _ ->
              let l = log () in
              let q = queue ~workers:1 l in
              let j = J.job ~name:"due" (fun () -> add l "ran") in
              Fiber.scope (fun sw ->
                  J.start q sw ;
                  ok (J.enqueue q ~delay:Span.zero j ()) ;
                  J.stop ~grace:(Span.s 10) q ) ;
              equal (list string) ["ran"] (lines l) ;
              equal string "Stopped w0 d0 r0 ok1 re0 f0 x0 l0" (stats q) ) )
    ; test "stop: the delayed jobs and the retries are lost at once" (fun () ->
          S.manual (fun sim ->
              let errors = log () in
              let q = queue errors in
              let j = J.job ~name:"later" ignore in
              let fails =
                J.job ~name:"fails"
                  ~retry:(J.retry ~base:(Span.s 3600) ~max:(Span.s 3600) ())
                  (fun () -> raise Boom)
              in
              Fiber.scope (fun sw ->
                  J.start q sw ;
                  ok (J.enqueue q ~delay:(Span.s 7200) j ()) ;
                  ok (J.enqueue q fails ()) ;
                  Weft_sim.settle sim ;
                  equal string "Running w0 d2 r0 ok0 re1 f0 x0 l0" (stats q) ;
                  let c = S.call sw (fun () -> J.stop ~grace:(Span.s 10) q) in
                  is_true ~msg:"stop ended at once" (S.ended sim c) ;
                  S.await c ) ;
              equal (list string)
                ["fails 1 1 Lost Not_started"; "later 0 0 Lost Not_started"]
                (lines errors) ;
              equal string "Stopped w0 d0 r0 ok0 re1 f0 x0 l2" (stats q) ) )
    ; test "an attempt that fails after the stop gets no retry" (fun () ->
          S.manual (fun sim ->
              let errors = log () in
              let q = queue errors in
              let gate = Ivar.create () in
              let j =
                J.job ~name:"late" (fun () ->
                    sync (Ivar.read gate) ;
                    raise Boom )
              in
              let other = J.job ~name:"other" ignore in
              Fiber.scope (fun sw ->
                  J.start q sw ;
                  ok (J.enqueue q j ()) ;
                  Weft_sim.settle sim ;
                  let c = S.call sw (fun () -> J.stop ~grace:(Span.s 10) q) in
                  Weft_sim.settle sim ;
                  equal string "Stopped" (show_result (J.enqueue q other ())) ;
                  Ivar.fill gate () ;
                  is_true ~msg:"stop ended" (S.ended sim c) ;
                  S.await c ;
                  equal string "Stopped" (show_result (J.enqueue q other ())) ) ;
              equal (list string) ["late 0 1 Failed Boom"] (lines errors) ;
              equal string "Stopped w0 d0 r0 ok0 re0 f1 x0 l0" (stats q) ) )
    ; test "two stops: the two calls return at the end of the first" (fun () ->
          S.manual (fun sim ->
              let errors = log () in
              let q = queue errors in
              let j =
                J.job ~name:"slow" (fun () -> Time.sleep Mono (Span.s 2))
              in
              Fiber.scope (fun sw ->
                  J.start q sw ;
                  ok (J.enqueue q j ()) ;
                  Weft_sim.settle sim ;
                  let t0 = Time.now Mono in
                  let c1 = S.call sw (fun () -> J.stop ~grace:(Span.s 10) q) in
                  let c2 = S.call sw (fun () -> J.stop ~grace:(Span.s 1) q) in
                  S.ends_at sim ~t0 (Span.s 2) c1 ;
                  is_true ~msg:"the second stop ended" (S.ended sim c2) ;
                  S.await c2 ) ;
              equal (list string) [] (lines errors) ) )
    ; test "a cancel of the scope with no stop reports each job" (fun () ->
          S.run (fun _ ->
              let errors = log () in
              let q = queue ~workers:1 errors in
              let started = Ivar.create () in
              let long =
                J.job ~name:"long" (fun () ->
                    ignore (Ivar.try_fill started ()) ;
                    Time.sleep Mono (Span.s 100) )
              in
              Fiber.scope (fun inner ->
                  J.start q inner ;
                  ok (J.enqueue q long ()) ;
                  ok (J.enqueue q long ()) ;
                  ok (J.enqueue q ~delay:(Span.s 5) long ()) ;
                  sync (Ivar.read started) ;
                  Fiber.cancel inner Exit ) ;
              equal (list string)
                [ "long 0 1 Lost Running"
                ; "long 1 0 Lost Not_started"
                ; "long 2 0 Lost Not_started" ]
                (lines errors) ;
              equal string "Stopped w0 d0 r0 ok0 re0 f0 x0 l3" (stats q) ;
              equal string "Stopped" (show_result (J.enqueue q long ())) ) ) ]

let life_tests =
  group "The life of a queue"
    [ test "stop before start reports the jobs at once" (fun () ->
          S.run (fun _ ->
              let errors = log () in
              let q = queue errors in
              let j = J.job ~name:"j" ignore in
              ok (J.enqueue q j ()) ;
              ok (J.enqueue q ~delay:(Span.s 1) j ()) ;
              J.stop q ;
              equal (list string)
                ["j 0 0 Lost Not_started"; "j 1 0 Lost Not_started"]
                (lines errors) ;
              equal string "Stopped w0 d0 r0 ok0 re0 f0 x0 l2" (stats q) ;
              equal string "Stopped" (show_result (J.enqueue q j ())) ;
              J.drain q ;
              Fiber.scope (fun sw -> invalid (fun () -> J.start q sw)) ) )
    ; test "start on a closed scope ends the queue, then raises" (fun () ->
          S.run (fun _ ->
              let l = log () in
              let q = queue l in
              let j = J.job ~name:"j" (fun () -> add l "ran") in
              ok (J.enqueue q j ()) ;
              let closed = Fiber.scope Fun.id in
              raises Fiber.Scope_closed (fun () -> J.start q closed) ;
              equal (list string) ["j 0 0 Lost Not_started"] (lines l) ;
              equal string "Stopped w0 d0 r0 ok0 re0 f0 x0 l1" (stats q) ;
              J.stop q ) )
    ; test "start outside fiber code raises, and the queue stays created"
        (fun () ->
          let q = queue (log ()) in
          let sw = S.run (fun _ -> Fiber.scope Fun.id) in
          ( match J.start q sw with
          | () ->
              fail "start ended"
          | exception Fiber.Not_in_fiber _ ->
              () ) ;
          equal string "Created w0 d0 r0 ok0 re0 f0 x0 l0" (stats q) )
    ; test "a second start raises" (fun () ->
          S.run (fun _ ->
              let q = queue (log ()) in
              with_queue q (fun sw -> invalid (fun () -> J.start q sw)) ) )
    ; test "drain and stop in a job of the queue raise" (fun () ->
          S.run (fun _ ->
              let l = log () in
              let q = queue l in
              let j =
                J.job ~name:"self" (fun () ->
                    invalid (fun () -> J.drain q) ;
                    invalid (fun () -> J.stop q) ;
                    add l "checked" )
              in
              with_queue q (fun _ ->
                  ok (J.enqueue q j ()) ;
                  J.drain q ) ;
              equal (list string) ["checked"] (lines l) ) )
    ; test "drain with no job returns at once" (fun () ->
          S.manual (fun _ ->
              let q = queue (log ()) in
              J.drain q ;
              Fiber.scope (fun sw -> J.start q sw ; J.drain q ; J.stop q) ) )
    ; test "a job that enqueues a job: drain waits for the two" (fun () ->
          S.run (fun _ ->
              let l = log () in
              let q = queue l in
              let child = J.job ~name:"child" (fun () -> add l "child") in
              let parent =
                J.job ~name:"parent" (fun () ->
                    ok (J.enqueue q ~delay:(Span.s 1) child ()) ;
                    add l "parent" )
              in
              with_queue q (fun _ ->
                  ok (J.enqueue q parent ()) ;
                  J.drain q ;
                  equal (list string) ["parent"; "child"] (lines l) ) ) ) ]

let hook_tests =
  group "Hooks that raise"
    [ test "stop before start: each job is reported, then stop raises" (fun () ->
          S.run (fun _ ->
              let l = log () in
              let q =
                J.create
                  ~on_error:(fun f ->
                    add l (show f) ;
                    raise Exit )
                  ()
              in
              let j = J.job ~name:"j" ignore in
              for _ = 1 to 3 do
                ok (J.enqueue q j ())
              done ;
              raises Exit (fun () -> J.stop q) ;
              equal int 3 (List.length (lines l)) ) )
    ; test "the end of the queue: each job is reported, then the scope fails"
        (fun () ->
          S.run (fun _ ->
              let l = log () in
              let q =
                J.create
                  ~on_error:(fun f ->
                    add l (show f) ;
                    raise Exit )
                  ()
              in
              let j = J.job ~name:"j" ignore in
              raises Exit (fun () ->
                  Fiber.scope (fun sw ->
                      J.start q sw ;
                      for _ = 1 to 3 do
                        ok (J.enqueue q ~delay:(Span.s 1) j ())
                      done ;
                      J.stop q ) ) ;
              equal int 3 (List.length (lines l)) ) )
    ; test "on_failure raises: on_error runs, then the scope fails" (fun () ->
          S.run (fun _ ->
              let l = log () in
              let q = queue l in
              let j =
                J.job ~name:"j" ~retry:J.no_retry
                  ~on_failure:(fun () _ -> raise Exit)
                  (fun () -> raise Boom)
              in
              raises Exit (fun () ->
                  Fiber.scope (fun sw ->
                      J.start q sw ;
                      ok (J.enqueue q j ()) ;
                      J.drain q ) ) ;
              equal (list string) ["j 0 1 Failed Boom"] (lines l) ;
              equal string "Stopped w0 d0 r0 ok0 re0 f1 x0 l0" (stats q) ) )
    ; test "the default on_error does not raise" (fun () ->
          S.run (fun _ ->
              let q = J.create () in
              let j =
                J.job ~name:"j" ~retry:J.no_retry (fun () -> raise Boom)
              in
              with_queue q (fun _ ->
                  ok (J.enqueue q j ()) ;
                  J.drain q ) ;
              equal string "Stopped w0 d0 r0 ok0 re0 f1 x0 l0" (stats q) ) ) ]

let argument_tests =
  group "Arguments"
    [ test "create" (fun () ->
          invalid (fun () -> J.create ~workers:0 ()) ;
          invalid (fun () -> J.create ~capacity:0 ()) ;
          ignore (J.create ~workers:1 ~capacity:1 ()) )
    ; test "retry" (fun () ->
          invalid (fun () -> J.retry ~attempts:0 ()) ;
          ignore (J.retry ~attempts:1 ()) ;
          invalid (fun () -> J.retry ~base:Span.zero ()) ;
          invalid (fun () -> J.retry ~base:(Span.ns (-1)) ()) ;
          ignore (J.retry ~base:(Span.ns 1) ()) ;
          invalid (fun () -> J.retry ~base:(Span.s 2) ~max:(Span.s 1) ()) ;
          ignore (J.retry ~base:(Span.s 2) ~max:(Span.s 2) ()) )
    ; test "enqueue: delay" (fun () ->
          S.run (fun _ ->
              let q = queue (log ()) in
              let j = J.job ~name:"j" ignore in
              Time.sleep Mono (Span.s 1) ;
              invalid (fun () -> J.enqueue q ~delay:(Span.ns (-1)) j ()) ;
              invalid (fun () ->
                  J.enqueue q ~delay:(Span.of_ns Int64.max_int) j () ) ;
              equal string "Created w0 d0 r0 ok0 re0 f0 x0 l0" (stats q) ) ) ]

(* Each job that [enqueue] accepted runs or is reported, also when the stop
   comes while 4 domains enqueue. *)
let race_tests =
  group "Races"
    [ test "enqueue on 4 domains during the stop: no job is lost silently"
        (fun () ->
          S.race (fun _ ->
              let ran = Atomic.make 0
              and lost = Atomic.make 0
              and accepted = Atomic.make 0 in
              let q =
                J.create ~workers:2 ~on_error:(fun _ -> Atomic.incr lost) ()
              in
              let j =
                J.job ~name:"n" ~retry:J.no_retry (fun () ->
                    Fiber.yield () ; Atomic.incr ran )
              in
              Fiber.scope (fun sw ->
                  J.start q sw ;
                  for d = 1 to 4 do
                    Fiber.fork sw (fun () ->
                        for i = 1 to 20 do
                          let delay =
                            if (i + d) mod 3 = 0 then Some (Span.us i) else None
                          in
                          ( match J.enqueue q ?delay j () with
                          | Ok () ->
                              Atomic.incr accepted
                          | Error _ ->
                              () ) ;
                          Fiber.yield ()
                        done )
                  done ;
                  Fiber.yield () ;
                  J.stop ~grace:(Span.us 5) q ) ;
              equal int (Atomic.get accepted) (Atomic.get ran + Atomic.get lost) ;
              let s = J.stats q in
              equal int (Atomic.get ran) s.succeeded ;
              equal int (Atomic.get lost) s.lost ) ) ]

let server_tests =
  let open Server_support in
  group "Server.run"
    [ test "a handler enqueues in its grace: the job runs, then the queue stops"
        (fun () ->
          let l = log () in
          let q = queue l in
          let j = J.job ~name:"mail" (fun to_ -> add l ("sent to " ^ to_)) in
          let started = Ivar.create () in
          let handler req =
            Ivar.fill started () ;
            sync
              (Caravan.Conn_info.stopping
                 (Option.get (Caravan.Request.conn req)) ) ;
            ok (J.enqueue q j "ann") ;
            Caravan.Response.string "queued"
          in
          Caravan.Server.run
            ~config:{config with shutdown_grace= Span.s 10}
            ~engine:Fake_engine.serve
            ~services:[J.service q]
            ~on_start:(fun server ->
              let flow, r = connect server in
              Caravan.Flow.write_string flow (get "/") ;
              sync (Ivar.read started) ;
              Weft_sim.signal (Option.get !os_sim) Signal.SIGTERM ;
              equal string {|200 "queued" close|} (read_response r) ;
              Caravan.Flow.close flow )
            (module Sim_os)
            handler ;
          equal (list string) ["sent to ann"] (lines l) ;
          equal string "Stopped w0 d0 r0 ok1 re0 f0 x0 l0" (stats q) )
    ; test "two queues share one grace" (fun () ->
          let l = log () in
          let t0 = ref None in
          (* Each report has its time after the SIGTERM. *)
          let timed () =
            J.create
              ~on_error:(fun f ->
                add l
                  (Printf.sprintf "%s at %Ld ns" (show f)
                     (Span.to_ns (since (Option.get !t0))) ) )
              ()
          in
          let started = Ivar.create () in
          let long =
            J.job ~name:"long" (fun () ->
                ignore (Ivar.try_fill started ()) ;
                Time.sleep Mono (Span.s 100) )
          in
          let q1 = timed () and q2 = timed () in
          Caravan.Server.run
            ~config:{config with shutdown_grace= Span.s 10}
            ~engine:Fake_engine.serve
            ~services:[J.service q1; J.service q2]
            ~on_start:(fun _ ->
              ok (J.enqueue q1 long ()) ;
              ok (J.enqueue q2 long ()) ;
              sync (Ivar.read started) ;
              Fiber.yield () ;
              t0 := Some (Time.now Mono) ;
              Weft_sim.signal (Option.get !os_sim) Signal.SIGTERM )
            (module Sim_auto_os)
            (fun _ -> Caravan.Response.string "x") ;
          (* q2 stops first and takes the 10 s. q1 gets no grace. *)
          equal (list string)
            [ "long 0 1 Lost Running at 10000000000 ns"
            ; "long 0 1 Lost Running at 10000000000 ns" ]
            (lines l) )
    ; test "the second signal in the grace of a queue: run returns" (fun () ->
          let l = log () in
          let started = Ivar.create () and term = Ivar.create () in
          let long =
            J.job ~name:"long" (fun () ->
                Ivar.fill started () ;
                Time.sleep Mono (Span.s 100) )
          in
          let q = queue l in
          (* A service that sends SIGINT 1 s after SIGTERM, and whose stop waits
             for ever. *)
          let second =
            Caravan.Server.service
              ~start:(fun sw ->
                Fiber.fork sw (fun () ->
                    sync (Ivar.read term) ;
                    Time.sleep Mono (Span.s 1) ;
                    Weft_sim.signal (Option.get !os_sim) Signal.SIGINT ) )
              ~stop:(fun ~grace:_ -> sync never)
          in
          Caravan.Server.run
            ~config:{config with shutdown_grace= Span.s 10}
            ~engine:Fake_engine.serve
            ~services:[second; J.service q]
            ~on_start:(fun _ ->
              ok (J.enqueue q long ()) ;
              sync (Ivar.read started) ;
              Weft_sim.signal (Option.get !os_sim) Signal.SIGTERM ;
              Ivar.fill term () )
            (module Sim_auto_os)
            (fun _ -> Caravan.Response.string "x") ;
          equal (list string) ["long 0 1 Lost Running"] (lines l) ;
          equal string "Stopped w0 d0 r0 ok0 re0 f0 x0 l1" (stats q) ) ]

let () =
  exit
    (Windtrap.run "jobs"
       [ fifo_tests
       ; delay_tests
       ; retry_tests
       ; stop_tests
       ; life_tests
       ; hook_tests
       ; argument_tests
       ; race_tests
       ; server_tests ] )
