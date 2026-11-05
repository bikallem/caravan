(* The tests of Caravan_jobs.Recurring, in the simulator: its POSIX clock starts
   at a known time, and a test can step it. *)

open Windtrap
open Weft
module J = Caravan_jobs
module D = Caravan_jobs.Durable
module R = Caravan_jobs.Recurring
module Sc = Caravan_jobs.Schedule
module S = Sim_support

let at text =
  Scanf.sscanf text "%d-%d-%d %d:%d:%d"
    (fun year month day hour minute second ->
      Option.get
        (Time.of_utc {year; month; day; hour; minute; second; weekday= 0}) )

let show p =
  let u = Time.to_utc p in
  Printf.sprintf "%02d:%02d:%02d" u.hour u.minute u.second

let log () = ref []

let add l s = l := s :: !l

let lines l = List.rev !l

let long = Span.s 100_000

let minute = Sc.every (Span.s 60)

(* [in_sim f]: a run whose POSIX clock is 2024-03-01 12:00:30 at the start. *)
let in_sim ?(guard = long) f =
  S.run ~guard (fun sim ->
      Weft_sim.set_posix sim (at "2024-03-01 12:00:30") ;
      f sim )

let started r f =
  Fiber.scope (fun sw ->
      R.start r sw ;
      let v = f sw in
      R.stop r ; v )

let text name f = D.job ~name ~encode:Fun.id ~decode:Fun.id f

let entry_tests =
  group "Entries"
    [ test "an entry runs at each tick, with the tick" (fun () ->
          in_sim (fun _ ->
              let l = log () in
              let r =
                R.create [R.entry ~name:"e" minute (fun t -> add l (show t))]
              in
              started r (fun _ -> Time.sleep Mono (Span.s 180)) ;
              equal (list string) ["12:01:00"; "12:02:00"; "12:03:00"] (lines l) ) )
    ; test "job: a job of the J1 queue at each tick" (fun () ->
          in_sim (fun _ ->
              let l = log () in
              let q = J.create () in
              let j = J.job ~name:"j" (fun s -> add l s) in
              Fiber.scope (fun sw ->
                  J.start q sw ;
                  let r = R.create [R.job ~name:"r" minute q j show] in
                  started r (fun _ -> Time.sleep Mono (Span.s 120)) ;
                  J.drain q ;
                  J.stop q ) ;
              equal (list string) ["12:01:00"; "12:02:00"] (lines l) ) )
    ; test "job: a refusal of the queue goes to on_error" (fun () ->
          in_sim (fun _ ->
              let l = log () in
              let q = J.create () in
              J.stop q ;
              let j = J.job ~name:"j" ignore in
              let r =
                R.create
                  ~on_error:(fun name e _ ->
                    add l
                      ( name ^ " "
                      ^
                      match e with
                      | J.Not_enqueued `Stopped ->
                          "Stopped"
                      | J.Not_enqueued `Full ->
                          "Full"
                      | e ->
                          Printexc.to_string e ) )
                  [R.job ~name:"r" minute q j show]
              in
              started r (fun _ -> Time.sleep Mono (Span.s 60)) ;
              equal (list string) ["r Stopped"] (lines l) ) )
    ; test "an action that raises: on_error, then the next tick" (fun () ->
          in_sim (fun _ ->
              let l = log () in
              let r =
                R.create
                  ~on_error:(fun name e _ ->
                    add l (name ^ " " ^ Printexc.to_string e) )
                  [ R.entry ~name:"e" minute (fun t ->
                        add l (show t) ;
                        if show t = "12:01:00" then raise Exit ) ]
              in
              started r (fun _ -> Time.sleep Mono (Span.s 120)) ;
              equal (list string)
                ["12:01:00"; "e Stdlib.Exit"; "12:02:00"]
                (lines l) ) )
    ; test "stop in an action: Invalid_argument" (fun () ->
          in_sim (fun _ ->
              let l = log () in
              let self = ref None in
              let r =
                R.create
                  ~on_error:(fun _ e _ ->
                    add l
                      ( match e with
                      | Invalid_argument m ->
                          m
                      | e ->
                          Printexc.to_string e ) )
                  [ R.entry ~name:"e" minute (fun _ ->
                        R.stop (Option.get !self) ) ]
              in
              self := Some r ;
              started r (fun _ -> Time.sleep Mono (Span.s 60)) ;
              equal (list string)
                ["Caravan_jobs.Recurring.stop: called in an entry"]
                (lines l) ) )
    ; test "on_error raises: the service fails" (fun () ->
          in_sim (fun _ ->
              let r =
                R.create
                  ~on_error:(fun _ _ _ -> failwith "on_error")
                  [R.entry ~name:"e" minute (fun _ -> raise Exit)]
              in
              raises (Failure "on_error") (fun () ->
                  Fiber.scope (fun sw ->
                      R.start r sw ;
                      Time.sleep Mono (Span.s 60) ) ) ) )
    ; test "the default on_error does not raise" (fun () ->
          in_sim (fun _ ->
              let n = ref 0 in
              let r =
                R.create
                  [R.entry ~name:"e" minute (fun _ -> incr n ; raise Exit)]
              in
              started r (fun _ -> Time.sleep Mono (Span.s 120)) ;
              equal int 2 !n ) ) ]

let tick_tests =
  group "Ticks"
    [ test "a step of the clock over many ticks: one run, no catch-up" (fun () ->
          in_sim (fun sim ->
              let l = log () in
              let r =
                R.create [R.entry ~name:"e" minute (fun t -> add l (show t))]
              in
              started r (fun _ ->
                  Time.sleep Mono (Span.s 40) ;
                  (* 12:01:10: one run of 12:01. Then a step to 12:10:40: the
                     wait for 12:02 ends, and 12:02 runs one time. Then 12:11,
                     with no run of 12:03 to 12:10. *)
                  Weft_sim.set_posix sim (at "2024-03-01 12:10:40") ;
                  Time.sleep Mono (Span.s 30) ) ;
              equal (list string) ["12:01:00"; "12:02:00"; "12:11:00"] (lines l) ) )
    ; test "a step forward during the wait: the tick runs at once, one time"
        (fun () ->
          in_sim (fun sim ->
              let l = log () in
              let r =
                R.create [R.entry ~name:"e" minute (fun t -> add l (show t))]
              in
              started r (fun _ ->
                  Time.sleep Mono (Span.s 10) ;
                  Weft_sim.set_posix sim (at "2024-03-01 12:05:30") ;
                  Time.sleep Mono (Span.s 1) ) ;
              equal (list string) ["12:01:00"] (lines l) ) )
    ; test "a step back during the wait: no early run" (fun () ->
          in_sim (fun sim ->
              let l = log () in
              let r =
                R.create [R.entry ~name:"e" minute (fun t -> add l (show t))]
              in
              started r (fun _ ->
                  Time.sleep Mono (Span.s 10) ;
                  Weft_sim.set_posix sim (at "2024-03-01 11:50:00") ;
                  Time.sleep Mono (Span.s 600) ) ;
              equal (list string) [] (lines l) ) )
    ; test "a slow action: the ticks that passed are skipped" (fun () ->
          in_sim (fun _ ->
              let l = log () in
              let r =
                R.create
                  [ R.entry ~name:"e" minute (fun t ->
                        add l (show t) ;
                        Time.sleep Mono (Span.s 150) ) ]
              in
              started r (fun _ -> Time.sleep Mono (Span.s 400)) ;
              (* 12:01 ends at 12:03:30, 12:04 at 12:06:30, then 12:07. *)
              equal (list string) ["12:01:00"; "12:04:00"; "12:07:00"] (lines l) ) )
    ; test "a schedule with no next tick: the entry ends" (fun () ->
          in_sim (fun sim ->
              let l = log () in
              Weft_sim.set_posix sim
                (Option.get
                   (Time.add Posix (Time.posix_of_unix_s 0.)
                      (Span.of_ns (Int64.sub Int64.max_int 1L)) ) ) ;
              let r =
                R.create [R.entry ~name:"e" minute (fun t -> add l (show t))]
              in
              started r (fun _ -> Time.sleep Mono (Span.s 120)) ;
              equal (list string) [] (lines l) ) ) ]

let durable_tests =
  group "Durable"
    [ test "two processes: one job for each tick" (fun () ->
          in_sim (fun _ ->
              let l = log () in
              let st = D.memory_store () in
              let j = text "tick" (fun _ s -> add l s) in
              let qa = D.create ~jobs:[Job j] st
              and qb = D.create ~jobs:[Job j] st in
              let ra = R.create [R.durable ~name:"r" minute qa j show]
              and rb = R.create [R.durable ~name:"r" minute qb j show] in
              Fiber.scope (fun sw ->
                  D.start qa sw ;
                  D.start qb sw ;
                  R.start ra sw ;
                  R.start rb sw ;
                  Time.sleep Mono (Span.s 150) ;
                  R.stop ra ;
                  R.stop rb ;
                  D.drain qa ;
                  D.stop qa ;
                  D.stop qb ) ;
              equal (list string) ["12:01:00"; "12:02:00"] (lines l) ) )
    ; test "a late process after the job of the tick ended: no second job"
        (fun () ->
          in_sim (fun _ ->
              let l = log () in
              let st = D.memory_store () in
              let j = text "tick" (fun _ s -> add l s) in
              let qa = D.create ~jobs:[Job j] st
              and qb = D.create ~jobs:[Job j] st in
              let gate = Ivar.create () and pushed = ref [] in
              let late =
                R.entry ~name:"late" minute (fun tick ->
                    sync (Ivar.read gate) ;
                    add pushed
                      ( match
                          D.enqueue_tick qb ~entry:"r" ~tick j (show tick)
                        with
                      | None ->
                          "None"
                      | Some _ ->
                          "Some" ) )
              in
              let ra = R.create [R.durable ~name:"r" minute qa j show]
              and rb = R.create [late] in
              Fiber.scope (fun sw ->
                  D.start qa sw ;
                  D.start qb sw ;
                  R.start ra sw ;
                  R.start rb sw ;
                  Time.sleep Mono (Span.s 35) ;
                  D.drain qa ;
                  equal (list string) ["12:01:00"] (lines l) ;
                  Ivar.fill gate () ;
                  Time.sleep Mono (Span.s 1) ;
                  R.stop ra ;
                  R.stop rb ;
                  D.drain qa ;
                  D.stop qa ;
                  D.stop qb ) ;
              equal (list string) ["None"] (lines pushed) ;
              equal (list string) ["12:01:00"] (lines l) ) )
    ; test "durable with a store with no push_tick: Invalid_argument" (fun () ->
          let m = D.memory_store () in
          let st =
            D.store ~push:m.push ~claim:m.claim ~extend:m.extend
              ~complete:m.complete ~retry:m.retry ~release:m.release
              ~fail:m.fail ()
          in
          let j = text "t" (fun _ _ -> ()) in
          let q = D.create ~jobs:[Job j] st in
          match R.durable ~name:"r" minute q j show with
          | _ ->
              fail "no exception"
          | exception Invalid_argument _ ->
              () ) ]

let life_tests =
  group "Life"
    [ test "stop: an action gets the grace, then a cancel, no error" (fun () ->
          S.manual (fun sim ->
              Weft_sim.set_posix sim (at "2024-03-01 12:00:59") ;
              let l = log () in
              let r =
                R.create
                  ~on_error:(fun _ e _ -> add l (Printexc.to_string e))
                  [ R.entry ~name:"e" minute (fun _ ->
                        add l "start" ;
                        Time.sleep Mono (Span.s 100) ;
                        add l "end" ) ]
              in
              Fiber.scope (fun sw ->
                  R.start r sw ;
                  S.advance sim (Span.s 1) ;
                  equal (list string) ["start"] (lines l) ;
                  let t0 = Time.now Mono in
                  let c = S.call sw (fun () -> R.stop ~grace:(Span.s 5) r) in
                  S.ends_at sim ~t0 (Span.s 5) c ) ;
              equal (list string) ["start"] (lines l) ) )
    ; test "stop: the default grace is 10 s" (fun () ->
          S.manual (fun sim ->
              Weft_sim.set_posix sim (at "2024-03-01 12:00:59") ;
              let r =
                R.create
                  [ R.entry ~name:"e" minute (fun _ ->
                        Time.sleep Mono (Span.s 100) ) ]
              in
              Fiber.scope (fun sw ->
                  R.start r sw ;
                  S.advance sim (Span.s 1) ;
                  let t0 = Time.now Mono in
                  let c = S.call sw (fun () -> R.stop r) in
                  S.ends_at sim ~t0 (Span.s 10) c ) ) )
    ; test
        "stop with no action in progress: at once; stop before start; two stops"
        (fun () ->
          S.manual (fun sim ->
              let r = R.create [R.entry ~name:"e" minute ignore] in
              Fiber.scope (fun sw ->
                  R.start r sw ;
                  Weft_sim.settle sim ;
                  let c1 = S.call sw (fun () -> R.stop r) in
                  let c2 = S.call sw (fun () -> R.stop r) in
                  is_true ~msg:"first" (S.ended sim c1) ;
                  is_true ~msg:"second" (S.ended sim c2) ;
                  S.await c1 ;
                  S.await c2 ;
                  match R.start r sw with
                  | () ->
                      fail "start after stop"
                  | exception Invalid_argument _ ->
                      () ) ;
              let r2 = R.create [] in
              R.stop r2 ;
              R.stop r2 ;
              Fiber.scope (fun sw ->
                  match R.start r2 sw with
                  | () ->
                      fail "start after a stop before start"
                  | exception Invalid_argument _ ->
                      () ) ) )
    ; test "a second stop waits for the end of the first" (fun () ->
          S.manual (fun sim ->
              Weft_sim.set_posix sim (at "2024-03-01 12:00:59") ;
              let r =
                R.create
                  [ R.entry ~name:"e" minute (fun _ ->
                        Time.sleep Mono (Span.s 2) ) ]
              in
              Fiber.scope (fun sw ->
                  R.start r sw ;
                  S.advance sim (Span.s 1) ;
                  let t0 = Time.now Mono in
                  let c1 = S.call sw (fun () -> R.stop ~grace:(Span.s 10) r) in
                  let c2 = S.call sw (fun () -> R.stop ~grace:Span.zero r) in
                  S.ends_at sim ~t0 (Span.s 2) c1 ;
                  is_true ~msg:"second" (S.ended sim c2) ;
                  S.await c2 ) ) )
    ; test "a stop with no grace before the fiber runs; start on a closed scope"
        (fun () ->
          S.run (fun _ ->
              let l = log () in
              let r =
                R.create [R.entry ~name:"e" minute (fun _ -> add l "ran")]
              in
              Fiber.scope (fun sw -> R.start r sw ; R.stop ~grace:Span.zero r) ;
              equal (list string) [] (lines l) ;
              let r2 = R.create [] in
              let closed = Fiber.scope Fun.id in
              raises Fiber.Scope_closed (fun () -> R.start r2 closed) ;
              R.stop r2 ) )
    ; test "two entries with one name: Invalid_argument" (fun () ->
          match
            R.create
              [R.entry ~name:"e" minute ignore; R.entry ~name:"e" minute ignore]
          with
          | _ ->
              fail "no exception"
          | exception Invalid_argument _ ->
              () )
    ; test "service: with Server.run, the entries stop with the server"
        (fun () ->
          let open Server_support in
          let l = log () in
          let st = D.memory_store () in
          let j = text "tick" (fun _ s -> add l s) in
          let q = D.create ~jobs:[Job j] st in
          let r = R.create [R.durable ~name:"r" minute q j show] in
          Caravan.Server.run
            ~config:{config with shutdown_grace= Span.s 10}
            ~engine:Fake_engine.serve
            ~services:[D.service q; R.service r]
            ~on_start:(fun _ ->
              Weft_sim.set_posix (Option.get !os_sim) (at "2024-03-01 12:00:30") ;
              Time.sleep Mono (Span.s 100) ;
              Weft_sim.signal (Option.get !os_sim) Signal.SIGTERM )
            (module Sim_auto_os)
            (fun _ -> Caravan.Response.string "x") ;
          equal (list string) ["12:01:00"; "12:02:00"] (lines l) ) ]

let () =
  exit
    (Windtrap.run "recurring"
       [entry_tests; tick_tests; durable_tests; life_tests] )
