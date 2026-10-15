(* The tests of the harness of the simulator (Sim_support). Each checker of the
   harness gets a test that fails on a bug of the checker. The probes of the
   reviews of the harness are tests here. *)

open Windtrap
open Weft
open Sim_support
module Sim = Weft_sim

let contains ~sub s =
  let n = String.length sub and m = String.length s in
  let rec at i = i + n <= m && (String.sub s i n = sub || at (i + 1)) in
  at 0

(* [failed_with sub] matches [Failed m] if [m] contains [sub]. *)
let failed_with sub = function Failed m -> contains ~sub m | _ -> false

(* The message of the [Failed] of [f]. *)
let failure f =
  match f () with
  | _ ->
      fail "the function did not raise Failed"
  | exception Failed m ->
      m

(* The domain that runs the fiber. This is a private API of weft. *)
let dom () = Private.Sched.prog "test" Private.Sched.Dom_index

let ns = Span.ns

let d = Span.s 1

let d_minus_1ns = Span.ns 999_999_999

let d_plus_1ns = Span.ns 1_000_000_001

(* [race] with no value for CARAVAN_SEED: a replay of a seed of another suite
   does not change the result of these tests. *)
let race ?seeds ?domains f = race_with ~replay:None ?seeds ?domains f

let is_deadlock = function Sim.Deadlock _ -> true | _ -> false

(* The reason of a cancel at the end of a test. *)
exception Test_done

let run_tests =
  group "run"
    [ test "run gives the value of the body" (fun () ->
          equal int 42 (run (fun _ -> 42)) )
    ; test "a body past the guard fails" (fun () ->
          raises_match (failed_with "2 s") (fun () ->
              run (fun _ -> Time.sleep Mono (Span.s 3)) ) )
    ; test "a body that ends at 2 s - 1 ns passes" (fun () ->
          equal int 7
            (run (fun _ ->
                 Time.sleep Mono (Span.ns 1_999_999_999) ;
                 7 ) ) )
    ; test "a Timeout of the code is not the guard" (fun () ->
          raises Fiber.Timeout (fun () ->
              run (fun _ ->
                  Fiber.with_timeout Mono (Span.ms 1) (fun () ->
                      Time.sleep Mono (Span.s 1) ) ) ) )
    ; test "the guard message names the waiting fibers" (fun () ->
          let m =
            failure (fun () ->
                run (fun _ ->
                    let never_filled = Ivar.create () in
                    Fiber.scope (fun s ->
                        Fiber.fork ~name:"stuck" s (fun () ->
                            sync (Ivar.read never_filled) ) ) ) )
          in
          is_true ~msg:m (contains ~sub:"stuck" m) )
    ; test "run has 1 domain and the seed 0 by default" (fun () ->
          let doms =
            run (fun _ ->
                Fiber.scope (fun s ->
                    let ps =
                      List.init 8 (fun _ ->
                          Fiber.fork_promise s (fun () ->
                              Fiber.yield () ; dom () ) )
                    in
                    List.map Promise.await ps ) )
          in
          equal (list int) (List.init 8 (fun _ -> 0)) doms ;
          (* 8 random choices: two seeds give the same list only by chance. *)
          let choice _ =
            List.init 8 (fun _ -> sync (choose (List.init 8 always)))
          in
          let pick_seed seed = run ~seed choice in
          equal (list int) (pick_seed 0) (run choice) ;
          is_true ~msg:"another seed gives another choice"
            (List.exists
               (fun seed -> pick_seed seed <> pick_seed 0)
               [1; 2; 3; 4; 5] ) )
    ; test "a body that ends at the guard fails for each seed" (fun () ->
          (* On 4 domains, the seed sets the order of the body and the guard at
             the same time. *)
          List.iter
            (fun domains ->
              for seed = 0 to 19 do
                let msg = Printf.sprintf "seed %d, %d domains" seed domains in
                match
                  run ~seed ~domains (fun _ -> Time.sleep Mono guard ; 1)
                with
                | _ ->
                    fail msg
                | exception Failed m ->
                    is_true ~msg (contains ~sub:"2 s" m)
              done )
            [1; 4] )
    ; test "a fiber that does not end prevents the end of the body" (fun () ->
          let body _ =
            Fiber.scope (fun s ->
                Fiber.fork ~name:"server_loop" s (fun () -> sync never) ;
                1 )
          in
          raises_match (failed_with "server_loop") (fun () -> run body) ;
          raises_match is_deadlock (fun () -> manual body) )
    ; test "a daemon or a cancel of the scope ends a fiber of the test"
        (fun () ->
          equal int 1
            (run (fun _ ->
                 Fiber.scope (fun s ->
                     Fiber.fork_daemon s (fun () -> sync never) ;
                     1 ) ) ) ;
          equal int 2
            (run (fun _ ->
                 Fiber.scope (fun s ->
                     Fiber.fork s (fun () -> sync never) ;
                     Fiber.cancel s Test_done ;
                     2 ) ) ) )
    ; test "a shielded wait after the guard gives Deadlock" (fun () ->
          raises_match is_deadlock (fun () ->
              run (fun _ ->
                  Fiber.shield (fun () -> sync (Ivar.read (Ivar.create ()))) ) ) )
    ]

let race_tests =
  group "race"
    [ test "race runs 100 seeds on 4 domains" (fun () ->
          let runs = ref 0 in
          let seen = Array.init 64 (fun _ -> Atomic.make false) in
          race (fun _ ->
              incr runs ;
              Fiber.scope (fun s ->
                  for _ = 1 to 8 do
                    Fiber.fork s (fun () ->
                        Fiber.yield () ;
                        Atomic.set seen.(dom ()) true )
                  done ) ) ;
          equal int 100 !runs ;
          let indices =
            List.filter (fun i -> Atomic.get seen.(i)) (List.init 64 Fun.id)
          in
          equal (list int) [0; 1; 2; 3] indices )
    ; test "a bug of some seeds fails and names the seed" (fun () ->
          let m =
            failure (fun () ->
                race (fun _ ->
                    if sync (choose [always `A; always `B]) <> `A then
                      failwith "choose gave `B" ) )
          in
          is_true ~msg:m (String.starts_with ~prefix:"seed " m) ;
          is_true ~msg:m (contains ~sub:"choose gave `B" m) )
    ; test "a simulator exception gets the seed" (fun () ->
          (* The guard turns a plain deadlock into a guard failure. A wait in a
             shield ignores the cancel of the guard, and then no timer is
             active. Thus the simulator raises Deadlock. *)
          let runs = ref 0 in
          let m =
            failure (fun () ->
                race (fun _ ->
                    incr runs ;
                    if !runs = 5 then
                      Fiber.shield (fun () -> sync (Ivar.read (Ivar.create ()))) ) )
          in
          is_true ~msg:m (String.starts_with ~prefix:"seed 5," m) ;
          is_true ~msg:m (contains ~sub:"Deadlock" m) )
    ; test "a replay runs only the seed of CARAVAN_SEED" (fun () ->
          let runs = ref 0 in
          let m =
            failure (fun () ->
                race_with ~replay:(Some "7") (fun _ ->
                    incr runs ; failwith "bug" ) )
          in
          equal int 1 !runs ;
          is_true ~msg:m (String.starts_with ~prefix:"seed 7," m) ;
          raises_match (failed_with "not an integer") (fun () ->
              race_with ~replay:(Some "x") (fun _ -> ()) ) )
    ; test "race reads CARAVAN_SEED" (fun () ->
          (* The process has no unsetenv. An empty value is the same as no value
             for [race]. *)
          let old = Option.value (Sys.getenv_opt "CARAVAN_SEED") ~default:"" in
          let runs = ref 0 in
          Fun.protect
            ~finally:(fun () -> Unix.putenv "CARAVAN_SEED" old)
            (fun () ->
              Unix.putenv "CARAVAN_SEED" "7" ;
              Sim_support.race (fun _ -> incr runs) ) ;
          equal int 1 !runs ) ]

let call_tests =
  group "call"
    [ test "ended settles first" (fun () ->
          is_true
            (run (fun sim ->
                 Fiber.scope (fun s ->
                     let c =
                       call s (fun () ->
                           Fiber.yield () ; Fiber.yield () ; Fiber.yield () )
                     in
                     ended sim c ) ) ) )
    ; test "ended is false for a call that waits" (fun () ->
          let before, after =
            run (fun sim ->
                Fiber.scope (fun s ->
                    let iv = Ivar.create () in
                    let c = call s (fun () -> sync (Ivar.read iv)) in
                    let before = ended sim c in
                    Ivar.fill iv () ;
                    (before, ended sim c) ) )
          in
          is_false ~msg:"before the fill" before ;
          is_true ~msg:"after the fill" after )
    ; test "await raises the exception of the call" (fun () ->
          raises Exit (fun () ->
              run (fun _ ->
                  Fiber.scope (fun s -> await (call s (fun () -> raise Exit))) ) ) )
    ; test "a cancel after the failure of a call does not prevent Unawaited"
        (fun () ->
          raises_match
            (function Sim.Unawaited _ -> true | _ -> false)
            (fun () ->
              run (fun sim ->
                  Fiber.scope (fun s ->
                      let c = call s (fun () -> raise Exit) in
                      is_true ~msg:"the call ended" (ended sim c) ;
                      cancel c ) ) ) )
    ; test "a cancel before the failure of a call prevents Unawaited" (fun () ->
          is_true
            (run (fun sim ->
                 Fiber.scope (fun s ->
                     let c =
                       call s (fun () -> Time.sleep Mono d ; raise Exit)
                     in
                     cancel c ; ended sim c ) ) ) ) ]

(* [ends_at_after span] runs [ends_at ~t0 d] on a call that sleeps [span]. *)
let ends_at_after ?(body = fun () -> 42) span =
  manual (fun sim ->
      Fiber.scope (fun s ->
          let t0 = Time.now Mono in
          let c = call s (fun () -> Time.sleep Mono span ; body ()) in
          ends_at sim ~t0 d c ) )

(* [holds_after span] runs [holds_from ~t0 d] on a variable that a fiber sets
   after [span]. *)
let holds_after span =
  manual (fun sim ->
      Fiber.scope (fun s ->
          let t0 = Time.now Mono in
          let v = Var.create false in
          Fiber.fork s (fun () -> Time.sleep Mono span ; Var.set v true) ;
          holds_from sim ~t0 d ~what:"the flag" (fun () -> Var.get v) ) )

let exact_time_tests =
  group "exact time"
    [ test "ends_at passes at t0 + d" (fun () -> equal int 42 (ends_at_after d))
    ; test "ends_at fails 1 ns early" (fun () ->
          raises_match (failed_with "ended already at t0 + d - 1 ns") (fun () ->
              ends_at_after d_minus_1ns ) )
    ; test "ends_at fails 1 ns late" (fun () ->
          raises_match (failed_with "the call did not end at t0 + d") (fun () ->
              ends_at_after d_plus_1ns ) )
    ; test "ends_at fails for a call that ends at once" (fun () ->
          raises_match (failed_with "the call ended at the start") (fun () ->
              ends_at_after Span.zero ) )
    ; test "ends_at refuses a late start" (fun () ->
          raises_match (failed_with "after t0 + d - 1 ns") (fun () ->
              manual (fun sim ->
                  Fiber.scope (fun s ->
                      let t0 = Time.now Mono in
                      advance sim d ;
                      let c = call s (fun () -> Time.sleep Mono d) in
                      ends_at sim ~t0 d c ) ) ) )
    ; test "ends_at passes when the time now is t0 + d - 1 ns" (fun () ->
          equal int 4
            (manual (fun sim ->
                 Fiber.scope (fun s ->
                     let t0 = Time.now Mono in
                     let c = call s (fun () -> Time.sleep Mono d ; 4) in
                     advance sim d_minus_1ns ; ends_at sim ~t0 d c ) ) ) )
    ; test "ends_at raises the exception of the call" (fun () ->
          raises Exit (fun () -> ends_at_after ~body:(fun () -> raise Exit) d) )
    ; test "holds_from: true at t0 + d, not 1 ns early, not 1 ns late"
        (fun () ->
          holds_after d ;
          raises_match (failed_with "the flag is true already at t0 + d - 1 ns")
            (fun () -> holds_after d_minus_1ns ) ;
          raises_match (failed_with "the flag is not true at") (fun () ->
              holds_after d_plus_1ns ) )
    ; test "holds_from fails for a probe that is true at the start" (fun () ->
          raises_match (failed_with "the flag is true at the start") (fun () ->
              manual (fun sim ->
                  let t0 = Time.now Mono in
                  holds_from sim ~t0 d ~what:"the flag" (fun () -> true) ) ) )
    ; test "+: fails on overflow" (fun () ->
          raises_match (failed_with "out of the range") (fun () ->
              Clock.of_ns Mono Int64.max_int +: ns 1 ) )
    ; test "advance_to refuses a target in the past" (fun () ->
          raises_match (failed_with "1 ns after the target") (fun () ->
              manual (fun sim ->
                  advance sim d ;
                  advance_to sim (Time.now Mono +: ns (-1)) ) ) )
    ; test "a wake at the target runs before advance_to returns" (fun () ->
          is_true
            (manual (fun sim ->
                 Fiber.scope (fun s ->
                     let target = Time.now Mono +: d in
                     let flag = Atomic.make false in
                     Fiber.fork s (fun () ->
                         sync (Time.at Mono target) ;
                         Fiber.yield () ;
                         Fiber.yield () ;
                         Fiber.yield () ;
                         Atomic.set flag true ) ;
                     advance_to sim target ;
                     Atomic.get flag ) ) ) )
    ; test "manual: a wait with no advance is a deadlock" (fun () ->
          raises_match is_deadlock (fun () ->
              manual (fun _ -> Time.sleep Mono (Span.s 1)) ) ) ]

let fd_tests =
  group "pairs and fds"
    [ test "with_pair gives a connected TCP pair" (fun () ->
          let first, second, client_addr =
            run (fun sim ->
                with_pair sim (fun client server ->
                    Net.write client "x" ;
                    Net.shutdown client ;
                    let buf = Bytes.create 8 in
                    let n = Net.read server buf 0 8 in
                    let first = Bytes.sub_string buf 0 n in
                    (first, Net.read server buf 0 8, Net.local_addr client) ) )
          in
          equal string "x" first ;
          equal int ~msg:"the end of the stream" 0 second ;
          is_true ~msg:"the first side is the client"
            (match client_addr with Net.Tcp ("client", _) -> true | _ -> false) )
    ; test "with_pair closes the two sides on an exception" (fun () ->
          let before, after =
            run (fun sim ->
                let before = Sim.open_fds sim in
                (try with_pair sim (fun _ _ -> raise Exit) with Exit -> ()) ;
                Sim.settle sim ;
                (before, Sim.open_fds sim) )
          in
          equal int before after )
    ; test "no_fd_leak fails for a leak" (fun () ->
          let m =
            run (fun sim ->
                let listener = ref None in
                match
                  no_fd_leak sim (fun () ->
                      listener := Some (Net.listen (Sim.addr sim "server:80")) )
                with
                | () ->
                    "no failure"
                | exception Failed m ->
                    Option.iter Net.close !listener ;
                    m )
          in
          is_true ~msg:m (contains ~sub:"0 fds open before the body, 1 after" m) ;
          equal int ~msg:"no failure with no leak" 5
            (run (fun sim ->
                 no_fd_leak sim (fun () -> with_pair sim (fun _ _ -> 5)) ) ) )
    ; test "no_fd_leak settles before it counts" (fun () ->
          (* The body makes another fiber open a listener, then returns. *)
          let m =
            run (fun sim ->
                Fiber.scope (fun s ->
                    let go = Ivar.create () in
                    let listener = ref None in
                    Fiber.fork s (fun () ->
                        sync (Ivar.read go) ;
                        listener := Some (Net.listen (Sim.addr sim "server:80")) ) ;
                    Sim.settle sim ;
                    let m =
                      match no_fd_leak sim (fun () -> Ivar.fill go ()) with
                      | () ->
                          "no failure"
                      | exception Failed m ->
                          m
                    in
                    Sim.settle sim ;
                    Option.iter Net.close !listener ;
                    m ) )
          in
          is_true ~msg:m (contains ~sub:"0 fds open before the body, 1 after" m) )
    ]

let () =
  exit
    (Windtrap.run "sim_support"
       [run_tests; race_tests; call_tests; exact_time_tests; fd_tests] )
