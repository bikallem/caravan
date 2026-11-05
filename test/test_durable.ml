(* The tests of Caravan_jobs.Durable. They run in the simulator: its POSIX clock
   moves the due times and the leases of the memory store. A [Fake] store wraps
   the memory store: it records each call, and a test can make a call block,
   raise, or give [false]. *)

open Windtrap
open Weft
module J = Caravan_jobs
module D = Caravan_jobs.Durable
module S = Sim_support

exception Boom

let exn_name = function Boom -> "Boom" | e -> Printexc.to_string e

let show_reason : D.reason -> string = function
  | Failed (e, _) ->
      "Failed " ^ exn_name e
  | Discarded (e, _) ->
      "Discarded " ^ exn_name e
  | Undecodable (e, _) ->
      "Undecodable " ^ exn_name e
  | Abandoned ->
      "Abandoned"

let show (f : D.failure) =
  Printf.sprintf "%s %s %d %s" f.name f.id f.attempts (show_reason f.reason)

let show_state = function
  | `Created ->
      "Created"
  | `Running ->
      "Running"
  | `Stopping ->
      "Stopping"
  | `Stopped ->
      "Stopped"

let stats q =
  let s = D.stats q in
  Printf.sprintf "%s r%d ok%d re%d f%d x%d u%d a%d rel%d lost%d se%d"
    (show_state s.state) s.running s.succeeded s.retried s.failed s.discarded
    s.undecodable s.abandoned s.released s.lease_lost s.store_errors

let log () = ref []

let add l s = l := s :: !l

let lines l = List.rev !l

let long = Span.s 100_000

let since t0 = Time.diff Mono (Time.now Mono) t0

let ms d = Int64.to_int (Int64.div (Span.to_ns d) 1_000_000L)

(* The backoff of J1 in ms, computed in a second way: [base * 2^(n-1)], at most
   [max], less a jitter of [id] and [n]. *)
let backoff_ms ~base ~max ~id n =
  let d = Int.min max (base * (1 lsl (n - 1))) in
  d - (d / 100 * (Hashtbl.hash (id, n) mod 16))

(* The seed of the jitters of a queue that starts now. *)
let seed () = Hashtbl.hash (Time.now Mono)

(* A job type of a string. *)
let text ?retry ?on_failure name f =
  D.job ~name ~encode:Fun.id ~decode:Fun.id ?retry ?on_failure f

(* A fake store around [inner]. [before op] runs before each call of [op] (it
   can raise or wait). [result op] can replace the result of [op]. [calls] has
   each call, in order. *)
module Fake = struct
  type t =
    { mutable before: string -> unit
    ; mutable result: string -> bool option
    ; mutable rename: string -> string
    ; mutable bump: int
    ; mutable extra: int
    ; mutable lease_seen: Span.t option
    ; calls: string list ref }

  let make () =
    { before= ignore
    ; result= (fun _ -> None)
    ; rename= Fun.id
    ; bump= 0
    ; extra= 0
    ; lease_seen= None
    ; calls= ref [] }

  let store f inner =
    let op name id g =
      add f.calls (name ^ " " ^ id) ;
      f.before name ;
      match f.result name with Some b -> b | None -> g ()
    in
    D.store
      ~push:(fun row ->
        f.before "push" ;
        let id = inner.D.push row in
        add f.calls ("push " ^ id) ;
        id )
      ~claim:(fun ~names ~max ~lease ->
        f.before "claim" ;
        f.lease_seen <- Some lease ;
        let cs = inner.claim ~names ~max:(max + f.extra) ~lease in
        add f.calls
          (Printf.sprintf "claim %d -> %s" max
             (String.concat "," (List.map (fun (c : D.claim) -> c.id) cs)) ) ;
        List.map
          (fun (c : D.claim) ->
            {c with name= f.rename c.name; attempts= c.attempts + f.bump} )
          cs )
      ~extend:(fun ~id ~lease span ->
        op "extend" id (fun () -> inner.extend ~id ~lease span) )
      ~complete:(fun ~id ~lease ->
        op "complete" id (fun () -> inner.complete ~id ~lease) )
      ~retry:(fun ~id ~lease ~due ->
        op "retry" id (fun () -> inner.retry ~id ~lease ~due) )
      ~release:(fun ~id ~lease ->
        op "release" id (fun () -> inner.release ~id ~lease) )
      ~fail:(fun ~id ~lease ~error ->
        op "fail" id (fun () -> inner.fail ~id ~lease ~error) )
      ~next_due:(fun ~names -> Option.get inner.next_due ~names)
      ?push_unique:inner.push_unique ?push_tick:inner.push_tick ()
end

(* The memory store, called directly. [at s] is [s] seconds after the start. *)
let with_store f =
  S.manual (fun sim ->
      let st = D.memory_store () in
      let t0 = Time.now Posix in
      let at s = Option.get (Time.add Posix t0 (Span.s s)) in
      let set s = Weft_sim.set_posix sim (at s) in
      f st at set )

let push (st : D.store) ?(name = "a") at due =
  st.push {name; payload= name ^ "!"; due= at due}

let show_claims cs =
  String.concat " "
    (List.map
       (fun (c : D.claim) -> Printf.sprintf "%s:%s:%d" c.id c.payload c.attempts)
       cs )

let claim ?(names = ["a"]) ?(lease = Span.s 10) (st : D.store) max =
  st.claim ~names ~max ~lease

let one cs = match cs with [c] -> c | _ -> fail "not one claim"

let store_tests =
  group "The memory store"
    [ test "claim: due order, then push order; names; max; not due" (fun () ->
          with_store (fun st at _ ->
              let a = push st at 0 in
              let b = push st at (-1) in
              let _c = push st ~name:"b" at 0 in
              let _d = push st at 5 in
              let e = push st at 0 in
              equal string
                (Printf.sprintf "%s:a!:1 %s:a!:1" b a)
                (show_claims (claim st 2)) ;
              equal string
                (Printf.sprintf "%s:a!:1" e)
                (show_claims (claim st 5)) ;
              equal string "" (show_claims (claim st 5)) ) )
    ; test "a lease lives until its end, then a new claim takes the job"
        (fun () ->
          with_store (fun st at set ->
              let a = push st at 0 in
              let c1 = one (claim st 1) in
              set 9 ;
              equal string "" (show_claims (claim st 1)) ;
              set 10 ;
              let c2 = one (claim st 1) in
              equal string (a ^ ":a!:2") (show_claims [c2]) ;
              is_true ~msg:"a new token" (c1.lease <> c2.lease) ;
              (* The old token changes nothing. *)
              is_false ~msg:"stale complete" (st.complete ~id:a ~lease:c1.lease) ;
              is_false ~msg:"stale extend"
                (st.extend ~id:a ~lease:c1.lease (Span.s 100)) ;
              is_false ~msg:"stale retry"
                (st.retry ~id:a ~lease:c1.lease ~due:(at 0)) ;
              is_false ~msg:"stale release" (st.release ~id:a ~lease:c1.lease) ;
              is_false ~msg:"stale fail"
                (st.fail ~id:a ~lease:c1.lease ~error:"x") ;
              is_true ~msg:"complete" (st.complete ~id:a ~lease:c2.lease) ;
              is_false ~msg:"a second complete"
                (st.complete ~id:a ~lease:c2.lease) ;
              set 100 ;
              equal string "" (show_claims (claim st 1)) ) )
    ; test "extend moves the end of the lease" (fun () ->
          with_store (fun st at set ->
              let a = push st at 0 in
              let c = one (claim st 1) in
              set 5 ;
              is_true ~msg:"extend" (st.extend ~id:a ~lease:c.lease (Span.s 10)) ;
              set 14 ;
              equal string "" (show_claims (claim st 1)) ;
              set 15 ;
              equal string (a ^ ":a!:2") (show_claims (claim st 1)) ) )
    ; test "retry sets the due time; release gives the attempt back" (fun () ->
          with_store (fun st at set ->
              let a = push st at 0 in
              let c = one (claim st 1) in
              is_true ~msg:"retry" (st.retry ~id:a ~lease:c.lease ~due:(at 7)) ;
              set 6 ;
              equal string "" (show_claims (claim st 1)) ;
              set 7 ;
              let c = one (claim st 1) in
              equal string (a ^ ":a!:2") (show_claims [c]) ;
              is_true ~msg:"release" (st.release ~id:a ~lease:c.lease) ;
              equal string (a ^ ":a!:2") (show_claims (claim st 1)) ) )
    ; test "fail removes the job; an unknown id gives false" (fun () ->
          with_store (fun st at set ->
              let a = push st at 0 in
              let c = one (claim st 1) in
              is_true ~msg:"fail" (st.fail ~id:a ~lease:c.lease ~error:"x") ;
              set 100 ;
              equal string "" (show_claims (claim st 1)) ;
              is_false ~msg:"no id" (st.complete ~id:"x" ~lease:c.lease) ;
              is_false ~msg:"an id of no job"
                (st.complete ~id:"99" ~lease:c.lease) ) )
    ; test "next_due: the first time of a claim of the names" (fun () ->
          with_store (fun st at set ->
              let next () = Option.get st.next_due ~names:["a"] in
              let show = function
                | None ->
                    "None"
                | Some d ->
                    string_of_int (ms (Time.diff Posix d (at 0)) / 1000)
              in
              equal string "None" (show (next ())) ;
              let _ = push st ~name:"b" at 1 in
              let _ = push st at 7 in
              let _ = push st at 3 in
              equal string "3" (show (next ())) ;
              set 3 ;
              let _ = claim st 1 in
              (* The lease ends at 13. *)
              equal string "7" (show (next ())) ;
              set 7 ;
              let _ = claim ~lease:(Span.s 1) st 1 in
              equal string "8" (show (next ())) ;
              (* The lease of the job of 7 ended: it can be claimed again. *)
              set 9 ;
              equal string "7" (show (next ())) ) )
    ; test "a lease after the end of the clock raises" (fun () ->
          with_store (fun st at _ ->
              let a = push st at 0 in
              ( match claim ~lease:(Span.of_ns Int64.max_int) st 1 with
              | _ ->
                  fail "no exception"
              | exception Invalid_argument _ ->
                  () ) ;
              let c = one (claim st 1) in
              match
                st.extend ~id:a ~lease:c.lease (Span.of_ns Int64.max_int)
              with
              | _ ->
                  fail "no exception"
              | exception Invalid_argument _ ->
                  () ) ) ]

(* A queue on [store], started in a scope, then stopped with no grace. *)
let run_queue q f =
  Fiber.scope (fun sw ->
      D.start q sw ;
      let v = f sw in
      D.stop ~grace:Span.zero q ; v )

let queue ?workers ?poll ?lease ?(errors = log ()) ?(store_errors = log ())
    ~jobs store =
  D.create ?workers ?poll ?lease
    ~on_error:(fun f -> add errors (show f))
    ~on_store_error:(fun e -> add store_errors (exn_name e))
    ~jobs store

let empty (st : D.store) = Option.get st.next_due ~names:["a"; "b"] = None

let queue_tests =
  group "Queue"
    [ test "enqueue 3, drain: each runs and is complete" (fun () ->
          S.run (fun _ ->
              let l = log () and errors = log () in
              let st = D.memory_store () in
              let j = text "a" (fun _ s -> add l s) in
              let q = queue ~errors ~jobs:[Job j] st in
              run_queue q (fun _ ->
                  List.iter (fun s -> ignore (D.enqueue q j s)) ["x"; "y"; "z"] ;
                  D.drain q ) ;
              equal (list string) ["x"; "y"; "z"] (lines l) ;
              is_true ~msg:"the store is empty" (empty st) ;
              equal (list string) [] (lines errors) ;
              equal string "Stopped r0 ok3 re0 f0 x0 u0 a0 rel0 lost0 se0"
                (stats q) ) )
    ; test "the codec and the info of a job" (fun () ->
          S.run (fun _ ->
              let l = log () in
              let st = D.memory_store () in
              let j =
                D.job ~name:"n" ~encode:string_of_int ~decode:int_of_string
                  (fun (i : D.info) n ->
                    add l (Printf.sprintf "%s %d %d" i.id i.attempt (n * 2)) )
              in
              let q = queue ~jobs:[Job j] st in
              run_queue q (fun _ ->
                  let id = D.enqueue q j 21 in
                  D.drain q ;
                  equal (list string) [id ^ " 1 42"] (lines l) ) ) )
    ; test "a delay: the job starts in [due, due + 1.1 poll]" (fun () ->
          S.run ~guard:long (fun _ ->
              let started = ref Span.zero in
              let st = D.memory_store () in
              let t0 = Time.now Mono in
              let j = text "a" (fun _ _ -> started := since t0) in
              let q = queue ~poll:(Span.s 1) ~jobs:[Job j] st in
              run_queue q (fun _ ->
                  ignore (D.enqueue q ~delay:(Span.s 10) j "x") ;
                  D.drain q ) ;
              let t = ms !started in
              is_true ~msg:(string_of_int t) (t >= 10_000 && t <= 11_100) ) )
    ; test "two queues of two names on one store: each runs its own" (fun () ->
          S.run (fun _ ->
              let l = log () in
              let st = D.memory_store () in
              let a = text "a" (fun _ s -> add l ("a " ^ s)) in
              let b = text "b" (fun _ s -> add l ("b " ^ s)) in
              let qa = queue ~jobs:[Job a] st and qb = queue ~jobs:[Job b] st in
              Fiber.scope (fun sw ->
                  D.start qa sw ;
                  ignore (D.enqueue qa b "1") ;
                  ignore (D.enqueue qa a "2") ;
                  D.drain qa ;
                  equal (list string) ["a 2"] (lines l) ;
                  D.start qb sw ;
                  D.drain qb ;
                  D.stop qa ;
                  D.stop qb ) ;
              equal (list string) ["a 2"; "b 1"] (lines l) ) )
    ; test "enqueue before start and after stop: the store keeps the job"
        (fun () ->
          S.run (fun _ ->
              let l = log () in
              let st = D.memory_store () in
              let j = text "a" (fun _ s -> add l s) in
              let q1 = queue ~jobs:[Job j] st in
              ignore (D.enqueue q1 j "before") ;
              run_queue q1 (fun _ -> D.drain q1) ;
              ignore (D.enqueue q1 j "after") ;
              equal (list string) ["before"] (lines l) ;
              let q2 = queue ~jobs:[Job j] st in
              run_queue q2 (fun _ -> D.drain q2) ;
              equal (list string) ["before"; "after"] (lines l) ) )
    ; test "row and wake: a job of the application claims at once" (fun () ->
          S.manual (fun sim ->
              let l = log () in
              let st = D.memory_store () in
              let j = text "a" (fun _ s -> add l s) in
              let q = queue ~poll:(Span.s 3600) ~jobs:[Job j] st in
              Fiber.scope (fun sw ->
                  D.start q sw ;
                  Weft_sim.settle sim ;
                  ignore (st.push (D.row j "outbox")) ;
                  Weft_sim.settle sim ;
                  equal (list string) [] (lines l) ;
                  D.wake q ;
                  Weft_sim.settle sim ;
                  equal (list string) ["outbox"] (lines l) ;
                  D.stop q ) ) )
    ; test "a full claim: the poller claims again at once, for the free permits"
        (fun () ->
          S.manual (fun sim ->
              let f = Fake.make () in
              let st = Fake.store f (D.memory_store ()) in
              let gate = Ivar.create () in
              let j = text "a" (fun _ _ -> sync (Ivar.read gate)) in
              let q = queue ~workers:3 ~poll:(Span.s 3600) ~jobs:[Job j] st in
              for _ = 1 to 5 do
                ignore (D.enqueue q j "x")
              done ;
              f.calls := [] ;
              Fiber.scope (fun sw ->
                  D.start q sw ;
                  Weft_sim.settle sim ;
                  equal (list string) ["claim 3 -> 0,1,2"] (lines f.calls) ;
                  Ivar.fill gate () ;
                  Weft_sim.settle sim ;
                  equal string "Running r0 ok5 re0 f0 x0 u0 a0 rel0 lost0 se0"
                    (stats q) ;
                  D.stop q ) ) ) ]

let failure_tests =
  group "Failures"
    [ test "a retry waits for its backoff on the POSIX clock" (fun () ->
          S.run ~guard:long (fun _ ->
              let times = log () in
              let st = D.memory_store () in
              let t0 = Time.now Mono in
              let tries = ref 0 in
              let j =
                text "a" (fun _ _ ->
                    add times (ms (since t0)) ;
                    incr tries ;
                    if !tries <= 2 then raise Boom )
              in
              let q = queue ~poll:(Span.ms 10) ~jobs:[Job j] st in
              let id =
                run_queue q (fun _ ->
                    let id = D.enqueue q j "x" in
                    D.drain q ; id )
              in
              (* The backoff of J1 with the hash of the id: 1 s and 2 s, less
                 the jitter. The poll adds at most 11 ms. *)
              let b =
                backoff_ms ~base:1_000 ~max:300_000 ~id:(Hashtbl.hash id)
              in
              match lines times with
              | [0; t1; t2] ->
                  is_true ~msg:"first retry" (t1 >= b 1 && t1 <= b 1 + 11) ;
                  is_true ~msg:"second retry"
                    (t2 - t1 >= b 2 && t2 - t1 <= b 2 + 11) ;
                  equal string "Stopped r0 ok1 re2 f0 x0 u0 a0 rel0 lost0 se0"
                    (stats q)
              | l ->
                  fail (String.concat " " (List.map string_of_int l)) ) )
    ; test "the last attempt fails: fail, on_failure then on_error" (fun () ->
          S.run ~guard:long (fun _ ->
              let l = log () in
              let st = D.memory_store () in
              let j =
                text "a"
                  ~retry:(J.retry ~attempts:3 ~base:(Span.ms 1) ())
                  ~on_failure:(fun arg f ->
                    add l ("on_failure " ^ arg ^ " " ^ show f) )
                  (fun _ _ -> raise Boom)
              in
              let q = queue ~poll:(Span.ms 10) ~errors:l ~jobs:[Job j] st in
              run_queue q (fun _ ->
                  ignore (D.enqueue q j "x") ;
                  D.drain q ) ;
              equal (list string)
                ["on_failure x a 0 3 Failed Boom"; "a 0 3 Failed Boom"]
                (lines l) ;
              is_true ~msg:"the store is empty" (empty st) ;
              equal string "Stopped r0 ok0 re2 f1 x0 u0 a0 rel0 lost0 se0"
                (stats q) ) )
    ; test "on gives false: Discarded at once" (fun () ->
          S.run (fun _ ->
              let errors = log () in
              let st = D.memory_store () in
              let j =
                text "a"
                  ~retry:(J.retry ~on:(fun _ -> false) ())
                  (fun _ _ -> raise Boom)
              in
              let q = queue ~errors ~jobs:[Job j] st in
              run_queue q (fun _ ->
                  ignore (D.enqueue q j "x") ;
                  D.drain q ) ;
              equal (list string) ["a 0 1 Discarded Boom"] (lines errors) ;
              equal string "Stopped r0 ok0 re0 f0 x1 u0 a0 rel0 lost0 se0"
                (stats q) ) )
    ; test "decode raises: retries, then Undecodable, with no on_failure"
        (fun () ->
          S.run ~guard:long (fun _ ->
              let l = log () in
              let st = D.memory_store () in
              let j =
                D.job ~name:"a" ~encode:Fun.id
                  ~decode:(fun _ -> raise Boom)
                  ~retry:(J.retry ~attempts:2 ~base:(Span.ms 1) ())
                  ~on_failure:(fun _ _ -> add l "on_failure")
                  (fun _ _ -> add l "ran")
              in
              let q = queue ~poll:(Span.ms 10) ~errors:l ~jobs:[Job j] st in
              run_queue q (fun _ ->
                  ignore (D.enqueue q j "x") ;
                  D.drain q ) ;
              equal (list string) ["a 0 2 Undecodable Boom"] (lines l) ;
              equal string "Stopped r0 ok0 re1 f0 x0 u1 a0 rel0 lost0 se0"
                (stats q) ) )
    ; test "a claim with more attempts than the policy: Abandoned, no run"
        (fun () ->
          S.run (fun _ ->
              let l = log () in
              let f = Fake.make () in
              f.bump <- 5 ;
              let st = Fake.store f (D.memory_store ()) in
              let j = text "a" (fun _ _ -> add l "ran") in
              let q = queue ~errors:l ~jobs:[Job j] st in
              run_queue q (fun _ ->
                  ignore (D.enqueue q j "x") ;
                  D.drain q ) ;
              equal (list string) ["a 0 6 Abandoned"] (lines l) ;
              is_true ~msg:"fail" (List.mem "fail 0" !(f.calls)) ;
              equal string "Stopped r0 ok0 re0 f0 x0 u0 a1 rel0 lost0 se0"
                (stats q) ) )
    ; test "on raises: Failed, then the queue fails with it" (fun () ->
          S.run (fun _ ->
              let errors = log () in
              let st = D.memory_store () in
              let j =
                text "a"
                  ~retry:(J.retry ~on:(fun _ -> failwith "on") ())
                  (fun _ _ -> raise Boom)
              in
              let q = queue ~errors ~jobs:[Job j] st in
              raises (Failure "on") (fun () ->
                  Fiber.scope (fun sw ->
                      D.start q sw ;
                      ignore (D.enqueue q j "x") ;
                      D.drain q ) ) ;
              equal (list string) ["a 0 1 Failed Boom"] (lines errors) ) )
    ; test "on_error raises: the report occurs, then the queue fails" (fun () ->
          S.run (fun _ ->
              let l = log () in
              let st = D.memory_store () in
              let j = text "a" ~retry:J.no_retry (fun _ _ -> raise Boom) in
              let q =
                D.create
                  ~on_error:(fun f ->
                    add l (show f) ;
                    raise Exit )
                  ~jobs:[Job j] st
              in
              raises Exit (fun () ->
                  Fiber.scope (fun sw ->
                      D.start q sw ;
                      ignore (D.enqueue q j "x") ;
                      D.drain q ) ) ;
              equal (list string) ["a 0 1 Failed Boom"] (lines l) ) )
    ; test "encode raises: enqueue raises, and the store has no job" (fun () ->
          S.run (fun _ ->
              let st = D.memory_store () in
              let j =
                D.job ~name:"a"
                  ~encode:(fun _ -> raise Boom)
                  ~decode:Fun.id
                  (fun _ _ -> ())
              in
              let q = queue ~jobs:[Job j] st in
              raises Boom (fun () -> D.enqueue q j "x") ;
              is_true ~msg:"empty" (empty st) ) ) ]

let lease_tests =
  group "Leases"
    [ test "a long job: extend each lease / 3, one claim" (fun () ->
          S.run ~guard:long (fun _ ->
              let f = Fake.make () in
              let st = Fake.store f (D.memory_store ()) in
              let j = text "a" (fun _ _ -> Time.sleep Mono (Span.ms 10_500)) in
              let q = queue ~lease:(Span.s 3) ~jobs:[Job j] st in
              run_queue q (fun _ ->
                  ignore (D.enqueue q j "x") ;
                  D.drain q ) ;
              equal (list string)
                ( ["push 0"; "claim 4 -> 0"]
                @ List.init 10 (fun _ -> "extend 0")
                @ ["complete 0"] )
                (List.filter
                   (fun s -> s <> "claim 4 -> " && s <> "claim 3 -> ")
                   (lines f.calls) ) ) )
    ; test "extend gives false: the job is cancelled, no other call" (fun () ->
          S.run ~guard:long (fun _ ->
              let l = log () in
              let f = Fake.make () in
              f.result <- (function "extend" -> Some false | _ -> None) ;
              let st = Fake.store f (D.memory_store ()) in
              let t0 = Time.now Mono in
              (* The cancel comes at the first extend, at 1 s. *)
              let j =
                text "a" (fun _ _ ->
                    match Time.sleep Mono (Span.s 100) with
                    | () ->
                        add l "ended"
                    | exception (Fiber.Cancelled _ as e) ->
                        add l (Printf.sprintf "cancelled at %d" (ms (since t0))) ;
                        raise e )
              in
              let q = queue ~lease:(Span.s 3) ~errors:l ~jobs:[Job j] st in
              run_queue q (fun _ ->
                  ignore (D.enqueue q j "x") ;
                  Time.sleep Mono (Span.s 2) ) ;
              equal (list string) ["cancelled at 1000"] (lines l) ;
              is_false ~msg:"no complete" (List.mem "complete 0" !(f.calls)) ;
              equal string "Stopped r0 ok0 re0 f0 x0 u0 a0 rel0 lost1 se0"
                (stats q) ) )
    ; test "a stale complete gives false: lease_lost, no report" (fun () ->
          S.run (fun _ ->
              let errors = log () in
              let f = Fake.make () in
              f.result <- (function "complete" -> Some false | _ -> None) ;
              let st = Fake.store f (D.memory_store ()) in
              let j = text "a" (fun _ _ -> ()) in
              let q = queue ~errors ~jobs:[Job j] st in
              run_queue q (fun _ ->
                  ignore (D.enqueue q j "x") ;
                  Time.sleep Mono (Span.ms 1) ) ;
              equal (list string) [] (lines errors) ;
              equal string "Stopped r0 ok0 re0 f0 x0 u0 a0 rel0 lost1 se0"
                (stats q) ) )
    ; test "a stale retry and a stale fail: lease_lost, no report" (fun () ->
          S.run (fun _ ->
              let errors = log () in
              let f = Fake.make () in
              f.result <- (function "retry" | "fail" -> Some false | _ -> None) ;
              let st = Fake.store f (D.memory_store ()) in
              let j = text "a" (fun _ s -> if s = "retry" then raise Boom) in
              let k = text "b" ~retry:J.no_retry (fun _ _ -> raise Boom) in
              let q = queue ~errors ~jobs:[Job j; Job k] st in
              run_queue q (fun _ ->
                  ignore (D.enqueue q j "retry") ;
                  ignore (D.enqueue q k "fail") ;
                  Time.sleep Mono (Span.ms 1) ) ;
              equal (list string) [] (lines errors) ;
              equal string "Stopped r0 ok0 re0 f0 x0 u0 a0 rel0 lost2 se0"
                (stats q) ) )
    ; test "a crash: another queue runs the job after the lease" (fun () ->
          S.run ~guard:long (fun _ ->
              let l = log () in
              let st = D.memory_store () in
              let started = Ivar.create () in
              let j =
                text "a" (fun (i : D.info) _ ->
                    add l (Printf.sprintf "run %d" i.attempt) ;
                    if i.attempt = 1 then (
                      Ivar.fill started () ;
                      Time.sleep Mono (Span.s 1000) ) )
              in
              let f = Fake.make () in
              let qa =
                queue ~lease:(Span.s 2) ~jobs:[Job j] (Fake.store f st)
              in
              let t0 = Time.now Mono in
              Fiber.scope (fun inner ->
                  D.start qa inner ;
                  ignore (D.enqueue qa j "x") ;
                  sync (Ivar.read started) ;
                  Fiber.cancel inner Exit ) ;
              (* No call of qa after the claim. *)
              equal (list string) ["push 0"; "claim 4 -> 0"]
                (List.filter (fun s -> s <> "claim 3 -> ") (lines f.calls)) ;
              let qb = queue ~poll:(Span.ms 100) ~jobs:[Job j] st in
              run_queue qb (fun _ -> D.drain qb) ;
              equal (list string) ["run 1"; "run 2"] (lines l) ;
              is_true ~msg:"after the lease" (ms (since t0) >= 2000) ) ) ]

let stop_tests =
  group "Stop"
    [ test "a job that ends in the grace: complete" (fun () ->
          S.manual (fun sim ->
              let f = Fake.make () in
              let st = Fake.store f (D.memory_store ()) in
              let j = text "a" (fun _ _ -> Time.sleep Mono (Span.s 2)) in
              let q = queue ~jobs:[Job j] st in
              Fiber.scope (fun sw ->
                  D.start q sw ;
                  ignore (D.enqueue q j "x") ;
                  Weft_sim.settle sim ;
                  let t0 = Time.now Mono in
                  let c = S.call sw (fun () -> D.stop ~grace:(Span.s 10) q) in
                  S.ends_at sim ~t0 (Span.s 2) c ) ;
              is_true ~msg:"complete" (List.mem "complete 0" !(f.calls)) ;
              equal string "Stopped r0 ok1 re0 f0 x0 u0 a0 rel0 lost0 se0"
                (stats q) ) )
    ; test "the end of the grace: release, and the attempt comes back"
        (fun () ->
          S.run ~guard:long (fun _ ->
              let l = log () in
              let st = D.memory_store () in
              let j =
                text "a" (fun (i : D.info) _ ->
                    add l (Printf.sprintf "run %d" i.attempt) ;
                    if List.length !l = 1 then Time.sleep Mono (Span.s 100) )
              in
              let q1 = queue ~jobs:[Job j] st in
              Fiber.scope (fun sw ->
                  D.start q1 sw ;
                  ignore (D.enqueue q1 j "x") ;
                  Time.sleep Mono (Span.ms 1) ;
                  D.stop ~grace:(Span.s 5) q1 ) ;
              equal string "Stopped r0 ok0 re0 f0 x0 u0 a0 rel1 lost0 se0"
                (stats q1) ;
              let q2 = queue ~jobs:[Job j] st in
              run_queue q2 (fun _ -> D.drain q2) ;
              equal (list string) ["run 1"; "run 1"] (lines l) ) )
    ; test "a job that ignores its cancel: release after it ends" (fun () ->
          S.manual (fun sim ->
              let f = Fake.make () in
              let st = Fake.store f (D.memory_store ()) in
              let j =
                text "a" (fun _ _ ->
                    Fiber.shield (fun () -> Time.sleep Mono (Span.s 100)) ;
                    Fiber.yield () )
              in
              let q = queue ~jobs:[Job j] st in
              Fiber.scope (fun sw ->
                  D.start q sw ;
                  ignore (D.enqueue q j "x") ;
                  Weft_sim.settle sim ;
                  let t0 = Time.now Mono in
                  let c = S.call sw (fun () -> D.stop ~grace:(Span.s 5) q) in
                  S.advance sim (Span.s 50) ;
                  is_false ~msg:"no release in the shield"
                    (List.mem "release 0" !(f.calls)) ;
                  S.ends_at sim ~t0 (Span.s 100) c ) ;
              is_true ~msg:"release" (List.mem "release 0" !(f.calls)) ;
              equal string "Stopped r0 ok0 re0 f0 x0 u0 a0 rel1 lost0 se0"
                (stats q) ) )
    ; test "a job that catches the cancel and returns: complete" (fun () ->
          S.manual (fun sim ->
              let f = Fake.make () in
              let st = Fake.store f (D.memory_store ()) in
              let j =
                text "a" (fun _ _ ->
                    try Time.sleep Mono (Span.s 100)
                    with Fiber.Cancelled _ -> () )
              in
              let q = queue ~jobs:[Job j] st in
              Fiber.scope (fun sw ->
                  D.start q sw ;
                  ignore (D.enqueue q j "x") ;
                  Weft_sim.settle sim ;
                  let t0 = Time.now Mono in
                  let c = S.call sw (fun () -> D.stop ~grace:(Span.s 5) q) in
                  S.ends_at sim ~t0 (Span.s 5) c ) ;
              is_true ~msg:"complete" (List.mem "complete 0" !(f.calls)) ;
              equal string "Stopped r0 ok1 re0 f0 x0 u0 a0 rel0 lost0 se0"
                (stats q) ) )
    ; test "a release in progress is not cut by the cancel" (fun () ->
          S.manual (fun sim ->
              let f = Fake.make () in
              let gate = Ivar.create () in
              f.before <-
                (function "release" -> sync (Ivar.read gate) | _ -> ()) ;
              let st = Fake.store f (D.memory_store ()) in
              let j = text "a" (fun _ _ -> Time.sleep Mono (Span.s 100)) in
              let q = queue ~jobs:[Job j] st in
              Fiber.scope (fun sw ->
                  D.start q sw ;
                  ignore (D.enqueue q j "x") ;
                  Weft_sim.settle sim ;
                  let c = S.call sw (fun () -> D.stop ~grace:Span.zero q) in
                  Weft_sim.settle sim ;
                  is_false ~msg:"stop waits for the release" (S.ended sim c) ;
                  Ivar.fill gate () ;
                  is_true ~msg:"stop ended" (S.ended sim c) ;
                  S.await c ) ;
              equal string "Stopped r0 ok0 re0 f0 x0 u0 a0 rel1 lost0 se0"
                (stats q) ) )
    ; test "a second stop waits for the first; stop before start" (fun () ->
          S.manual (fun sim ->
              let st = D.memory_store () in
              let j = text "a" (fun _ _ -> Time.sleep Mono (Span.s 2)) in
              let q = queue ~jobs:[Job j] st in
              Fiber.scope (fun sw ->
                  D.start q sw ;
                  ignore (D.enqueue q j "x") ;
                  Weft_sim.settle sim ;
                  let t0 = Time.now Mono in
                  let c1 = S.call sw (fun () -> D.stop ~grace:(Span.s 10) q) in
                  let c2 = S.call sw (fun () -> D.stop ~grace:(Span.s 1) q) in
                  S.ends_at sim ~t0 (Span.s 2) c1 ;
                  is_true ~msg:"second" (S.ended sim c2) ;
                  S.await c2 ) ;
              let q2 = queue ~jobs:[Job j] st in
              D.stop q2 ;
              equal string "Stopped r0 ok0 re0 f0 x0 u0 a0 rel0 lost0 se0"
                (stats q2) ;
              Fiber.scope (fun sw ->
                  match D.start q2 sw with
                  | () ->
                      fail "start"
                  | exception Invalid_argument _ ->
                      () ) ) )
    ; test "a stop with no grace before the queue fiber runs: no claim"
        (fun () ->
          S.run (fun _ ->
              let f = Fake.make () in
              let st = Fake.store f (D.memory_store ()) in
              let j = text "a" (fun _ _ -> ()) in
              let q = queue ~jobs:[Job j] st in
              ignore (D.enqueue q j "x") ;
              Fiber.scope (fun sw -> D.start q sw ; D.stop ~grace:Span.zero q) ;
              equal (list string) ["push 0"] (lines f.calls) ) )
    ; test "start on a closed scope: the queue is stopped" (fun () ->
          S.run (fun _ ->
              let q =
                queue ~jobs:[Job (text "a" (fun _ _ -> ()))] (D.memory_store ())
              in
              let closed = Fiber.scope Fun.id in
              raises Fiber.Scope_closed (fun () -> D.start q closed) ;
              equal string "Stopped r0 ok0 re0 f0 x0 u0 a0 rel0 lost0 se0"
                (stats q) ;
              D.stop q ) ) ]

let store_failure_tests =
  group "Store failures"
    [ test "claim raises: on_store_error, then the job runs" (fun () ->
          S.run ~guard:long (fun _ ->
              let store_errors = log () and times = log () in
              let f = Fake.make () in
              let fails = ref 3 in
              let t0 = Time.now Mono in
              f.before <-
                (function
                | "claim" ->
                    add times (ms (since t0)) ;
                    if !fails > 0 then (decr fails ; raise Boom)
                | _ ->
                    () ) ;
              let st = Fake.store f (D.memory_store ()) in
              let l = log () in
              let j = text "a" (fun _ s -> add l s) in
              let q = queue ~poll:(Span.s 1) ~store_errors ~jobs:[Job j] st in
              run_queue q (fun _ ->
                  ignore (D.enqueue q j "x") ;
                  D.drain q ) ;
              equal (list string) ["x"] (lines l) ;
              equal (list string) ["Boom"; "Boom"; "Boom"] (lines store_errors) ;
              (* The waits: 1 s, 2 s, 4 s less the jitter of J1 (id 0). *)
              equal string "Stopped r0 ok1 re0 f0 x0 u0 a0 rel0 lost0 se3"
                (stats q) ) )
    ; test "complete raises: the job runs again after its lease" (fun () ->
          S.run ~guard:long (fun _ ->
              let l = log () and store_errors = log () in
              let f = Fake.make () in
              let once = ref true in
              f.before <-
                (function
                | "complete" when !once ->
                    once := false ;
                    raise Boom
                | _ ->
                    () ) ;
              let st = Fake.store f (D.memory_store ()) in
              let j =
                text "a" (fun (i : D.info) _ ->
                    add l (Printf.sprintf "run %d" i.attempt) )
              in
              let q =
                queue ~poll:(Span.ms 100) ~lease:(Span.s 2) ~store_errors
                  ~jobs:[Job j] st
              in
              run_queue q (fun _ ->
                  ignore (D.enqueue q j "x") ;
                  D.drain q ) ;
              equal (list string) ["run 1"; "run 2"] (lines l) ;
              equal (list string) ["Boom"] (lines store_errors) ;
              equal string "Stopped r0 ok1 re0 f0 x0 u0 a0 rel0 lost0 se1"
                (stats q) ) )
    ; test "extend raises: on_store_error, and the job continues" (fun () ->
          S.run ~guard:long (fun _ ->
              let store_errors = log () in
              let f = Fake.make () in
              f.before <- (function "extend" -> raise Boom | _ -> ()) ;
              let st = Fake.store f (D.memory_store ()) in
              let j = text "a" (fun _ _ -> Time.sleep Mono (Span.ms 1500)) in
              let q = queue ~lease:(Span.s 3) ~store_errors ~jobs:[Job j] st in
              run_queue q (fun _ ->
                  ignore (D.enqueue q j "x") ;
                  D.drain q ) ;
              equal (list string) ["Boom"] (lines store_errors) ;
              equal string "Stopped r0 ok1 re0 f0 x0 u0 a0 rel0 lost0 se1"
                (stats q) ) )
    ; test "the store gives an unknown name: release and a store error"
        (fun () ->
          S.run (fun _ ->
              let store_errors = log () in
              let f = Fake.make () in
              let st = Fake.store f (D.memory_store ()) in
              let j = text "a" (fun _ _ -> ()) in
              let q = queue ~store_errors ~jobs:[Job j] st in
              f.rename <- (fun _ -> "zz") ;
              run_queue q (fun _ ->
                  ignore (D.enqueue q j "x") ;
                  Time.sleep Mono (Span.ms 1) ) ;
              is_true ~msg:"release" (List.mem "release 0" !(f.calls)) ;
              equal (list string)
                [ "Invalid_argument(\"Caravan_jobs.Durable: the store gave the \
                   job type zz\")" ]
                (lines store_errors) ;
              equal string "Stopped r0 ok0 re0 f0 x0 u0 a0 rel0 lost0 se1"
                (stats q) ) ) ]

let invalid f =
  match f () with
  | _ ->
      fail "no Invalid_argument"
  | exception Invalid_argument _ ->
      ()

let argument_tests =
  group "Arguments"
    [ test "create" (fun () ->
          let st = D.memory_store () in
          let a = D.Job (text "a" (fun _ _ -> ())) in
          invalid (fun () -> D.create ~workers:0 ~jobs:[a] st) ;
          invalid (fun () -> D.create ~poll:Span.zero ~jobs:[a] st) ;
          invalid (fun () -> D.create ~lease:(Span.ms 999) ~jobs:[a] st) ;
          invalid (fun () -> D.create ~jobs:[] st) ;
          invalid (fun () -> D.create ~jobs:[a; a] st) ;
          ignore
            (D.create ~workers:1 ~poll:(Span.ns 1) ~lease:(Span.s 1) ~jobs:[a]
               st ) )
    ; test "row: delay" (fun () ->
          S.run (fun _ ->
              let j = text "a" (fun _ _ -> ()) in
              Time.sleep Mono (Span.s 1) ;
              invalid (fun () -> D.row ~delay:(Span.ns (-1)) j "x") ;
              invalid (fun () -> D.row ~delay:(Span.of_ns Int64.max_int) j "x") ;
              let now = Time.now Posix in
              let r = D.row ~delay:(Span.s 5) j "x" in
              equal string "a x 5000"
                (Printf.sprintf "%s %s %d" r.name r.payload
                   (ms (Time.diff Posix r.due now)) ) ;
              equal int 0 (ms (Time.diff Posix (D.row j "x").due now)) ) )
    ; test "drain needs next_due; drain and stop in a job raise" (fun () ->
          S.run (fun _ ->
              let m = D.memory_store () in
              let st =
                D.store ~push:m.push ~claim:m.claim ~extend:m.extend
                  ~complete:m.complete ~retry:m.retry ~release:m.release
                  ~fail:m.fail ()
              in
              let q = queue ~jobs:[Job (text "a" (fun _ _ -> ()))] st in
              invalid (fun () -> D.drain q) ;
              let l = log () in
              let rec q2 =
                lazy
                  (queue
                     ~jobs:
                       [ Job
                           (text "a" (fun _ _ ->
                                invalid (fun () -> D.drain (Lazy.force q2)) ;
                                invalid (fun () -> D.stop (Lazy.force q2)) ;
                                add l "checked" ) ) ]
                     (D.memory_store ()) )
              in
              let q2 = Lazy.force q2 in
              run_queue q2 (fun _ ->
                  ignore (D.enqueue q2 (text "a" (fun _ _ -> ())) "x") ;
                  D.drain q2 ) ;
              equal (list string) ["checked"] (lines l) ) ) ]

let race_tests =
  group "Races"
    [ test "two queues on 4 domains: each job runs exactly once" (fun () ->
          S.race (fun _ ->
              let n = 60 in
              let runs = Array.init n (fun _ -> Atomic.make 0) in
              let st = D.memory_store () in
              let j =
                D.job ~name:"n" ~encode:string_of_int ~decode:int_of_string
                  (fun _ i ->
                    Fiber.yield () ;
                    Atomic.incr runs.(i) )
              in
              let qa = queue ~workers:2 ~poll:(Span.ms 1) ~jobs:[Job j] st
              and qb = queue ~workers:3 ~poll:(Span.ms 1) ~jobs:[Job j] st in
              Fiber.scope (fun sw ->
                  D.start qa sw ;
                  D.start qb sw ;
                  Fiber.scope (fun s ->
                      for d = 0 to 3 do
                        Fiber.fork s (fun () ->
                            for i = 0 to (n / 4) - 1 do
                              ignore
                                (D.enqueue
                                   (if i mod 2 = 0 then qa else qb)
                                   j
                                   ((d * n / 4) + i) )
                            done )
                      done ) ;
                  D.drain qa ;
                  D.drain qb ;
                  D.stop qa ;
                  D.stop qb ) ;
              Array.iteri
                (fun i r -> equal int ~msg:(string_of_int i) 1 (Atomic.get r))
                runs ) ) ]

let server_tests =
  let open Server_support in
  group "Server.run"
    [ test "a handler enqueues in its grace: the job runs" (fun () ->
          let l = log () in
          let st = D.memory_store () in
          let j = text "mail" (fun _ s -> add l ("sent to " ^ s)) in
          let q = queue ~jobs:[Job j] st in
          let started = Ivar.create () in
          let handler req =
            Ivar.fill started () ;
            sync
              (Caravan.Conn_info.stopping
                 (Option.get (Caravan.Request.conn req)) ) ;
            ignore (D.enqueue q j "ann") ;
            Caravan.Response.string "queued"
          in
          Caravan.Server.run
            ~config:{config with shutdown_grace= Span.s 10}
            ~engine:Fake_engine.serve
            ~services:[D.service q]
            ~on_start:(fun server ->
              let flow, r = connect server in
              Caravan.Flow.write_string flow (get "/") ;
              sync (Ivar.read started) ;
              Weft_sim.signal (Option.get !os_sim) Signal.SIGTERM ;
              equal string {|200 "queued" close|} (read_response r) ;
              Caravan.Flow.close flow )
            (module Sim_auto_os)
            handler ;
          equal (list string) ["sent to ann"] (lines l) ;
          equal string "Stopped r0 ok1 re0 f0 x0 u0 a0 rel0 lost0 se0" (stats q) )
    ]

(* A job that ends with no run of its function leaves the set of the extender:
   no [extend] after its end. *)
let no_extend name setup j =
  test name (fun () ->
      S.run ~guard:long (fun _ ->
          let f = Fake.make () in
          setup f ;
          let st = Fake.store f (D.memory_store ()) in
          let q = queue ~lease:(Span.s 3) ~jobs:[Job j] st in
          run_queue q (fun _ ->
              ignore (D.enqueue q j "x") ;
              Time.sleep Mono (Span.ms 2500) ) ;
          is_false ~msg:"no extend" (List.mem "extend 0" !(f.calls)) ) )

let detail_tests =
  group "Details"
    [ test "the default lease is 60 s" (fun () ->
          S.run (fun _ ->
              let f = Fake.make () in
              let st = Fake.store f (D.memory_store ()) in
              let q = D.create ~jobs:[Job (text "a" (fun _ _ -> ()))] st in
              run_queue q (fun _ -> Time.sleep Mono (Span.ms 1)) ;
              equal int 60_000 (ms (Option.get f.lease_seen)) ) )
    ; test
        "the times of the claims: poll with its jitter, the backoff of \
         failures to 1 min, and its reset" (fun () ->
          S.run ~guard:long (fun _ ->
              let f = Fake.make () in
              let calls = ref 0 and times = log () in
              let t0 = Time.now Mono and seed = seed () in
              (* Claims 1 to 7 fail, 8 gives nothing, 9 fails, 10 and 11 give
                 nothing. *)
              f.before <-
                (function
                | "claim" ->
                    incr calls ;
                    add times (ms (since t0)) ;
                    if !calls <= 7 || !calls = 9 then raise Boom
                | _ ->
                    () ) ;
              let st = Fake.store f (D.memory_store ()) in
              let q =
                queue ~poll:(Span.s 1) ~jobs:[Job (text "a" (fun _ _ -> ()))] st
              in
              run_queue q (fun _ -> Time.sleep Mono (Span.s 200)) ;
              let b = backoff_ms ~base:1_000 ~max:60_000 ~id:seed in
              let jitter n = 1_000 + (10 * (Hashtbl.hash (seed, n) mod 11)) in
              let rec gaps = function
                | a :: (b :: _ as rest) ->
                    (b - a) :: gaps rest
                | _ ->
                    []
              in
              match gaps (lines times) with
              | g1 :: g2 :: g3 :: g4 :: g5 :: g6 :: g7 :: g8 :: g9 :: g10 :: g11
                :: _ ->
                  equal (list int)
                    [ b 1
                    ; b 2
                    ; b 3
                    ; b 4
                    ; b 5
                    ; b 6
                    ; b 7
                    ; jitter 0
                    ; b 1
                    ; jitter 1
                    ; jitter 2 ]
                    [g1; g2; g3; g4; g5; g6; g7; g8; g9; g10; g11]
              | l ->
                  fail (String.concat " " (List.map string_of_int l)) ) )
    ; no_extend "an undecodable job leaves the extender" ignore
        (D.job ~name:"a" ~encode:Fun.id
           ~decode:(fun _ -> raise Boom)
           ~retry:J.no_retry
           (fun _ _ -> ()) )
    ; no_extend "an abandoned job leaves the extender"
        (fun f -> f.bump <- 5)
        (text "a" (fun _ _ -> ()))
    ; no_extend "a job of an unknown name leaves the extender"
        (fun f -> f.rename <- (fun _ -> "zz"))
        (text "a" (fun _ _ -> ()))
    ; test "the lease ends before the job starts: no run, lease_lost" (fun () ->
          S.run ~guard:long (fun _ ->
              let l = log () in
              let f = Fake.make () in
              f.result <- (function "extend" -> Some false | _ -> None) ;
              let st = Fake.store f (D.memory_store ()) in
              (* The decode takes 1.5 s: the extend of 1 s comes first. *)
              let j =
                D.job ~name:"a" ~encode:Fun.id
                  ~decode:(fun s ->
                    Time.sleep Mono (Span.ms 1500) ;
                    s )
                  (fun _ _ -> add l "ran")
              in
              let q = queue ~lease:(Span.s 3) ~jobs:[Job j] st in
              run_queue q (fun _ ->
                  ignore (D.enqueue q j "x") ;
                  Time.sleep Mono (Span.s 2) ) ;
              equal (list string) [] (lines l) ;
              is_false ~msg:"no complete" (List.mem "complete 0" !(f.calls)) ;
              equal string "Stopped r0 ok0 re0 f0 x0 u0 a0 rel0 lost1 se0"
                (stats q) ) )
    ; test "drain returns at the stop" (fun () ->
          S.manual (fun sim ->
              let st = D.memory_store () in
              let j = text "a" (fun _ _ -> ()) in
              let q = queue ~poll:(Span.s 3600) ~jobs:[Job j] st in
              Fiber.scope (fun sw ->
                  D.start q sw ;
                  ignore (D.enqueue q ~delay:(Span.s 3600) j "x") ;
                  let c = S.call sw (fun () -> D.drain q) in
                  is_false ~msg:"drain waits" (S.ended sim c) ;
                  D.stop q ;
                  is_true ~msg:"drain ended" (S.ended sim c) ;
                  S.await c ) ) )
    ; test "drain wakes the poller at the due time" (fun () ->
          S.run ~guard:long (fun _ ->
              let started = ref (-1) in
              let st = D.memory_store () in
              let t0 = Time.now Mono in
              let j = text "a" (fun _ _ -> started := ms (since t0)) in
              let q = queue ~poll:(Span.s 3600) ~jobs:[Job j] st in
              run_queue q (fun _ ->
                  ignore (D.enqueue q ~delay:(Span.s 10) j "x") ;
                  D.drain q ) ;
              equal int 10_000 !started ) )
    ; test "enqueue wakes the poller" (fun () ->
          S.manual (fun sim ->
              let l = log () in
              let st = D.memory_store () in
              let j = text "a" (fun _ s -> add l s) in
              let q = queue ~poll:(Span.s 3600) ~jobs:[Job j] st in
              Fiber.scope (fun sw ->
                  D.start q sw ;
                  Weft_sim.settle sim ;
                  ignore (D.enqueue q j "now") ;
                  Weft_sim.settle sim ;
                  equal (list string) ["now"] (lines l) ;
                  D.stop q ) ) )
    ; test "the default grace: 10 s" (fun () ->
          S.manual (fun sim ->
              let st = D.memory_store () in
              let j = text "a" (fun _ _ -> Time.sleep Mono (Span.s 100)) in
              let q = queue ~jobs:[Job j] st in
              Fiber.scope (fun sw ->
                  D.start q sw ;
                  ignore (D.enqueue q j "x") ;
                  Weft_sim.settle sim ;
                  let t0 = Time.now Mono in
                  let c = S.call sw (fun () -> D.stop q) in
                  S.ends_at sim ~t0 (Span.s 10) c ) ) )
    ; test "start outside fiber code raises, and the queue stays created"
        (fun () ->
          let q =
            queue ~jobs:[Job (text "a" (fun _ _ -> ()))] (D.memory_store ())
          in
          let sw = S.run (fun _ -> Fiber.scope Fun.id) in
          ( match D.start q sw with
          | () ->
              fail "start ended"
          | exception Fiber.Not_in_fiber _ ->
              () ) ;
          equal string "Created r0 ok0 re0 f0 x0 u0 a0 rel0 lost0 se0" (stats q) )
    ; test "a delay of zero is now" (fun () ->
          S.run (fun _ ->
              let j = text "a" (fun _ _ -> ()) in
              let now = Time.now Posix in
              equal int 0
                (ms (Time.diff Posix (D.row ~delay:Span.zero j "x").due now)) ) )
    ]

let claims f =
  List.length
    (List.filter
       (fun s -> String.starts_with ~prefix:"claim" s)
       !(f.Fake.calls) )

let review_tests =
  group "Review cases"
    [ test "after the stop starts, the poller claims nothing" (fun () ->
          S.race ~domains:1 (fun _ ->
              let f = Fake.make () in
              let mem = D.memory_store () in
              let st = Fake.store f mem in
              let l = log () in
              let j = text "a" (fun _ s -> add l s) in
              let q = queue ~poll:(Span.s 3600) ~jobs:[Job j] st in
              Fiber.scope (fun sw ->
                  D.start q sw ;
                  Fiber.yield () ;
                  (* A job of another process: no wake of this queue. *)
                  ignore (mem.push (D.row j "x")) ;
                  let before = claims f in
                  D.stop ~grace:(Span.s 10) q ;
                  equal int before (claims f) ) ;
              equal (list string) [] (lines l) ) )
    ; test "a delayed enqueue makes no claim" (fun () ->
          S.manual (fun sim ->
              let f = Fake.make () in
              let st = Fake.store f (D.memory_store ()) in
              let j = text "a" (fun _ _ -> ()) in
              let q = queue ~poll:(Span.s 3600) ~jobs:[Job j] st in
              Fiber.scope (fun sw ->
                  D.start q sw ;
                  Weft_sim.settle sim ;
                  let before = claims f in
                  ignore (D.enqueue q ~delay:(Span.s 10) j "x") ;
                  Weft_sim.settle sim ;
                  equal int before (claims f) ;
                  D.stop q ) ) )
    ; test "a claim that ends after the grace: its jobs go back to the store"
        (fun () ->
          S.manual (fun sim ->
              let f = Fake.make () in
              f.before <-
                (function "claim" -> Time.sleep Mono (Span.s 1) | _ -> ()) ;
              let mem = D.memory_store () in
              let st = Fake.store f mem in
              let l = log () in
              let j =
                text "a" (fun (i : D.info) _ ->
                    add l (Printf.sprintf "run %d" i.attempt) )
              in
              let q = queue ~jobs:[Job j] st in
              ignore (D.enqueue q j "x") ;
              Fiber.scope (fun sw ->
                  D.start q sw ;
                  Weft_sim.settle sim ;
                  let t0 = Time.now Mono in
                  let c = S.call sw (fun () -> D.stop ~grace:Span.zero q) in
                  S.ends_at sim ~t0 (Span.s 1) c ) ;
              equal (list string) [] (lines l) ;
              equal string "Stopped r0 ok0 re0 f0 x0 u0 a0 rel1 lost0 se0"
                (stats q) ;
              (* The attempt came back. *)
              let q2 = queue ~jobs:[Job j] mem in
              Fiber.scope (fun sw ->
                  D.start q2 sw ; Weft_sim.settle sim ; D.stop q2 ) ;
              equal (list string) ["run 1"] (lines l) ) )
    ; test "a job that ignores its cancel keeps its lease after the grace"
        (fun () ->
          S.run ~guard:long (fun _ ->
              let l = log () in
              let st = D.memory_store () in
              let running = Atomic.make 0 and peak = Atomic.make 0 in
              let j =
                text "a" (fun (i : D.info) _ ->
                    let n = Atomic.fetch_and_add running 1 + 1 in
                    if n > Atomic.get peak then Atomic.set peak n ;
                    add l (Printf.sprintf "run %d" i.attempt) ;
                    Fun.protect
                      ~finally:(fun () -> Atomic.decr running)
                      (fun () ->
                        if List.length !l = 1 then (
                          Fiber.shield (fun () -> Time.sleep Mono (Span.s 20)) ;
                          Fiber.yield () ) ) )
              in
              let qa = queue ~lease:(Span.s 3) ~jobs:[Job j] st in
              let qb =
                queue ~poll:(Span.ms 100) ~lease:(Span.s 3) ~jobs:[Job j] st
              in
              Fiber.scope (fun sw ->
                  D.start qa sw ;
                  ignore (D.enqueue qa j "x") ;
                  Time.sleep Mono (Span.ms 10) ;
                  D.start qb sw ;
                  D.stop ~grace:(Span.s 1) qa ;
                  D.drain qb ;
                  D.stop qb ) ;
              equal int 1 (Atomic.get peak) ;
              equal (list string) ["run 1"; "run 1"] (lines l) ;
              equal string "Stopped r0 ok0 re0 f0 x0 u0 a0 rel1 lost0 se0"
                (stats qa) ) )
    ; test "a crash during an extend is not a store error" (fun () ->
          S.run ~guard:long (fun _ ->
              let store_errors = log () in
              let f = Fake.make () in
              f.before <-
                (function "extend" -> Time.sleep Mono (Span.s 1) | _ -> ()) ;
              let st = Fake.store f (D.memory_store ()) in
              let j = text "a" (fun _ _ -> Time.sleep Mono (Span.s 100)) in
              let q = queue ~lease:(Span.s 3) ~store_errors ~jobs:[Job j] st in
              Fiber.scope (fun inner ->
                  D.start q inner ;
                  ignore (D.enqueue q j "x") ;
                  Time.sleep Mono (Span.ms 1500) ;
                  Fiber.cancel inner Exit ) ;
              equal (list string) [] (lines store_errors) ) )
    ; test "the store gives more than max: release, a store error, a backoff"
        (fun () ->
          S.run ~guard:long (fun _ ->
              let l = log () and store_errors = log () in
              let f = Fake.make () in
              f.extra <- 1 ;
              let st = Fake.store f (D.memory_store ()) in
              let j = text "a" (fun _ s -> add l s) in
              let q = queue ~workers:1 ~store_errors ~jobs:[Job j] st in
              ignore (D.enqueue q j "1") ;
              ignore (D.enqueue q j "2") ;
              run_queue q (fun _ -> D.drain q) ;
              equal (list string) ["1"; "2"] (lines l) ;
              equal (list string)
                [ "Invalid_argument(\"Caravan_jobs.Durable: the store gave \
                   more than max jobs\")" ]
                (lines store_errors) ) )
    ; test "an unknown name: the poller waits the backoff" (fun () ->
          S.run ~guard:long (fun _ ->
              let f = Fake.make () in
              f.rename <- (fun _ -> "zz") ;
              let st = Fake.store f (D.memory_store ()) in
              let j = text "a" (fun _ _ -> ()) in
              let q = queue ~store_errors:(log ()) ~jobs:[Job j] st in
              ignore (D.enqueue q j "x") ;
              run_queue q (fun _ -> Time.sleep Mono (Span.ms 2500)) ;
              (* At 0 and after about 1 s; the next is after about 3 s. *)
              equal int 2 (claims f) ) )
    ; test "on_store_error raises at a failure of claim: the queue fails"
        (fun () ->
          S.run (fun _ ->
              let f = Fake.make () in
              f.before <- (function "claim" -> raise Boom | _ -> ()) ;
              let st = Fake.store f (D.memory_store ()) in
              let q =
                D.create
                  ~on_store_error:(fun _ -> raise Exit)
                  ~jobs:[Job (text "a" (fun _ _ -> ()))]
                  st
              in
              raises Exit (fun () ->
                  Fiber.scope (fun sw ->
                      D.start q sw ;
                      Time.sleep Mono (Span.ms 1) ;
                      D.stop q ) ) ) )
    ; test "a cancel of the scope right after start is no failure" (fun () ->
          S.run (fun _ ->
              let q =
                queue ~jobs:[Job (text "a" (fun _ _ -> ()))] (D.memory_store ())
              in
              Fiber.scope (fun sw -> D.start q sw ; Fiber.cancel sw Exit) ;
              equal string "Stopped r0 ok0 re0 f0 x0 u0 a0 rel0 lost0 se0"
                (stats q) ) )
    ; test "a crash during a claim that does not stop: no release" (fun () ->
          S.run ~guard:long (fun _ ->
              let f = Fake.make () in
              (* A driver that blocks: the cancel does not cut it. *)
              f.before <-
                (function
                | "claim" ->
                    Fiber.shield (fun () -> Time.sleep Mono (Span.s 1))
                | _ ->
                    () ) ;
              let st = Fake.store f (D.memory_store ()) in
              let j = text "a" (fun _ _ -> ()) in
              let q = queue ~jobs:[Job j] st in
              ignore (D.enqueue q j "x") ;
              Fiber.scope (fun inner ->
                  D.start q inner ;
                  Time.sleep Mono (Span.ms 500) ;
                  Fiber.cancel inner Exit ) ;
              is_false ~msg:"no release" (List.mem "release 0" !(f.calls)) ;
              equal string "Stopped r0 ok0 re0 f0 x0 u0 a0 rel0 lost0 se0"
                (stats q) ) )
    ; test "a hung claim: the stop waits, and a crash ends it" (fun () ->
          S.manual (fun sim ->
              let f = Fake.make () in
              let never = Ivar.create () in
              f.before <-
                (function "claim" -> sync (Ivar.read never) | _ -> ()) ;
              let st = Fake.store f (D.memory_store ()) in
              let q = queue ~jobs:[Job (text "a" (fun _ _ -> ()))] st in
              Fiber.scope (fun sw ->
                  let scope = ref None in
                  let run =
                    S.call sw (fun () ->
                        Fiber.scope (fun inner ->
                            scope := Some inner ;
                            D.start q inner ) )
                  in
                  Weft_sim.settle sim ;
                  let c = S.call sw (fun () -> D.stop ~grace:(Span.s 1) q) in
                  S.advance sim (Span.s 5) ;
                  is_false ~msg:"the stop waits" (S.ended sim c) ;
                  Fiber.cancel (Option.get !scope) Exit ;
                  is_true ~msg:"the crash ended the queue" (S.ended sim run) ;
                  is_true ~msg:"the stop ended" (S.ended sim c) ;
                  S.await c ) ) )
    ; test "a bad claim whose release gives false: no lost lease" (fun () ->
          S.run (fun _ ->
              let f = Fake.make () in
              f.rename <- (fun _ -> "zz") ;
              f.result <- (function "release" -> Some false | _ -> None) ;
              let st = Fake.store f (D.memory_store ()) in
              let j = text "a" (fun _ _ -> ()) in
              let q = queue ~store_errors:(log ()) ~jobs:[Job j] st in
              ignore (D.enqueue q j "x") ;
              run_queue q (fun _ -> Time.sleep Mono (Span.ms 1)) ;
              equal string "Stopped r0 ok0 re0 f0 x0 u0 a0 rel0 lost0 se1"
                (stats q) ) )
    ; test "drain returns at a stop before the start" (fun () ->
          S.manual (fun sim ->
              let j = text "a" (fun _ _ -> ()) in
              let q =
                queue ~poll:(Span.s 3600) ~jobs:[Job j] (D.memory_store ())
              in
              ignore (D.enqueue q j "x") ;
              Fiber.scope (fun sw ->
                  let c = S.call sw (fun () -> D.drain q) in
                  is_false ~msg:"drain waits" (S.ended sim c) ;
                  D.stop q ;
                  is_true ~msg:"drain ended" (S.ended sim c) ;
                  S.await c ;
                  (* A second stop returns too. *)
                  D.stop q ) ) )
    ; test "drain returns when start fails" (fun () ->
          S.manual (fun sim ->
              let j = text "a" (fun _ _ -> ()) in
              let q =
                queue ~poll:(Span.s 3600) ~jobs:[Job j] (D.memory_store ())
              in
              ignore (D.enqueue q j "x") ;
              Fiber.scope (fun sw ->
                  let c = S.call sw (fun () -> D.drain q) in
                  is_false ~msg:"drain waits" (S.ended sim c) ;
                  let closed = Fiber.scope Fun.id in
                  raises Fiber.Scope_closed (fun () -> D.start q closed) ;
                  is_true ~msg:"drain ended" (S.ended sim c) ;
                  S.await c ) ) )
    ; test "a crash that cuts a claim is not a store error" (fun () ->
          S.run ~guard:long (fun _ ->
              let store_errors = log () in
              let f = Fake.make () in
              f.before <-
                (function "claim" -> Time.sleep Mono (Span.s 1) | _ -> ()) ;
              let st = Fake.store f (D.memory_store ()) in
              let q =
                queue ~store_errors ~jobs:[Job (text "a" (fun _ _ -> ()))] st
              in
              Fiber.scope (fun inner ->
                  D.start q inner ;
                  Time.sleep Mono (Span.ms 500) ;
                  Fiber.cancel inner Exit ) ;
              equal (list string) [] (lines store_errors) ;
              equal string "Stopped r0 ok0 re0 f0 x0 u0 a0 rel0 lost0 se0"
                (stats q) ) )
    ; test "after a hook raised in a job, a new claim goes back to the store"
        (fun () ->
          S.run ~guard:long (fun _ ->
              let f = Fake.make () in
              let mem = D.memory_store () in
              let st = Fake.store f mem in
              let slow =
                text "slow" (fun _ _ ->
                    Fiber.shield (fun () -> Time.sleep Mono (Span.s 30)) )
              in
              let boom =
                text "boom" ~retry:J.no_retry (fun _ _ -> raise Boom)
              in
              let late =
                text "late" ~retry:(J.retry ~attempts:1 ()) (fun _ _ -> ())
              in
              let q =
                D.create
                  ~on_error:(fun _ -> raise Exit)
                  ~jobs:[Job slow; Job boom; Job late]
                  st
              in
              raises Exit (fun () ->
                  Fiber.scope (fun sw ->
                      D.start q sw ;
                      ignore (D.enqueue q slow "x") ;
                      ignore (D.enqueue q boom "x") ;
                      Time.sleep Mono (Span.s 1) ;
                      let id = D.enqueue q late "x" in
                      Time.sleep Mono (Span.s 1) ;
                      is_true ~msg:"the late job is released"
                        (List.mem ("release " ^ id) !(f.calls)) ) ) ;
              let l = log () in
              let late =
                text "late" (fun (i : D.info) _ ->
                    add l (Printf.sprintf "late %d" i.attempt) )
              in
              let q2 = queue ~jobs:[Job late] mem in
              run_queue q2 (fun _ -> D.drain q2) ;
              equal (list string) ["late 1"] (lines l) ) )
    ; test "poll and lease: the upper limits" (fun () ->
          let st = D.memory_store () in
          let a = D.Job (text "a" (fun _ _ -> ())) in
          ignore
            (D.create ~poll:(Span.s 3600) ~lease:(Span.s 86_400) ~jobs:[a] st) ;
          invalid (fun () ->
              D.create ~poll:(Span.of_ns 3_600_000_000_001L) ~jobs:[a] st ) ;
          invalid (fun () ->
              D.create ~lease:(Span.of_ns 86_400_000_000_001L) ~jobs:[a] st ) )
    ]

(* Part (a) of J3: unique keys and ticks. *)
let show_id = function None -> "None" | Some id -> "Some " ^ id

let unique_tests =
  group "Unique keys and ticks"
    [ test "push_unique: a key of a name is held until the end of the job"
        (fun () ->
          with_store (fun st at set ->
              let pu = Option.get st.push_unique in
              let row name = ({name; payload= "p"; due= at 0} : D.row) in
              let a = Option.get (pu (row "a") ~key:"k") in
              equal string "None" (show_id (pu (row "a") ~key:"k")) ;
              (* Another name, or another key: a new job. *)
              is_true ~msg:"other name" (pu (row "b") ~key:"k" <> None) ;
              is_true ~msg:"other key" (pu (row "a") ~key:"k2" <> None) ;
              (* Held during the run and after a retry. *)
              let c = one (claim st 1) in
              equal string a c.id ;
              equal string "None" (show_id (pu (row "a") ~key:"k")) ;
              is_true ~msg:"retry" (st.retry ~id:a ~lease:c.lease ~due:(at 5)) ;
              equal string "None" (show_id (pu (row "a") ~key:"k")) ;
              set 5 ;
              let c = List.find (fun (c : D.claim) -> c.id = a) (claim st 5) in
              is_true ~msg:"complete" (st.complete ~id:a ~lease:c.lease) ;
              (* The key is free after the end. *)
              is_true ~msg:"free" (pu (row "a") ~key:"k" <> None) ) )
    ; test "push_tick: only a tick after the last tick of the entry" (fun () ->
          with_store (fun st at _ ->
              let pt = Option.get st.push_tick in
              let row = ({name= "a"; payload= "p"; due= at 0} : D.row) in
              let push entry tick = show_id (pt row ~entry ~tick:(at tick)) in
              equal string "Some 0" (push "e" 10) ;
              equal string "None" (push "e" 10) ;
              equal string "None" (push "e" 9) ;
              equal string "Some 1" (push "e" 11) ;
              equal string "Some 2" (push "f" 10) ;
              (* The tick holds also after the end of its job. *)
              let c = one (claim st 1) in
              is_true ~msg:"complete" (st.complete ~id:c.id ~lease:c.lease) ;
              equal string "None" (push "e" 11) ) )
    ; test "push_tick: a tick more than 5 min after the store time: None"
        (fun () ->
          with_store (fun st at _ ->
              let pt = Option.get st.push_tick in
              let row = ({name= "a"; payload= "p"; due= at 0} : D.row) in
              let push entry tick = show_id (pt row ~entry ~tick:(at tick)) in
              equal string "None" (push "e" 301) ;
              equal string "Some 0" (push "e" 300) ) )
    ; test "push_tick near the end of the range of the clock: no limit"
        (fun () ->
          S.manual (fun sim ->
              let last =
                Option.get
                  (Time.add Posix (Time.posix_of_unix_s 0.)
                     (Span.of_ns (Int64.sub Int64.max_int 1_000_000_000L)) )
              in
              Weft_sim.set_posix sim last ;
              let st = D.memory_store () in
              let pt = Option.get st.push_tick in
              let row = ({name= "a"; payload= "p"; due= last} : D.row) in
              equal string "Some 0" (show_id (pt row ~entry:"e" ~tick:last)) ) )
    ; test "push_unique: fail frees the key" (fun () ->
          with_store (fun st at _ ->
              let pu = Option.get st.push_unique in
              let row = ({name= "a"; payload= "p"; due= at 0} : D.row) in
              ignore (pu row ~key:"k") ;
              let c = one (claim st 1) in
              is_true ~msg:"fail" (st.fail ~id:c.id ~lease:c.lease ~error:"x") ;
              is_true ~msg:"free" (pu row ~key:"k" <> None) ) )
    ; test "enqueue_unique: Some, then None; a wake only if due" (fun () ->
          S.manual (fun sim ->
              let f = Fake.make () in
              let st = Fake.store f (D.memory_store ()) in
              let l = log () in
              let j = text "a" (fun _ s -> add l s) in
              let q = queue ~poll:(Span.s 3600) ~jobs:[Job j] st in
              Fiber.scope (fun sw ->
                  D.start q sw ;
                  Weft_sim.settle sim ;
                  let before = claims f in
                  is_true ~msg:"delayed"
                    ( D.enqueue_unique q ~delay:(Span.s 10) ~key:"d" j "d"
                    <> None ) ;
                  Weft_sim.settle sim ;
                  equal int before (claims f) ;
                  is_true ~msg:"first"
                    (D.enqueue_unique q ~key:"k" j "x" <> None) ;
                  equal string "None"
                    (show_id (D.enqueue_unique q ~key:"k" j "y")) ;
                  Weft_sim.settle sim ;
                  equal (list string) ["x"] (lines l) ;
                  (* After the end of the job, the key is free. *)
                  is_true ~msg:"again"
                    (D.enqueue_unique q ~key:"k" j "z" <> None) ;
                  Weft_sim.settle sim ;
                  equal (list string) ["x"; "z"] (lines l) ;
                  D.stop q ) ) )
    ; test "enqueue_tick: one job for each tick; a wake" (fun () ->
          S.manual (fun sim ->
              let st = D.memory_store () in
              let l = log () in
              let j = text "a" (fun _ s -> add l s) in
              let q = queue ~poll:(Span.s 3600) ~jobs:[Job j] st in
              Fiber.scope (fun sw ->
                  D.start q sw ;
                  Weft_sim.settle sim ;
                  let tick = Time.now Posix in
                  is_true ~msg:"first"
                    (D.enqueue_tick q ~entry:"e" ~tick j "x" <> None) ;
                  (* The push woke the poller. *)
                  Weft_sim.settle sim ;
                  equal (list string) ["x"] (lines l) ;
                  equal string "None"
                    (show_id (D.enqueue_tick q ~entry:"e" ~tick j "y")) ;
                  Weft_sim.settle sim ;
                  equal (list string) ["x"] (lines l) ;
                  D.stop q ) ) )
    ; test "a store with no push_unique or push_tick: Invalid_argument"
        (fun () ->
          S.run (fun _ ->
              let m = D.memory_store () in
              let st =
                D.store ~push:m.push ~claim:m.claim ~extend:m.extend
                  ~complete:m.complete ~retry:m.retry ~release:m.release
                  ~fail:m.fail ()
              in
              let j = text "a" (fun _ _ -> ()) in
              let q = queue ~jobs:[Job j] st in
              invalid (fun () -> D.enqueue_unique q ~key:"k" j "x") ;
              invalid (fun () ->
                  D.enqueue_tick q ~entry:"e" ~tick:(Time.now Posix) j "x" ) ) )
    ]

let () =
  exit
    (Windtrap.run "durable"
       [ store_tests
       ; queue_tests
       ; failure_tests
       ; lease_tests
       ; stop_tests
       ; store_failure_tests
       ; argument_tests
       ; race_tests
       ; server_tests
       ; detail_tests
       ; review_tests
       ; unique_tests ] )
