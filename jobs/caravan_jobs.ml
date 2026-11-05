open Weft

type reason =
  | Failed of exn * Printexc.raw_backtrace
  | Discarded of exn * Printexc.raw_backtrace
  | Lost of [`Not_started | `Running]

type failure = {name: string; id: int; attempts: int; reason: reason}

type retry = {attempts: int; base: Span.t; max: Span.t; on: exn -> bool}

let positive s = Span.compare s Span.zero > 0

let retry ?(attempts = 5) ?(base = Span.s 1) ?(max = Span.s 300)
    ?(on = fun _ -> true) () =
  if attempts < 1 then invalid_arg "Caravan_jobs.retry: attempts < 1" ;
  if not (positive base) then invalid_arg "Caravan_jobs.retry: base <= 0" ;
  if Span.compare max base < 0 then invalid_arg "Caravan_jobs.retry: max < base" ;
  {attempts; base; max; on}

let no_retry = retry ~attempts:1 ()

(* [backoff r ~id ~n] is the wait after the failure of attempt [n] of the job
   [id]: [d = min max (base * 2^(n-1))], less a jitter of 0 to 15 % of [d]. The
   doubling stops at [max]: no overflow. *)
let backoff r ~id ~n =
  let max = Span.to_ns r.max in
  let rec grow d k =
    if k = 0 then d
    else if Int64.compare d (Int64.div max 2L) > 0 then max
    else grow (Int64.mul d 2L) (k - 1)
  in
  let d = Int64.min max (grow (Span.to_ns r.base) (n - 1)) in
  let j = Hashtbl.hash (id, n) mod 16 in
  Span.of_ns (Int64.sub d (Int64.mul (Int64.div d 100L) (Int64.of_int j)))

type 'a job =
  { name: string
  ; retry: retry
  ; on_failure: 'a -> failure -> unit
  ; run: 'a -> unit }

let job ~name ?(retry = retry ()) ?(on_failure = fun _ _ -> ()) run =
  {name; retry; on_failure; run}

(* A job with its argument. *)
type item =
  | Item : {id: int; job: 'a job; arg: 'a; mutable attempts: int} -> item

(* The delayed jobs, in the order of their due time, then of their id. *)
module Due = Map.Make (struct
  type t = Clock.mono * int

  let compare (a, i) (b, j) =
    match Span.compare (Time.diff Mono a b) Span.zero with
    | 0 ->
        Int.compare i j
    | c ->
        c
end)

module Ids = Hashtbl.Make (Fiber.Id)

type state = [`Created | `Running | `Stopping | `Stopped]

type stats =
  { state: state
  ; waiting: int
  ; delayed: int
  ; running: int
  ; succeeded: int
  ; retried: int
  ; failed: int
  ; discarded: int
  ; lost: int }

(* The state is in [lock], a mutex of the stdlib: no effect and no code of the
   user runs while it is locked. Each permit of [permits] is a job in [waiting],
   or, after the stop, the end of one worker. The timer fiber waits for the
   first due time of [due], or for a fill of [wake]. *)
type t =
  { workers: int
  ; capacity: int
  ; on_error: failure -> unit
  ; lock: Mutex.t
  ; permits: Fiber.Semaphore.t
  ; waiting: item Queue.t
  ; mutable due: item Due.t
  ; mutable delayed: int
  ; mutable wake: unit Ivar.t
  ; mutable state: state
  ; mutable next_id: int
  ; mutable scope: Fiber.scope option
  ; mutable cancel: bool
  ; mutable drains: unit Ivar.t list
  ; worker_ids: unit Ids.t
  ; stopping: unit Ivar.t
  ; ended: unit Ivar.t
  ; mutable running: int
  ; mutable succeeded: int
  ; mutable retried: int
  ; mutable failed: int
  ; mutable discarded: int
  ; mutable lost: int }

let write_error f =
  let what =
    match f.reason with
    | Failed (e, _) ->
        "failed: " ^ Printexc.to_string e
    | Discarded (e, _) ->
        "discarded: " ^ Printexc.to_string e
    | Lost `Not_started ->
        "lost before its start"
    | Lost `Running ->
        "lost while it ran"
  in
  try
    Printf.eprintf "caravan.jobs: %s %d after %d attempts, %s\n%!" f.name f.id
      f.attempts what
  with Sys_error _ -> ()

let create ?(workers = 4) ?(capacity = 10_000) ?(on_error = write_error) () =
  if workers < 1 then invalid_arg "Caravan_jobs.create: workers < 1" ;
  if capacity < 1 then invalid_arg "Caravan_jobs.create: capacity < 1" ;
  { workers
  ; capacity
  ; on_error
  ; lock= Mutex.create ()
  ; permits= Fiber.Semaphore.create ~max:max_int 0
  ; waiting= Queue.create ()
  ; due= Due.empty
  ; delayed= 0
  ; wake= Ivar.create ()
  ; state= `Created
  ; next_id= 0
  ; scope= None
  ; cancel= false
  ; drains= []
  ; worker_ids= Ids.create 8
  ; stopping= Ivar.create ()
  ; ended= Ivar.create ()
  ; running= 0
  ; succeeded= 0
  ; retried= 0
  ; failed= 0
  ; discarded= 0
  ; lost= 0 }

let locked t f = Mutex.protect t.lock f

(* Under the lock: if no job waits, is delayed or runs, it takes the Ivars of
   the calls of [drain], for a fill after the unlock. *)
let take_drains_if_idle t =
  if Queue.is_empty t.waiting && t.delayed = 0 && t.running = 0 then (
    let d = t.drains in
    (t.drains <-
      [] )
    [@mutate
      off "a fill of a full Ivar does nothing: the clear only frees the list"] ;
    d )
  else []

let fill_all = List.iter (fun iv -> ignore (Ivar.try_fill iv ()))

let catch f =
  match f () with
  | () ->
      []
  | exception e ->
      [(e, Printexc.get_raw_backtrace ())]

(* The first exception only: the others are reported already, or are of the same
   hook. *)
let raise_first = function
  | [] ->
      ()
  | (e, bt) :: _ ->
      Printexc.raise_with_backtrace e bt

(* The two hooks of a failure. Each runs, also after a hook that raised. They
   run in a shield: a cancelled fiber reports too. *)
let report t (Item i) reason =
  let f = {name= i.job.name; id= i.id; attempts= i.attempts; reason} in
  Fiber.shield (fun () ->
      let a = catch (fun () -> i.job.on_failure i.arg f) in
      a @ catch (fun () -> t.on_error f) )

(* Under the lock: move the delayed jobs that are due to [waiting]. *)
let take_due t now =
  let rec go n =
    match Due.min_binding_opt t.due with
    | Some (((d, _) as k), item)
      when Span.compare (Time.diff Mono d now) Span.zero <= 0 ->
        t.due <- Due.remove k t.due ;
        t.delayed <- t.delayed - 1 ;
        Queue.push item t.waiting ;
        go (n + 1)
    | _ ->
        n
  in
  go 0

(* Under the lock: add a delayed job. The result is the Ivar that wakes the
   timer, for a fill after the unlock. Each new delayed job wakes it: the timer
   then waits for the new first due time. *)
let add_due t due (Item i as item) =
  t.due <- Due.add (due, i.id) item t.due ;
  t.delayed <- t.delayed + 1 ;
  t.wake

let release t n =
  for _ = 1 to n do
    Fiber.Semaphore.release t.permits
  done

(* The timer fiber: it moves the due jobs to [waiting], then waits for the next
   due time, a new delayed job, or the stop. After the stop, it ends: [stop]
   moved the jobs that were due. *)
let rec timer t =
  let now = Time.now Mono in
  let wake = Ivar.create () in
  match
    locked t (fun () ->
        if t.state <> `Running then None
        else (
          t.wake <- wake ;
          let n = take_due t now in
          Some (n, Option.map (fun ((d, _), _) -> d) (Due.min_binding_opt t.due))
          ) )
  with
  | None ->
      ()
  | Some (n, next) ->
      release t n ;
      let at = match next with None -> [] | Some d -> [Time.at Mono d] in
      sync (choose (Ivar.read wake :: Ivar.read t.stopping :: at)) ;
      timer t

let is_cancelled () =
  match Fiber.check_cancel () with
  | () ->
      false
  | exception Fiber.Cancelled _ ->
      true

(* One attempt of [item]. Then a success, a retry, or a report. The [on] of the
   policy and the due time of a retry are computed before the lock: [on] is code
   of the user. *)
let attempt t (Item i as item) =
  i.attempts <- i.attempts + 1 ;
  match i.job.run i.arg with
  | () ->
      fill_all
        (locked t (fun () ->
             t.running <- t.running - 1 ;
             t.succeeded <- t.succeeded + 1 ;
             take_drains_if_idle t ) )
  | exception e -> (
      let bt = Printexc.get_raw_backtrace () in
      let now = Time.now Mono in
      let policy = i.job.retry in
      let decision =
        if is_cancelled () then `Lost
        else
          match policy.on e with
          | false ->
              `Discard
          | true -> (
              if i.attempts >= policy.attempts then `Fail []
              else
                match
                  Time.add Mono now (backoff policy ~id:i.id ~n:i.attempts)
                with
                | Some due ->
                    `Retry due
                | None ->
                    (* The due time is after the end of the clock. *)
                    `Fail [] )
          | exception on_error ->
              `Fail [(on_error, Printexc.get_raw_backtrace ())]
      in
      let outcome, drains =
        locked t (fun () ->
            t.running <- t.running - 1 ;
            let outcome =
              match decision with
              | `Lost ->
                  t.lost <- t.lost + 1 ;
                  `Report (Lost `Running, [])
              | `Discard ->
                  t.discarded <- t.discarded + 1 ;
                  `Report (Discarded (e, bt), [])
              | `Retry due when t.state = `Running ->
                  t.retried <- t.retried + 1 ;
                  `Wake (add_due t due item)
              | `Retry _ ->
                  t.failed <- t.failed + 1 ;
                  `Report (Failed (e, bt), [])
              | `Fail hook ->
                  t.failed <- t.failed + 1 ;
                  `Report (Failed (e, bt), hook)
            in
            (outcome, take_drains_if_idle t) )
      in
      fill_all drains ;
      match outcome with
      | `Wake wake ->
          ignore (Ivar.try_fill wake ())
      | `Report (reason, hook) ->
          let errors = report t item reason in
          raise_first (hook @ errors) )

(* A worker takes the jobs in FIFO order. A permit with no job is the stop. A
   cancelled worker takes no job. A free permit needs no [sync]. *)
let rec work t =
  Fiber.check_cancel () ;
  if not (Fiber.Semaphore.try_acquire t.permits) then
    sync (Fiber.Semaphore.acquire t.permits) ;
  match
    locked t (fun () ->
        let item = Queue.take_opt t.waiting in
        if Option.is_some item then t.running <- t.running + 1 ;
        item )
  with
  | Some item ->
      attempt t item ; work t
  | None ->
      ()

let worker t () =
  let self = Fiber.self () in
  locked t (fun () -> Ids.replace t.worker_ids self ()) ;
  work t

(* Under the lock: the end of the queue. It takes each job that did not run, in
   O(1): the lists come after the unlock. *)
let take_all t =
  let waiting = Queue.create () in
  Queue.transfer t.waiting waiting ;
  let due = t.due in
  (t.due <-
    Due.empty )
  [@mutate off "nothing reads the map after the end: the clear frees the jobs"] ;
  t.lost <- t.lost + Queue.length waiting + t.delayed ;
  t.delayed <- 0 ;
  t.state <- `Stopped ;
  (waiting, due, take_drains_if_idle t)

(* Each job of [take_all] is lost: first the jobs that waited, then the delayed
   jobs, in the order of their due time. *)
let report_all t (waiting, due, drains) =
  let items =
    List.of_seq (Queue.to_seq waiting) @ List.map snd (Due.bindings due)
  in
  let errors =
    List.concat_map (fun item -> report t item (Lost `Not_started)) items
  in
  ignore (Ivar.try_fill t.ended ()) ;
  fill_all drains ;
  errors

let finish t = report_all t (locked t (fun () -> take_all t))

(* The fiber of the queue: the workers and the timer run in its scope. A cancel
   of this scope at the end of the grace cancels them only. *)
let run t () =
  let result =
    catch (fun () ->
        Fiber.scope (fun s ->
            let cancel =
              locked t (fun () ->
                  t.scope <- Some s ;
                  t.cancel )
            in
            if not cancel then
              try
                for _ = 1 to t.workers do
                  Fiber.fork ~name:"caravan.jobs.worker" s (worker t)
                done ;
                Fiber.fork ~name:"caravan.jobs.timer" s (fun () -> timer t)
              with Fiber.Scope_closed -> () ) )
  in
  raise_first (result @ finish t)

(* If the fork fails, the queue ends: a [stop] can wait for it at this time. The
   check of fiber code comes first: the reports need it. *)
let start t sw =
  ignore (Fiber.self ()) ;
  locked t (fun () ->
      if t.state <> `Created then
        invalid_arg "Caravan_jobs.start: the queue started or stopped" ;
      t.state <- `Running ) ;
  match Fiber.fork ~name:"caravan.jobs" sw (run t) with
  | () ->
      ()
  | exception e ->
      let bt = Printexc.get_raw_backtrace () in
      raise_first (finish t) ;
      Printexc.raise_with_backtrace e bt

let in_job t name =
  let self = Fiber.self () in
  if locked t (fun () -> Ids.mem t.worker_ids self) then
    invalid_arg (name ^ ": called in a job of the same queue")

(* [due_after line fn d] is the time now + [d] on [line]. [fn] names the
   function in the errors. *)
let due_after line fn d =
  if Span.compare d Span.zero < 0 then invalid_arg (fn ^ ": delay < 0") ;
  match Time.add line (Time.now line) d with
  | Some due ->
      due
  | None ->
      invalid_arg (fn ^ ": the delay is too long")

let enqueue t ?delay (j : 'a job) arg =
  let due = Option.map (due_after Mono "Caravan_jobs.enqueue") delay in
  let result =
    locked t (fun () ->
        if t.state = `Stopping || t.state = `Stopped then Error `Stopped
        else if Queue.length t.waiting + t.delayed >= t.capacity then
          Error `Full
        else
          let item = Item {id= t.next_id; job= j; arg; attempts= 0} in
          t.next_id <- t.next_id + 1 ;
          match due with
          | None ->
              Queue.push item t.waiting ; Ok None
          | Some due ->
              Ok (Some (add_due t due item)) )
  in
  match result with
  | Ok None ->
      Fiber.Semaphore.release t.permits ;
      Ok ()
  | Ok (Some wake) ->
      ignore (Ivar.try_fill wake ()) ;
      Ok ()
  | Error _ as e ->
      e

let stop ?(grace = Span.s 10) t =
  in_job t "Caravan_jobs.stop" ;
  let now = Time.now Mono in
  match
    locked t (fun () ->
        match t.state with
        | `Created ->
            `Not_started (take_all t)
        | `Running ->
            t.state <- `Stopping ;
            (* The jobs that are due run in the grace. *)
            `Stop (take_due t now)
        | `Stopping | `Stopped ->
            `Wait )
  with
  | `Not_started taken ->
      raise_first (report_all t taken)
  | `Stop due ->
      release t (due + t.workers) ;
      ignore (Ivar.try_fill t.stopping ()) ;
      sync (choose [Ivar.read t.ended; Time.after Mono grace]) ;
      (* The end of the grace, or the queue ended: then the cancel does nothing.
         Before the start of the scope, [cancel] tells the queue to run no
         job. *)
      ( match
          locked t (fun () ->
              t.cancel <- true ;
              t.scope )
        with
      | Some s ->
          Fiber.cancel s Exit
      | None ->
          () ) ;
      sync (Ivar.read t.ended)
  | `Wait ->
      sync (Ivar.read t.ended)

let drain t =
  in_job t "Caravan_jobs.drain" ;
  let iv = Ivar.create () in
  let wait =
    locked t (fun () ->
        if
          t.state = `Stopped
          || (Queue.is_empty t.waiting && t.delayed = 0 && t.running = 0)
        then false
        else (
          t.drains <- iv :: t.drains ;
          true ) )
  in
  if wait then sync (Ivar.read iv)

let service t =
  Caravan.Server.service ~start:(start t) ~stop:(fun ~grace -> stop ~grace t)

let stats t =
  locked t (fun () ->
      { state= t.state
      ; waiting= Queue.length t.waiting
      ; delayed= t.delayed
      ; running= t.running
      ; succeeded= t.succeeded
      ; retried= t.retried
      ; failed= t.failed
      ; discarded= t.discarded
      ; lost= t.lost } )

(* [before a b]: the POSIX time [a] is before [b]. *)
let before a b = Span.compare (Time.diff Posix a b) Span.zero < 0

module Durable = struct
  type reason =
    | Failed of exn * Printexc.raw_backtrace
    | Discarded of exn * Printexc.raw_backtrace
    | Undecodable of exn * Printexc.raw_backtrace
    | Abandoned

  type failure = {name: string; id: string; attempts: int; reason: reason}

  type info = {id: string; attempt: int}

  type 'a job =
    { name: string
    ; encode: 'a -> string
    ; decode: string -> 'a
    ; retry: retry
    ; on_failure: 'a -> failure -> unit
    ; run: info -> 'a -> unit }

  let job ~name ~encode ~decode ?(retry = retry ())
      ?(on_failure = fun _ _ -> ()) run =
    {name; encode; decode; retry; on_failure; run}

  type any = Job : 'a job -> any

  type row = {name: string; payload: string; due: Clock.posix}

  type claim =
    {id: string; name: string; payload: string; attempts: int; lease: string}

  type store =
    { push: row -> string
    ; claim: names:string list -> max:int -> lease:Span.t -> claim list
    ; extend: id:string -> lease:string -> Span.t -> bool
    ; complete: id:string -> lease:string -> bool
    ; retry: id:string -> lease:string -> due:Clock.posix -> bool
    ; release: id:string -> lease:string -> bool
    ; fail: id:string -> lease:string -> error:string -> bool
    ; next_due: (names:string list -> Clock.posix option) option
    ; push_unique: (row -> key:string -> string option) option
    ; push_tick:
        (row -> entry:string -> tick:Clock.posix -> string option) option }

  let store ~push ~claim ~extend ~complete ~retry ~release ~fail ?next_due
      ?push_unique ?push_tick () =
    { push
    ; claim
    ; extend
    ; complete
    ; retry
    ; release
    ; fail
    ; next_due
    ; push_unique
    ; push_tick }

  (* The memory store. [lease] is the token and the end of the lease of the last
     claim, until a [retry] or a [release]. *)
  type mrow =
    { mid: int
    ; mname: string
    ; mpayload: string
    ; mutable mdue: Clock.posix
    ; mutable mattempts: int
    ; mutable lease: (string * Clock.posix) option
    ; mkey: string option }

  (* The end of the live lease of [r], if it has one. *)
  let lease_end now r =
    match r.lease with
    | Some (_, until) when before now until ->
        Some until
    | _ ->
        None

  (* A tick of [push_tick] at most this time after the time of the store. *)
  let tick_tolerance = Span.s 300

  let memory_store () =
    let lock = Mutex.create () in
    let rows = Hashtbl.create 64 and next = ref 0 in
    let locked f = Mutex.protect lock f in
    (* [owned ~id ~lease f] runs [f] on the row of [id] if its token is
       [lease]. *)
    let owned ~id ~lease f =
      locked (fun () ->
          match Option.bind (int_of_string_opt id) (Hashtbl.find_opt rows) with
          | Some r when Option.map fst r.lease = Some lease ->
              f r ; true
          | _ ->
              false )
    in
    let add now span =
      match Time.add Posix now span with
      | Some t ->
          t
      | None ->
          invalid_arg "Caravan_jobs.Durable.memory_store: the lease is too long"
    in
    let ticks = Hashtbl.create 8 in
    (* Under the lock: a new job. *)
    let add_row ?key (row : row) =
      let i = !next in
      incr next ;
      Hashtbl.replace rows i
        { mid= i
        ; mname= row.name
        ; mpayload= row.payload
        ; mdue= row.due
        ; mattempts= 0
        ; lease= None
        ; mkey= key } ;
      string_of_int i
    in
    let push row = locked (fun () -> add_row row) in
    (* The keys of the jobs that exist: (name, key). *)
    let keys = Hashtbl.create 8 in
    let push_unique (row : row) ~key =
      locked (fun () ->
          if Hashtbl.mem keys (row.name, key) then None
          else (
            Hashtbl.replace keys (row.name, key) () ;
            Some (add_row ~key row) ) )
    in
    let push_tick row ~entry ~tick =
      (* After the end of the range of the clock: no limit. *)
      let limit = Time.add Posix (Time.now Posix) tick_tolerance in
      locked (fun () ->
          match Hashtbl.find_opt ticks entry with
          | _ when Option.fold ~none:false ~some:(fun l -> before l tick) limit
            ->
              (* A clock far ahead: no tick, else it blocks the entry. *)
              None
          | Some last when not (before last tick) ->
              None
          | _ ->
              Hashtbl.replace ticks entry tick ;
              Some (add_row row) )
    in
    let claim ~names ~max ~lease =
      let now = Time.now Posix in
      let until = add now lease in
      locked (fun () ->
          Hashtbl.fold
            (fun _ r acc ->
              if
                List.mem r.mname names
                && (not (before now r.mdue))
                && lease_end now r = None
              then r :: acc
              else acc )
            rows []
          |> List.sort (fun a b ->
              match Span.compare (Time.diff Posix a.mdue b.mdue) Span.zero with
              | 0 ->
                  Int.compare a.mid b.mid
              | c ->
                  c )
          |> List.take max
          |> List.map (fun r ->
              let token = string_of_int !next in
              incr next ;
              r.lease <- Some (token, until) ;
              r.mattempts <- r.mattempts + 1 ;
              { id= string_of_int r.mid
              ; name= r.mname
              ; payload= r.mpayload
              ; attempts= r.mattempts
              ; lease= token } ) )
    in
    let extend ~id ~lease span =
      let until = add (Time.now Posix) span in
      owned ~id ~lease (fun r -> r.lease <- Some (lease, until))
    in
    let remove ~id ~lease =
      owned ~id ~lease (fun r ->
          Hashtbl.remove rows r.mid ;
          Option.iter (fun k -> Hashtbl.remove keys (r.mname, k)) r.mkey )
    in
    let retry ~id ~lease ~due =
      owned ~id ~lease (fun r ->
          r.lease <- None ;
          r.mdue <- due )
    in
    let release ~id ~lease =
      owned ~id ~lease (fun r ->
          r.lease <- None ;
          r.mattempts <- r.mattempts - 1 )
    in
    let next_due ~names =
      let now = Time.now Posix in
      locked (fun () ->
          Hashtbl.fold
            (fun _ r acc ->
              if List.mem r.mname names then
                let t = Option.value (lease_end now r) ~default:r.mdue in
                match acc with Some d when before d t -> acc | _ -> Some t
              else acc )
            rows None )
    in
    store ~push ~claim ~extend ~complete:remove ~retry ~release
      ~fail:(fun ~id ~lease ~error:_ -> remove ~id ~lease)
      ~next_due ~push_unique ~push_tick ()

  (* A running job: its lease token, its fiber, the scope of its function, and
     [lost] if the extender found that another worker has it. *)
  type run =
    { token: string
    ; fiber: Fiber.Id.t
    ; mutable scope: Fiber.scope option
    ; mutable lost: bool }

  type stats =
    { state: state
    ; running: int
    ; succeeded: int
    ; retried: int
    ; failed: int
    ; discarded: int
    ; undecodable: int
    ; abandoned: int
    ; released: int
    ; lease_lost: int
    ; store_errors: int }

  (* The state is in [lock], a mutex of the stdlib: no effect and no code of the
     user runs while it is locked. [runs] has the leases that the extender
     extends. [active] counts the job fibers, also during their last store call.
     [seed] makes the jitters of each process different. *)
  type t =
    { store: store
    ; poll: Span.t
    ; lease: Span.t
    ; on_error: failure -> unit
    ; on_store_error: exn -> unit
    ; jobs: (string, any) Hashtbl.t
    ; names: string list
    ; lock: Mutex.t
    ; permits: Fiber.Semaphore.t
    ; runs: (string, run) Hashtbl.t
    ; mutable active: int
    ; mutable state: state
    ; mutable scope: Fiber.scope option
    ; mutable cancel: bool
    ; mutable wake: unit Ivar.t
    ; mutable progress: unit Ivar.t
    ; mutable seed: int
    ; stopping: unit Ivar.t
    ; ended: unit Ivar.t
    ; mutable succeeded: int
    ; mutable retried: int
    ; mutable failed: int
    ; mutable discarded: int
    ; mutable undecodable: int
    ; mutable abandoned: int
    ; mutable released: int
    ; mutable lease_lost: int
    ; mutable store_errors: int }

  let write_error (f : failure) =
    let what =
      match f.reason with
      | Failed (e, _) ->
          "failed: " ^ Printexc.to_string e
      | Discarded (e, _) ->
          "discarded: " ^ Printexc.to_string e
      | Undecodable (e, _) ->
          "undecodable: " ^ Printexc.to_string e
      | Abandoned ->
          "abandoned: its attempts ended with no result"
    in
    try
      Printf.eprintf "caravan.jobs: %s %s after %d attempts, %s\n%!" f.name f.id
        f.attempts what
    with Sys_error _ -> ()

  let write_store_error e =
    try Printf.eprintf "caravan.jobs: store: %s\n%!" (Printexc.to_string e)
    with Sys_error _ -> ()

  let create ?(workers = 4) ?(poll = Span.s 1) ?(lease = Span.s 60)
      ?(on_error = write_error) ?(on_store_error = write_store_error) ~jobs
      store =
    ( if workers < 1 then invalid_arg "Caravan_jobs.Durable.create: workers < 1"
    )
    [@mutate
      off
        "Fiber.Semaphore.create raises the same exception: the check gives a \
         clear text"] ;
    if (not (positive poll)) || Span.compare poll (Span.s 3600) > 0 then
      invalid_arg "Caravan_jobs.Durable.create: poll is not in (0, 1 h]" ;
    if
      Span.compare lease (Span.s 1) < 0
      || Span.compare lease (Span.s 86_400) > 0
    then invalid_arg "Caravan_jobs.Durable.create: lease is not in [1 s, 1 day]" ;
    if jobs = [] then invalid_arg "Caravan_jobs.Durable.create: no job type" ;
    let table = Hashtbl.create 8 in
    List.iter
      (fun (Job j as any) ->
        if Hashtbl.mem table j.name then
          invalid_arg ("Caravan_jobs.Durable.create: two job types " ^ j.name) ;
        Hashtbl.replace table j.name any )
      jobs ;
    { store
    ; poll
    ; lease
    ; on_error
    ; on_store_error
    ; jobs= table
    ; names= List.map (fun (Job j) -> j.name) jobs
    ; lock= Mutex.create ()
    ; permits= Fiber.Semaphore.create workers
    ; runs= Hashtbl.create 8
    ; active= 0
    ; state= `Created
    ; scope= None
    ; cancel= false
    ; wake= Ivar.create ()
    ; progress= Ivar.create ()
    ; seed= (0 [@mutate off "start sets the seed before its first use"])
    ; stopping= Ivar.create ()
    ; ended= Ivar.create ()
    ; succeeded= 0
    ; retried= 0
    ; failed= 0
    ; discarded= 0
    ; undecodable= 0
    ; abandoned= 0
    ; released= 0
    ; lease_lost= 0
    ; store_errors= 0 }

  let locked t f = Mutex.protect t.lock f

  (* Under the lock: a new Ivar for the next progress, and the old one for a
     fill after the unlock. *)
  let take_progress t =
    let p = t.progress in
    t.progress <- Ivar.create () ;
    p

  let fill iv = ignore (Ivar.try_fill iv ())

  let release_permits t n =
    for _ = 1 to n do
      Fiber.Semaphore.release t.permits
    done

  type count = Succeeded | Retried | Released | Lost | Ended of reason

  let bump t c =
    locked t (fun () ->
        match c with
        | Succeeded ->
            t.succeeded <- t.succeeded + 1
        | Retried ->
            t.retried <- t.retried + 1
        | Released ->
            t.released <- t.released + 1
        | Lost ->
            t.lease_lost <- t.lease_lost + 1
        | Ended (Failed _) ->
            t.failed <- t.failed + 1
        | Ended (Discarded _) ->
            t.discarded <- t.discarded + 1
        | Ended (Undecodable _) ->
            t.undecodable <- t.undecodable + 1
        | Ended Abandoned ->
            t.abandoned <- t.abandoned + 1 )

  let store_error t e =
    locked t (fun () -> t.store_errors <- t.store_errors + 1) ;
    Fiber.shield (fun () -> catch (fun () -> t.on_store_error e))

  (* [settle t f ~ok] is a store call [f] on a claimed job, in a shield: a
     cancel does not cut it. If [f] gives [true], [ok ()]. If it gives [false],
     another worker has the job. A failure goes to [on_store_error]: the lease
     of the job ends, and the job runs again. The result is the errors of the
     hooks. *)
  let settle t f ~ok =
    match Fiber.shield f with
    | true ->
        ok ()
    | false ->
        bump t Lost ; []
    | exception e ->
        store_error t e

  let counted t f c = settle t f ~ok:(fun () -> bump t c ; [])

  let release_claim t (c : claim) =
    counted t (fun () -> t.store.release ~id:c.id ~lease:c.lease) Released

  let report t (f : failure) on_failure =
    Fiber.shield (fun () ->
        let a = catch on_failure in
        a @ catch (fun () -> t.on_error f) )

  (* The end of [c] with [reason]: [fail], then the reports. [hook] is an
     exception of [on], to raise after them. *)
  let end_job t (c : claim) reason ~error ~hook ~on_failure =
    hook
    @ settle t
        (fun () -> t.store.fail ~id:c.id ~lease:c.lease ~error)
        ~ok:(fun () ->
          bump t (Ended reason) ;
          let f = {name= c.name; id= c.id; attempts= c.attempts; reason} in
          report t f (fun () -> on_failure f) )

  (* A failure of the attempt of [c]: a retry, or the end with [last]. *)
  let failed t (c : claim) policy e bt ~last ~on_failure =
    let decision =
      match policy.on e with
      | false ->
          `End (Discarded (e, bt), [])
      | true -> (
          let time =
            if c.attempts >= policy.attempts then None
            else
              Time.add Posix (Time.now Posix)
                (backoff policy ~id:(Hashtbl.hash c.id) ~n:c.attempts)
          in
          match time with
          | Some due ->
              `Retry due
          | None ->
              (* The last attempt, or a due time after the end of the clock. *)
              `End (last (e, bt), []) )
      | exception hook ->
          `End (last (e, bt), [(hook, Printexc.get_raw_backtrace ())])
    in
    match decision with
    | `Retry due ->
        counted t (fun () -> t.store.retry ~id:c.id ~lease:c.lease ~due) Retried
    | `End (reason, hook) ->
        end_job t c reason ~error:(Printexc.to_string e) ~hook ~on_failure

  exception Lease_lost

  exception Grace_ended

  (* The fiber of one claim. It holds one permit. *)
  let run_claim t (c : claim) (Job j) () =
    locked t (fun () -> t.active <- t.active + 1) ;
    let errors =
      Fun.protect
        ~finally:(fun () ->
          let p =
            locked t (fun () ->
                t.active <- t.active - 1 ;
                take_progress t )
          in
          Fiber.Semaphore.release t.permits ;
          fill p )
        (fun () ->
          if c.attempts > j.retry.attempts then
            end_job t c Abandoned ~error:"abandoned" ~hook:[] ~on_failure:ignore
          else
            let r =
              {token= c.lease; fiber= Fiber.self (); scope= None; lost= false}
            in
            locked t (fun () -> Hashtbl.replace t.runs c.id r) ;
            (* The extender stops before the last store call. *)
            let leave () =
              locked t (fun () ->
                  match Hashtbl.find_opt t.runs c.id with
                  | Some r' when r' == r ->
                      Hashtbl.remove t.runs c.id
                  | _ ->
                      () )
            in
            match j.decode c.payload with
            | exception e ->
                let bt = Printexc.get_raw_backtrace () in
                leave () ;
                failed t c j.retry e bt
                  ~last:(fun (e, bt) -> Undecodable (e, bt))
                  ~on_failure:ignore
            | arg -> (
                let outcome =
                  match
                    Fiber.scope (fun s ->
                        if
                          locked t (fun () ->
                              r.scope <- Some s ;
                              r.lost )
                        then raise Lease_lost ;
                        j.run {id= c.id; attempt= c.attempts} arg )
                  with
                  | () ->
                      `Done
                  | exception e ->
                      `Raised (e, Printexc.get_raw_backtrace ())
                in
                leave () ;
                if r.lost then (bump t Lost ; [])
                else
                  match outcome with
                  | `Done ->
                      counted t
                        (fun () -> t.store.complete ~id:c.id ~lease:c.lease)
                        Succeeded
                  | `Raised _ when is_cancelled () ->
                      (* The end of the grace: give the job back. A crash: no
                         call, the lease ends. *)
                      if locked t (fun () -> t.cancel) then release_claim t c
                      else []
                  | `Raised (e, bt) ->
                      failed t c j.retry e bt
                        ~last:(fun (e, bt) -> Failed (e, bt))
                        ~on_failure:(fun f -> j.on_failure arg f) ) )
    in
    raise_first errors

  (* The wait of the poller after a claim that did not fill its permits: [poll]
     and a jitter of 0 to 10 %. *)
  let jittered t polls =
    let d = Span.to_ns t.poll in
    Span.of_ns
      (Int64.add d
         (Int64.mul (Int64.div d 100L)
            (Int64.of_int (Hashtbl.hash (t.seed, polls) mod 11)) ) )

  (* The wait after the failure [k] of [claim] (k = 1, 2, ...): the backoff of a
     retry, with [poll] as its base and 1 min as its max. *)
  let store_backoff t k =
    backoff {(retry ()) with base= t.poll; max= Span.s 60} ~id:t.seed ~n:k

  (* [check t n claims] splits the result of a claim of [max = n]: the claims to
     run, and the claims that the store must not give (a name that this queue
     does not know, or more than [n]). *)
  let check t n claims =
    let rec go i ok bad = function
      | [] ->
          (List.rev ok, List.rev bad)
      | (c : claim) :: rest -> (
        match Hashtbl.find_opt t.jobs c.name with
        | Some j when i < n ->
            go (i + 1) ((c, j) :: ok) bad rest
        | Some _ ->
            go i ok ((c, "more than max jobs") :: bad) rest
        | None ->
            go i ok ((c, "the job type " ^ c.name) :: bad) rest )
    in
    go 0 [] [] claims

  (* The poller: it claims for the free permits, and forks a fiber for each
     claim. [k] counts the failures of the store in a row, and [polls] the
     waits. *)
  let rec poller t s ~k ~polls =
    sync (choose [Fiber.Semaphore.acquire t.permits; Ivar.read t.stopping]) ;
    (* After the stop, no claim. If the [choose] took a permit, the queue does
       not need it: it stops. *)
    if Option.is_none (Ivar.peek t.stopping) then (
      (* The semaphore has [workers] permits: no more. *)
      let rec more n =
        if Fiber.Semaphore.try_acquire t.permits then more (n + 1) else n
      in
      let n = more 1 in
      let wake = Ivar.create () in
      locked t (fun () -> t.wake <- wake) ;
      let wait events d =
        sync (choose (Time.after Mono d :: Ivar.read t.stopping :: events))
      in
      let failure errors =
        raise_first errors ;
        (* A failing store: a wake does not end this wait. *)
        wait [] (store_backoff t (k + 1)) ;
        poller t s ~k:(k + 1) ~polls
      in
      (* The poller is not in the scope [s] of the jobs: the cancel of the end
         of the grace does not cut its claim. A crash cuts it. *)
      match t.store.claim ~names:t.names ~max:n ~lease:t.lease with
      | exception (Fiber.Cancelled _ as e) when is_cancelled () ->
          Printexc.raise_with_backtrace e (Printexc.get_raw_backtrace ())
      | exception e ->
          release_permits t n ;
          failure (store_error t e)
      | claims -> (
          let ok, bad = check t n claims in
          let errors =
            List.concat_map
              (fun ((c : claim), what) ->
                (* A [false] is no lost lease: the store is wrong. *)
                ( match
                    Fiber.shield (fun () ->
                        t.store.release ~id:c.id ~lease:c.lease )
                  with
                  | _ ->
                      []
                  | exception e ->
                      store_error t e )
                @ store_error t
                    (Invalid_argument
                       ("Caravan_jobs.Durable: the store gave " ^ what) ) )
              bad
          in
          (* The scope [s] ends at the end of the grace, or at a failure of a
             job fiber (a hook raised): give the jobs back, and the poller ends.
             At a crash, the leases end. *)
          let rec fork started = function
            | [] ->
                (started, [])
            | ((c : claim), j) :: rest -> (
              match
                Fiber.fork ~name:"caravan.jobs.durable.job" s (run_claim t c j)
              with
              | () ->
                  fork (started + 1) rest
              | exception Fiber.Scope_closed ->
                  ( started
                  , if locked t (fun () -> t.cancel) || not (is_cancelled ())
                    then
                      List.concat_map
                        (fun (c, _) -> release_claim t c)
                        ((c, j) :: rest)
                    else [] ) )
          in
          let started, released = fork 0 ok in
          release_permits t (n - started) ;
          match (bad, released) with
          | _ :: _, _ ->
              failure (errors @ released)
          | [], _ :: _ ->
              raise_first released
          | [], [] when started < List.length ok ->
              ()
          | [], [] ->
              if started < n then wait [Ivar.read wake] (jittered t polls) ;
              poller t s ~k:0 ~polls:(polls + 1) ) )

  (* The extender: each [lease / 3], it extends the lease of each running
     job. *)
  let rec extender t =
    Time.sleep Mono (Span.of_ns (Int64.div (Span.to_ns t.lease) 3L)) ;
    let runs =
      locked t (fun () ->
          Hashtbl.fold (fun id r acc -> (id, r) :: acc) t.runs [] )
    in
    List.iter
      (fun (id, r) ->
        match t.store.extend ~id ~lease:r.token t.lease with
        | true ->
            ()
        | false ->
            let scope =
              locked t (fun () ->
                  match Hashtbl.find_opt t.runs id with
                  | Some r' when r' == r ->
                      r.lost <- true ;
                      r.scope
                  | _ ->
                      None )
            in
            Option.iter (fun s -> Fiber.cancel s Lease_lost) scope
        | exception (Fiber.Cancelled _ as e) when is_cancelled () ->
            Printexc.raise_with_backtrace e (Printexc.get_raw_backtrace ())
        | exception e ->
            raise_first (store_error t e) )
      runs ;
    extender t

  (* The fiber of the queue. The jobs and the keeper run in the scope [s]: the
     end of the grace cancels it. The poller and the extender run in [outer]:
     the claim in progress ends, and the extender extends the jobs that ignore
     that cancel, until they end. *)
  let run t () =
    let result =
      catch (fun () ->
          Fiber.scope (fun outer ->
              Fiber.scope (fun s ->
                  let cancel =
                    locked t (fun () ->
                        t.scope <- Some s ;
                        t.cancel )
                  in
                  if not cancel then
                    try
                      let polled = Ivar.create () in
                      Fiber.fork_daemon ~name:"caravan.jobs.durable.extender"
                        outer (fun () -> extender t ) ;
                      Fiber.fork ~name:"caravan.jobs.durable.poller" outer
                        (fun () ->
                          Fun.protect
                            ~finally:(fun () -> fill polled)
                            (fun () -> poller t s ~k:0 ~polls:0) ) ;
                      (* It keeps [s] open while the poller runs. *)
                      Fiber.fork ~name:"caravan.jobs.durable.keeper" s
                        (fun () -> sync (Ivar.read polled) )
                    with Fiber.Scope_closed -> () ) ) )
    in
    let p =
      locked t (fun () ->
          t.state <- `Stopped ;
          take_progress t )
    in
    fill t.ended ; fill p ; raise_first result

  let start t sw =
    let seed = Hashtbl.hash (Time.now Mono) in
    locked t (fun () ->
        if t.state <> `Created then
          invalid_arg "Caravan_jobs.Durable.start: the queue started or stopped" ;
        t.seed <- seed ;
        t.state <- `Running ) ;
    match Fiber.fork ~name:"caravan.jobs.durable" sw (run t) with
    | () ->
        ()
    | exception e ->
        let bt = Printexc.get_raw_backtrace () in
        let p =
          locked t (fun () ->
              t.state <- `Stopped ;
              take_progress t )
        in
        fill t.ended ;
        fill p ;
        Printexc.raise_with_backtrace e bt

  let in_job t name =
    let self = Fiber.self () in
    if
      locked t (fun () ->
          Hashtbl.fold
            (fun _ r acc -> acc || Fiber.Id.equal r.fiber self)
            t.runs false )
    then invalid_arg (name ^ ": called in a job of the same queue")

  let row ?delay (j : 'a job) arg =
    let payload = j.encode arg in
    let due =
      match delay with
      | None ->
          Time.now Posix
      | Some d ->
          due_after Posix "Caravan_jobs.Durable.row" d
    in
    ({name= j.name; payload; due} : row)

  (* A change of the store: [drain] looks at it again. *)
  let progress t = fill (locked t (fun () -> take_progress t))

  let wake t =
    fill (locked t (fun () -> t.wake)) ;
    progress t

  (* A new job: a wake of the poller if it is due now. *)
  let pushed t delay =
    match delay with
    | Some d when positive d ->
        (* The job is not due: a claim now gets nothing. *)
        progress t
    | _ ->
        wake t

  let enqueue t ?delay j arg =
    let id = t.store.push (row ?delay j arg) in
    pushed t delay ; id

  (* [need fn field f]: the optional function [f] of the store, for [fn]. *)
  let need fn field = function
    | Some f ->
        f
    | None ->
        invalid_arg (fn ^ ": the store has no " ^ field)

  let enqueue_unique t ?delay ~key j arg =
    let push =
      need "Caravan_jobs.Durable.enqueue_unique" "push_unique"
        t.store.push_unique
    in
    let id = push (row ?delay j arg) ~key in
    if Option.is_some id then pushed t delay ;
    id

  let enqueue_tick t ~entry ~tick j arg =
    let push =
      need "Caravan_jobs.Durable.enqueue_tick" "push_tick" t.store.push_tick
    in
    let id = push (row j arg) ~entry ~tick in
    if Option.is_some id then wake t ;
    id

  let stop ?(grace = Span.s 10) t =
    in_job t "Caravan_jobs.Durable.stop" ;
    match
      locked t (fun () ->
          match t.state with
          | `Created ->
              t.state <- `Stopped ;
              `Ended (take_progress t)
          | `Running ->
              t.state <- `Stopping ;
              `Stop
          | `Stopping | `Stopped ->
              `Wait )
    with
    | `Ended p ->
        fill t.ended ; fill p
    | `Stop ->
        fill t.stopping ;
        sync (choose [Ivar.read t.ended; Time.after Mono grace]) ;
        ( match
            locked t (fun () ->
                t.cancel <- true ;
                t.scope )
          with
        | Some s ->
            Fiber.cancel s Grace_ended
        | None ->
            () ) ;
        sync (Ivar.read t.ended)
    | `Wait ->
        sync (Ivar.read t.ended)

  let drain t =
    in_job t "Caravan_jobs.Durable.drain" ;
    let next_due =
      need "Caravan_jobs.Durable.drain" "next_due" t.store.next_due
    in
    let rec go () =
      (* The store first: a job that ends after this read is still active, or
         its end fills [progress]. *)
      let next = next_due ~names:t.names in
      let progress, active, state =
        locked t (fun () -> (t.progress, t.active, t.state))
      in
      if state <> `Stopped then
        let now = Time.now Posix in
        match (active, next) with
        | 0, None ->
            ()
        | _, next ->
            let at =
              match next with
              | Some d when before now d ->
                  [Time.at Posix d]
              | Some _ ->
                  (* A job is due: the poller must claim it now. *)
                  fill (locked t (fun () -> t.wake)) ;
                  []
              | None ->
                  []
            in
            (* Another queue can run the jobs: look again after [poll]. *)
            sync (choose (Ivar.read progress :: Time.after Mono t.poll :: at)) ;
            go ()
    in
    go ()

  let service t =
    Caravan.Server.service ~start:(start t) ~stop:(fun ~grace -> stop ~grace t)

  let stats t =
    locked t (fun () ->
        { state= t.state
        ; running= t.active
        ; succeeded= t.succeeded
        ; retried= t.retried
        ; failed= t.failed
        ; discarded= t.discarded
        ; undecodable= t.undecodable
        ; abandoned= t.abandoned
        ; released= t.released
        ; lease_lost= t.lease_lost
        ; store_errors= t.store_errors } )
end

module Schedule = struct
  (* The values of a field of a cron expression. [star] is the Vixie rule: the
     text of the field starts with "*". *)
  type field = {values: bool array; star: bool}

  type cron = {minute: field; hour: field; dom: field; month: field; dow: field}

  type t = Every of int64 | Cron of cron

  let ns_per_s = 1_000_000_000L

  let epoch = Time.posix_of_unix_s 0.

  let ns_of t = Span.to_ns (Time.diff Posix t epoch)

  let of_ns n = Time.add Posix epoch (Span.of_ns n)

  (* The floor of [a / b], for [b > 0]. *)
  let floor_div a b =
    let q = Int64.div a b in
    if Int64.compare (Int64.rem a b) 0L < 0 then Int64.pred q else q

  let every span =
    let d = Span.to_ns span in
    if
      Int64.compare d ns_per_s < 0
      || Int64.compare d (Int64.mul 86_400L ns_per_s) > 0
      || Int64.rem d ns_per_s <> 0L
    then
      invalid_arg
        "Caravan_jobs.Schedule.every: the span is not whole seconds from 1 s \
         to 1 day" ;
    Every d

  let months =
    [ "jan"
    ; "feb"
    ; "mar"
    ; "apr"
    ; "may"
    ; "jun"
    ; "jul"
    ; "aug"
    ; "sep"
    ; "oct"
    ; "nov"
    ; "dec" ]

  let days = ["sun"; "mon"; "tue"; "wed"; "thu"; "fri"; "sat"]

  let is_digit c = c >= '0' && c <= '9'

  (* [digits text] is the value of [text], digits only. *)
  let digits text =
    if text <> "" && String.for_all is_digit text then
      (* At most 9 digits: no overflow. *)
      if String.length text > 9 then Error (text ^ " is too large")
      else Ok (int_of_string text)
    else Error (Printf.sprintf "%S is not a number" text)

  (* [number names first text] is the value of [text]: digits, or a name of
     [names] (any case; the first name is [first]). *)
  let number names first text =
    match List.find_index (( = ) (String.lowercase_ascii text)) names with
    | Some i ->
        Ok (first + i)
    | None ->
        digits text

  (* [cut c s]: the text of [s] before the first [c], and the rest after it. *)
  let cut c s =
    match String.index_opt s c with
    | None ->
        (s, None)
    | Some i ->
        (String.sub s 0 i, Some (String.sub s (i + 1) (String.length s - i - 1)))

  let ( let* ) = Result.bind

  (* [field ~name ~lo ~hi ~names text]: the values of one field. For the day of
     the week, [hi] is 7, and 7 is Sunday (0). *)
  let field ~name ~lo ~hi ~names text =
    let values = Array.make (hi + 1) false in
    let error fmt = Printf.ksprintf (fun m -> Error (name ^ ": " ^ m)) fmt in
    let value s =
      match number names lo s with
      | Ok n when n >= lo && n <= hi ->
          Ok n
      | Ok n ->
          error "%d is out of the range %d-%d" n lo hi
      | Error m ->
          error "%s" m
    in
    let item s =
      let base, step = cut '/' s in
      let* first, last =
        if base = "*" then Ok (lo, hi)
        else
          match cut '-' base with
          | a, Some b ->
              let* a = value a in
              let* b = value b in
              if a > b then error "the range %s goes down" base else Ok (a, b)
          | _, None -> (
              let* a = value base in
              match step with
              | Some _ ->
                  error "a step needs * or a range: %s" s
              | None ->
                  Ok (a, a) )
      in
      let* step =
        match step with
        | None ->
            Ok 1
        | Some n -> (
          match digits n with
          | Ok n when n >= 1 ->
              Ok n
          | Ok _ ->
              error "the step of %s is zero" s
          | Error m ->
              error "%s" m )
      in
      let rec mark v =
        if v <= last then (
          values.(v) <- true ;
          mark (v + step) )
      in
      mark first ; Ok ()
    in
    let rec items = function
      | [] ->
          Ok ()
      | "" :: _ ->
          error "an empty item in %S" text
      | s :: rest ->
          let* () = item s in
          items rest
    in
    let* () = items (String.split_on_char ',' text) in
    Ok {values; star= String.starts_with ~prefix:"*" text}

  let macros =
    [ ("@yearly", "0 0 1 1 *")
    ; ("@annually", "0 0 1 1 *")
    ; ("@monthly", "0 0 1 * *")
    ; ("@weekly", "0 0 * * 0")
    ; ("@daily", "0 0 * * *")
    ; ("@midnight", "0 0 * * *")
    ; ("@hourly", "0 * * * *") ]

  (* The last day of each month, with 29 days for February. *)
  let month_days = [|0; 31; 29; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31|]

  (* The day of the month and the day of the week: the Vixie rule. *)
  let day_matches c ~day ~weekday =
    if c.dom.star || c.dow.star then
      c.dom.values.(day) && c.dow.values.(weekday)
    else c.dom.values.(day) || c.dow.values.(weekday)

  let of_string text =
    let text = String.trim text in
    let text =
      Option.value
        (List.assoc_opt (String.lowercase_ascii text) macros)
        ~default:text
    in
    let fields =
      String.map (function '\t' -> ' ' | c -> c) text
      |> String.split_on_char ' '
      |> List.filter (( <> ) "")
    in
    match fields with
    | [minute; hour; dom; month; dow] ->
        let* minute = field ~name:"minute" ~lo:0 ~hi:59 ~names:[] minute in
        let* hour = field ~name:"hour" ~lo:0 ~hi:23 ~names:[] hour in
        let* dom = field ~name:"day of the month" ~lo:1 ~hi:31 ~names:[] dom in
        let* month = field ~name:"month" ~lo:1 ~hi:12 ~names:months month in
        let* dow = field ~name:"day of the week" ~lo:0 ~hi:7 ~names:days dow in
        if dow.values.(7) then dow.values.(0) <- true ;
        let c = {minute; hour; dom; month; dow} in
        (* With the AND rule and a restricted day of the month, a date must
           exist: some month has the day (February with 29 days). *)
        let possible =
          c.dom.star || (not c.dow.star)
          || List.exists
               (fun m ->
                 c.month.values.(m)
                 && List.exists
                      (fun d -> c.dom.values.(d))
                      (List.init month_days.(m) (fun i -> i + 1)) )
               (List.init 12 (fun i -> i + 1))
        in
        if possible then Ok (Cron c) else Error "the expression matches no date"
    | _ ->
        Error
          (Printf.sprintf "%S: a cron expression has 5 fields, or is a macro"
             text )

  let cron text =
    match of_string text with
    | Ok t ->
        t
    | Error m ->
        invalid_arg ("Caravan_jobs.Schedule.cron: " ^ m)

  let ns_per_min = Int64.mul 60L ns_per_s

  (* The first minute of a day that matches, from [h:m]. *)
  let rec in_day c h m =
    if h > 23 then None
    else if m > 59 then in_day c (h + 1) 0
    else if c.hour.values.(h) && c.minute.values.(m) then Some (h, m)
    else if c.hour.values.(h) then in_day c h (m + 1)
    else in_day c (h + 1) 0

  (* [later p d]: [p] + [d] ns, or [None] after the end of the range. *)
  let later p d = Time.add Posix p (Span.of_ns d)

  (* The first multiple of [d] strictly after [after], or [None] after the end
     of the range. [k * d] is after [after]: no underflow. *)
  let next_multiple d after =
    let k = Int64.succ (floor_div (ns_of after) d) in
    if Int64.compare k (Int64.div Int64.max_int d) > 0 then None
    else of_ns (Int64.mul k d)

  let next t after =
    match t with
    | Every d ->
        next_multiple d after
    | Cron c ->
        (* The minute after [after], then day by day from midnight. Each [p] is
           at the start of a minute. A day has 1440 minutes: POSIX has no leap
           second. *)
        let rec day p =
          let u = Time.to_utc p in
          let now = (u.hour * 60) + u.minute in
          let at k = later p (Int64.mul (Int64.of_int (k - now)) ns_per_min) in
          let next_day () = Option.bind (at 1440) day in
          if
            c.month.values.(u.month)
            && day_matches c ~day:u.day ~weekday:u.weekday
          then
            match in_day c u.hour u.minute with
            | Some (h, m) ->
                at ((h * 60) + m)
            | None ->
                next_day ()
          else next_day ()
        in
        Option.bind (next_multiple ns_per_min after) day
end

exception Not_enqueued of [`Full | `Stopped]

module Recurring = struct
  type entry = {name: string; schedule: Schedule.t; action: Clock.posix -> unit}

  let entry ~name schedule action = {name; schedule; action}

  let job ~name schedule q j arg =
    entry ~name schedule (fun tick ->
        match enqueue q j (arg tick) with
        | Ok () ->
            ()
        | Error e ->
            raise (Not_enqueued e) )

  let durable ~name schedule (q : Durable.t) j arg =
    let (_ : Durable.row -> entry:string -> tick:Clock.posix -> string option) =
      Durable.need "Caravan_jobs.Recurring.durable" "push_tick"
        q.store.push_tick
    in
    entry ~name schedule (fun tick ->
        ignore (Durable.enqueue_tick q ~entry:name ~tick j (arg tick)) )

  type t =
    { entries: entry list
    ; on_error: string -> exn -> Printexc.raw_backtrace -> unit
    ; lock: Mutex.t
    ; mutable state: state
    ; mutable scope: Fiber.scope option
    ; entry_ids: unit Ids.t
    ; stopping: unit Ivar.t
    ; ended: unit Ivar.t }

  let write_error name e _ =
    try
      Printf.eprintf "caravan.jobs: recurring %s: %s\n%!" name
        (Printexc.to_string e)
    with Sys_error _ -> ()

  let create ?(on_error = write_error) entries =
    let seen = Hashtbl.create 8 in
    List.iter
      (fun e ->
        if Hashtbl.mem seen e.name then
          invalid_arg ("Caravan_jobs.Recurring.create: two entries " ^ e.name) ;
        Hashtbl.replace seen e.name () )
      entries ;
    { entries
    ; on_error
    ; lock= Mutex.create ()
    ; state= `Created
    ; scope= None
    ; entry_ids= Ids.create 8
    ; stopping= Ivar.create ()
    ; ended= Ivar.create () }

  let locked t f = Mutex.protect t.lock f

  (* The fiber of an entry: it waits for the next tick, runs it one time, then
     takes the next tick after the tick or after now, the later one. Thus a slow
     action or a step of the clock skips the ticks that passed. *)
  let rec loop t e after =
    match Schedule.next e.schedule after with
    | None ->
        ()
    | Some tick -> (
        sync (choose [Time.at Posix tick; Ivar.read t.stopping]) ;
        if Option.is_none (Ivar.peek t.stopping) then
          match e.action tick with
          | () ->
              after_tick t e tick
          | exception (Fiber.Cancelled _ as x) when is_cancelled () ->
              Printexc.raise_with_backtrace x (Printexc.get_raw_backtrace ())
          | exception x ->
              let bt = Printexc.get_raw_backtrace () in
              Fiber.shield (fun () -> t.on_error e.name x bt) ;
              after_tick t e tick )

  (* The wait for the first tick after the tick or after now. *)
  and after_tick t e tick =
    let now = Time.now Posix in
    loop t e (if before tick now then now else tick)

  let run t () =
    let result =
      catch (fun () ->
          Fiber.scope (fun s ->
              (* After a stop, the entries see [stopping] and end. *)
              locked t (fun () -> t.scope <- Some s) ;
              try
                List.iter
                  (fun e ->
                    Fiber.fork ~name:"caravan.jobs.recurring" s (fun () ->
                        let self = Fiber.self () in
                        locked t (fun () -> Ids.replace t.entry_ids self ()) ;
                        loop t e (Time.now Posix) ) )
                  t.entries
              with Fiber.Scope_closed -> () ) )
    in
    ignore (Ivar.try_fill t.ended ()) ;
    raise_first result

  let start t sw =
    locked t (fun () ->
        if t.state <> `Created then
          invalid_arg "Caravan_jobs.Recurring.start: it started or stopped" ;
        t.state <- `Running ) ;
    match Fiber.fork ~name:"caravan.jobs.recurring" sw (run t) with
    | () ->
        ()
    | exception e ->
        let bt = Printexc.get_raw_backtrace () in
        ignore (Ivar.try_fill t.ended ()) ;
        Printexc.raise_with_backtrace e bt

  let stop ?(grace = Span.s 10) t =
    let self = Fiber.self () in
    if locked t (fun () -> Ids.mem t.entry_ids self) then
      invalid_arg "Caravan_jobs.Recurring.stop: called in an entry" ;
    match
      locked t (fun () ->
          match t.state with
          | `Created ->
              t.state <- `Stopped ;
              `Ended
          | `Running ->
              t.state <- `Stopping ;
              `Stop
          | `Stopping | `Stopped ->
              `Wait )
    with
    | `Ended ->
        ignore (Ivar.try_fill t.ended ())
    | `Stop ->
        ignore (Ivar.try_fill t.stopping ()) ;
        sync (choose [Ivar.read t.ended; Time.after Mono grace]) ;
        ( match locked t (fun () -> t.scope) with
        | Some s ->
            Fiber.cancel s Exit
        | None ->
            () ) ;
        sync (Ivar.read t.ended)
    | `Wait ->
        sync (Ivar.read t.ended)

  let service t =
    Caravan.Server.service ~start:(start t) ~stop:(fun ~grace -> stop ~grace t)
end
