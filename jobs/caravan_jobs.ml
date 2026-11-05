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

let enqueue t ?delay (j : 'a job) arg =
  let due =
    match delay with
    | None ->
        None
    | Some d -> (
        if Span.compare d Span.zero < 0 then
          invalid_arg "Caravan_jobs.enqueue: delay < 0" ;
        match Time.add Mono (Time.now Mono) d with
        | Some due ->
            Some due
        | None ->
            invalid_arg "Caravan_jobs.enqueue: the delay is too long" )
  in
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
