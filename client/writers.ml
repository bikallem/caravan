open Affect

(* The writers of request bodies belong to the client, not to the fibers that
   make requests: a request returns its response while its body goes out, for
   example through a proxy. A child call belongs to the fiber that makes it,
   thus a supervisor of the client makes the calls. *)

type result = (unit, exn * Printexc.raw_backtrace) Stdlib.result

(* A function that runs at the end of the job, or the result of the job. *)
type ended = Running | Then of (result -> unit) | Ended of result

type job =
  { body: unit -> unit
  ; result: result Cell.Once.t
  ; handle: unit Fun.Async.t Cell.Once.t
  ; self: Fun.Async.Call.t Cell.Once.t
  ; taken: bool Atomic.t
  ; ended: ended Atomic.t }

let job body =
  { body
  ; result= Cell.Once.make ()
  ; handle= Cell.Once.make ()
  ; self= Cell.Once.make ()
  ; taken= Atomic.make false
  ; ended= Atomic.make Running }

type t =
  {jobs: job Port.t; stopping: unit Cell.Once.t; supervisor: unit Fun.Async.t}

(* The end of a job that the client cancelled. A [Cancelled] of a different
   call, which the producer raises, is an error of the user. *)
exception Job_cancelled

(* A job sets its result in all cases, also when it is cancelled before its
   first step. *)
let run_job j =
  Cell.Once.try_set j.self (Fun.Async.Call.current ()) ;
  let r =
    match j.body () with
    | () ->
        Ok ()
    | exception Fun.Async.Cancelled when Fun.Async.is_current_cancelled () ->
        Error (Job_cancelled, Printexc.get_raw_backtrace ())
    | exception e ->
        Error (e, Printexc.get_raw_backtrace ())
  in
  Cell.Once.try_set j.result r ;
  match Atomic.exchange j.ended (Ended r) with
  | Then f ->
      f r
  | Running | Ended _ ->
      ()

let make ?(on_stop = ignore) () =
  let jobs = Port.make () and stopping = Cell.Once.make () in
  let stop = Action.map (fun () -> `Stop) (Cell.Once.get' stopping) in
  let take = Action.map (fun j -> `Job j) (Port.take' jobs) in
  let rec loop () =
    match Action.invoke (Action.choose [stop; take]) with
    | `Stop ->
        ()
    | `Job j ->
        Cell.Once.try_set j.handle (Fun.Async.call (fun () -> run_job j)) ;
        loop ()
  in
  (* A cancel of the fiber that made the supervisor is a close too. A stop
     without a cancel lets the jobs end (see [finish]). *)
  let supervisor =
    Fun.Async.call (fun () ->
        Fun.Async.protect
          ~finally:(fun () -> Cell.Once.try_set stopping ())
          (fun () -> try loop () with Fun.Async.Cancelled -> on_stop ()) )
  in
  {jobs; stopping; supervisor}

let start t j =
  let offer = Port.offer' t.jobs j `Taken
  and stop = Action.map (fun () -> `Stop) (Cell.Once.get' t.stopping) in
  match Action.invoke (Action.choose [offer; stop]) with
  | `Taken ->
      Atomic.set j.taken true
  | `Stop ->
      invalid_arg "Caravan_client: the client is closed"

(* A cancel of the supervisor reaches all its jobs, also a job that it took
   after [stopping] was set. *)
let stop t =
  Cell.Once.try_set t.stopping () ;
  Fun.Async.cancel t.supervisor

(* Takes no more jobs, and waits at most [grace] for the jobs that run: the
   supervisor call ends after its children. *)
let finish t ~grace =
  Cell.Once.try_set t.stopping () ;
  let ended = Action.map (fun () -> `Ended) (Fun.Async.get' t.supervisor) in
  match
    Action.invoke
      (Action.choose [ended; Caravan.Time.(at mono (after mono grace)) `Late])
  with
  | `Ended | `Late ->
      ()
  | exception _ ->
      ()

let is_stopping t = Cell.Once.is_set t.stopping

let wait t =
  Fun.Async.mask_cancellation (fun () ->
      try Fun.Async.get t.supervisor with Fun.Async.Cancelled -> () )

(* [f] gets the result of the job: at once if the job ended, and then
   [when_done] gives the result too, else in the fiber of the job at its end.
   Thus the caller does not wait. *)
let when_done j f =
  if Atomic.compare_and_set j.ended Running (Then f) then None
  else
    match Atomic.get j.ended with
    | Ended r ->
        f r ; Some r
    | Running | Then _ ->
        assert false

let is_self j =
  match Cell.Once.is_set j.self with
  | false ->
      false
  | true ->
      Fun.Async.Call.equal (Fun.Async.Call.current ()) (Cell.Once.get j.self)

(* The result of a job that the supervisor took. With [cancel], the job stops
   first if it runs: a cancel of a call that ended has no effect. *)
let await ~cancel j =
  Fun.Async.mask_cancellation (fun () ->
      if cancel then Fun.Async.cancel (Cell.Once.get j.handle) ;
      Cell.Once.get j.result )
