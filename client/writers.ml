open Weft
module Cancel = Caravan.Private.Cancel

(* The writers of request bodies belong to the client, not to the fibers that
   make requests: a request returns its response while its body goes out, for
   example through a proxy. A fiber belongs to the scope that forks it, thus the
   jobs are fibers of a scope of the client. *)

type result = (unit, exn * Printexc.raw_backtrace) Stdlib.result

(* A function that runs at the end of the job, or the result of the job. *)
type job_state = Running | Then of (result -> unit) | Ended of result

type job =
  { body: unit -> unit
  ; result: result Ivar.t
  ; handle: unit Promise.t option Atomic.t
  ; self: Fiber.Id.t option Atomic.t
  ; ended: job_state Atomic.t }

let job body =
  { body
  ; result= Ivar.create ()
  ; handle= Atomic.make None
  ; self= Atomic.make None
  ; ended= Atomic.make Running }

type t = {scope: Fiber.scope; stopping: unit Ivar.t; finished: unit Ivar.t}

(* The end of a job that the client cancelled. A [Cancelled] of a different
   scope, which the producer raises, is an error of the user. *)
exception Job_cancelled

(* The reason of the cancel of the scope of the writers. *)
exception Stopped

(* A job sets its result in all cases, also when it is cancelled before its
   first step. Thus its promise never holds a failure, and no fiber awaits
   it. *)
let run_job j =
  Atomic.set j.self (Some (Fiber.self ())) ;
  let r =
    match j.body () with
    | () ->
        Ok ()
    | exception e ->
        let bt = Printexc.get_raw_backtrace () in
        if Cancel.is_own e then Error (Job_cancelled, bt) else Error (e, bt)
  in
  ignore (Ivar.try_fill j.result r) ;
  match Atomic.exchange j.ended (Ended r) with
  | Then f ->
      f r
  | Running | Ended _ ->
      ()

(* The daemon runs the scope of the writers until [stopping]. A [Cancelled
   Stopped] from that scope is the end of the daemon. The chain of the daemon is
   not cancelled, so that exception would be a failure of [s]. A cancel of [s]
   is a close too: the daemon then ends with that cancel, and [on_stop] runs. *)
let make ?(on_stop = ignore) s =
  let ready = Ivar.create ()
  and stopping = Ivar.create ()
  and finished = Ivar.create () in
  Fiber.fork_daemon ~name:"caravan.client" s (fun () ->
      Stdlib.Fun.protect
        ~finally:(fun () ->
          on_stop () ;
          ignore (Ivar.try_fill finished ()) )
        (fun () ->
          match
            Fiber.scope (fun cs ->
                Ivar.fill ready cs ;
                sync (Ivar.read stopping) )
          with
          | () ->
              ()
          | exception Fiber.Cancelled Stopped ->
              () ) ) ;
  {scope= sync (Ivar.read ready); stopping; finished}

let scope t = t.scope

(* A cancelled caller gets [Fiber.Cancelled], not the refusal: the same cancel
   can close the client at the same time in a different fiber. *)
let refuse () = Cancel.check () ; Error.refuse_closed ()

let is_stopping t = Option.is_some (Ivar.peek t.stopping)

(* After the body of the scope returns, a fork still works until the last member
   ends. Thus [stopping] comes first. A start at the same time as [finish] can
   still get a job, and the scope waits for it. *)
let start t j =
  if is_stopping t then refuse () ;
  match
    Fiber.fork_promise ~name:"caravan.client.writer" t.scope (fun () ->
        run_job j )
  with
  | p ->
      Atomic.set j.handle (Some p)
  | exception Fiber.Scope_closed ->
      refuse ()

let result j = Ivar.read j.result

let is_started j = Option.is_some (Atomic.get j.handle)

(* A cancel before the fiber starts is not lost: the job then raises at its
   first sync. A cancel after the end does nothing. *)
let cancel j = Option.iter Promise.cancel (Atomic.get j.handle)

(* The cancel reaches all the jobs, also a job that started after [stopping] was
   set. *)
let stop t =
  ignore (Ivar.try_fill t.stopping ()) ;
  Fiber.cancel t.scope Stopped

(* Takes no more jobs, and waits at most [grace] for the jobs that run: the
   daemon ends after them. A cancel of the caller ends the wait. *)
let finish t ~grace =
  ignore (Ivar.try_fill t.stopping ()) ;
  try sync (choose [Ivar.read t.finished; Time.after Mono grace])
  with Fiber.Cancelled _ -> ()

let wait t = Fiber.shield (fun () -> sync (Ivar.read t.finished))

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
  match Atomic.get j.self with
  | None ->
      false
  | Some id ->
      Fiber.Id.equal id (Fiber.self ())

(* The result of a job that started. With [cancel], the job stops first if it
   runs: a cancel of a fiber that ended has no effect. *)
let await ~cancel:c j =
  Fiber.shield (fun () ->
      if c then cancel j ;
      sync (Ivar.read j.result) )
