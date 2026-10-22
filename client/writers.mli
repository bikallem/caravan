(* Internal. The writers of request bodies. They belong to the client, not to
   the fibers that make requests. *)

type result = (unit, exn * Printexc.raw_backtrace) Stdlib.result

(* The write of a request body that a fiber of the writers runs, and its
   result. *)
type job

(* The end of a job that the client cancelled. *)
exception Job_cancelled

type t

val make : ?on_stop:(unit -> unit) -> Weft.Fiber.scope -> t
(* [make s] forks a daemon fiber in [s] that runs the scope of the writers.
   [on_stop] runs at the end of that fiber: after [stop], after [finish] and the
   end of the jobs, or at a cancel of [s]. [make] waits until the fiber starts,
   so it is a cancellation point. @raise Weft.Fiber.Scope_closed if [s] ended or
   is cancelled. *)

val scope : t -> Weft.Fiber.scope
(* The scope of the writers, for the reaper of the client. *)

val job : (unit -> unit) -> job
(* [job body] is a job that runs [body] when it starts. *)

val start : t -> job -> unit
(* [start t j] forks the job [j] in the scope of the writers. It does not wait.
   A cancel of a scope of the caller that is also on the chain of the scope of
   the writers makes the fork refuse. A caller that is cancelled through a
   different scope gets a forked job, and its next sync raises. @raise
   Weft.Fiber.Cancelled in place of [Invalid_argument] if the caller is
   cancelled. @raise Invalid_argument after [stop] or [finish], or after the end
   of the scope of the writers. *)

val result : job -> result Weft.event
(* [result j] is ready with the result of [j] at its end. *)

val is_started : job -> bool
(* [is_started j] is [true] after [start t j] forked [j]. *)

val cancel : job -> unit
(* [cancel j] cancels the fiber of [j], if [j] started. It does no sync. *)

val stop : t -> unit
(* [stop t] refuses the next jobs and cancels all the jobs at once. It does no
   sync. *)

val finish : t -> grace:Weft.Span.t -> unit
(* [finish t ~grace] refuses the next jobs, and waits at most [grace] for the
   jobs that run. A cancel of the caller ends the wait with no exception. *)

val is_stopping : t -> bool
(* [is_stopping t] is [true] after [stop] or [finish]. *)

val wait : t -> unit
(* [wait t] waits for the end of the daemon fiber of [t]. It ignores the cancels
   of the caller. *)

val when_done : job -> (result -> unit) -> result option
(* [when_done j f] gives [f] the result of [j]. If [j] ended, [f] runs at once,
   and [when_done] gives the result too. Else [f] runs later in the fiber of
   [j], at its end, and [when_done] gives [None]. Call it one time. *)

val is_self : job -> bool
(* [is_self j] is [true] in the fiber of [j]. *)

val await : cancel:bool -> job -> result
(* [await ~cancel j] gives the result of [j], a job that started. With
   [~cancel:true], it cancels [j] first. It ignores the cancels of the caller:
   the producer must not catch the cancel of its job. *)
