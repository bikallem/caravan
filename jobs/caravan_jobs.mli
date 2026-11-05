(** Background jobs in the process, with retries.

    A handler gives work to a queue, and the response does not wait for it. The
    workers of the queue run the jobs in fibers that live longer than the
    request.

    {[
    let send_code : (string * string) Caravan_jobs.job =
      Caravan_jobs.job ~name:"send_code" (fun (address, code) ->
          Mail.send address code )

    let jobs = Caravan_jobs.create ()

    let handler req =
      match Caravan_jobs.enqueue jobs send_code (address, code) with
      | Ok () ->
          Caravan.Response.string "We sent a code."
      | Error (`Full | `Stopped) ->
          Caravan.Response.string ~status:Caravan.Status.service_unavailable
            "Try again later."

    let () =
      Caravan.Server.run
        ~services:[Caravan_jobs.service jobs]
        (module Weft_os)
        handler
    ]}

    The queue is in the memory of the process: it is not durable. A stop of the
    process loses the jobs that did not run. The queue reports each lost job to
    [on_failure] and [on_error], thus the application can log them or keep them
    for a resend.

    A job runs at least one time if [enqueue] gives [Ok], and the queue does not
    stop before its start. It can run more than one time: a retry runs it again
    after a failure. Thus make each job safe to run again. *)

(** {1 Failures} *)

(** Why a job did not succeed. *)
type reason =
  | Failed of exn * Printexc.raw_backtrace
      (** The last attempt raised this exception. After the stop of the queue,
          the first attempt that raises is the last attempt. *)
  | Discarded of exn * Printexc.raw_backtrace
      (** The [on] of the retry policy gave [false] for this exception. *)
  | Lost of [`Not_started | `Running]
      (** The queue stopped before the job ran, or it cancelled the job at the
          end of the grace. A job that ran in part can have done a part of its
          work. *)

type failure =
  { name: string  (** The name of the job type. *)
  ; id: int  (** The number of the job in its queue, from 0. *)
  ; attempts: int  (** The attempts that started. *)
  ; reason: reason }

(** {1 Job types} *)

(** A retry policy. *)
type retry

val retry :
     ?attempts:int
  -> ?base:Weft.Span.t
  -> ?max:Weft.Span.t
  -> ?on:(exn -> bool)
  -> unit
  -> retry
(** [retry ~attempts ~base ~max ~on ()] runs a job at most [attempts] times
    (default 5). After a failure of attempt [n], the next attempt starts after
    [d] less a jitter:
    - [d] is [base * 2^(n-1)], at most [max]. The default [base] is 1 s, and the
      default [max] is 5 min.
    - The jitter is from 0 to 15 % of [d]. It comes from the number of the job
      and [n], thus a test gets the same times in each run.

    If [on e] (default: [true] for all) gives [false], the job ends at once with
    [Discarded]. [on] must not raise: if it raises, the job is [Failed], then
    the queue fails with the exception of [on].

    @raise Invalid_argument
      if [attempts] is less than 1, [base] is not more than zero, or [max] is
      less than [base]. *)

val no_retry : retry
(** One attempt. *)

(** A job type with an argument of type ['a]. *)
type 'a job

val job :
     name:string
  -> ?retry:retry
  -> ?on_failure:('a -> failure -> unit)
  -> ('a -> unit)
  -> 'a job
(** [job ~name ~retry ~on_failure f] is a job type that runs [f arg]. A job
    succeeds when [f] returns. The default [retry] is [retry ()].
    [on_failure arg failure] gets each job that did not succeed, before the
    [on_error] of the queue. It has the same rules as [on_error] (see
    {!create}).

    A job must not use the request or its body: they close after the response.
    Give the job the values that it needs. *)

(** {1 Queues} *)

(** A queue with its workers. *)
type t

val create :
  ?workers:int -> ?capacity:int -> ?on_error:(failure -> unit) -> unit -> t
(** [create ~workers ~capacity ~on_error ()] is a queue that runs at most
    [workers] jobs at the same time (default 4).
    - At most [capacity] new jobs (default 10_000) wait or are delayed. A retry
      is not a new job: it always goes in the queue. Thus at most
      [capacity + workers] jobs wait or are delayed. When the jobs fail and
      their retries fill the queue, new jobs get [`Full]: this is by design.
    - [on_error] gets each job that did not succeed. The default writes one line
      to the standard error.

    The rules for [on_error] and [on_failure]:
    - They must not raise. If one raises, the other reports occur, then the
      queue fails with the first exception: in {!Caravan.Server.run}, the server
      stops.
    - They must not wait: they run in a shield.
    - Two domains can call them at the same time.
    - They must not call {!stop} or {!drain}: the queue can wait for them.

    @raise Invalid_argument if [workers] or [capacity] is less than 1. *)

val enqueue :
  t -> ?delay:Weft.Span.t -> 'a job -> 'a -> (unit, [`Full | `Stopped]) result
(** [enqueue t ~delay job arg] adds a job. It runs after [delay] (default: at
    once), in FIFO order: the jobs that wait run in the order of their
    [enqueue], and the delayed jobs in the order of their due time, then of
    their [enqueue].
    - [Error `Full]: [capacity] jobs wait or are delayed.
    - [Error `Stopped]: the queue stops or stopped. A job that enqueues a job
      gets it too: match the result.

    A job that comes before {!start} waits for it. [enqueue] never waits and is
    not a cancellation point. Call it from fiber code: it reads the time.

    @raise Invalid_argument
      if [delay] is less than zero, or the due time is after the end of the
      range of the clock. *)

val start : t -> Weft.Fiber.scope -> unit
(** [start t sw] starts the workers in a new fiber of [sw]. Use {!service} for
    {!Caravan.Server.run}.

    @raise Invalid_argument if [t] started or stopped before.
    @raise Weft.Fiber.Scope_closed
      if [sw] ended or is cancelled. Then the queue ends: it reports each job as
      [Lost `Not_started], as a stop before {!start} does. If a hook raises in
      these reports, [start] raises the first exception of the hooks.
    @raise Weft.Fiber.Not_in_fiber outside fiber code. *)

val stop : ?grace:Weft.Span.t -> t -> unit
(** [stop ~grace t] stops [t] and waits until it ended:
    + The queue refuses new jobs ([`Stopped]).
    + The workers run the jobs that wait, also the delayed jobs that are due at
      the stop, for at most [grace] (default 10 s). An attempt that raises gets
      no retry: [Failed].
    + At the end of the grace, the queue cancels the jobs that run
      ([Lost `Running]). Each job that did not start is [Lost `Not_started],
      also the delayed jobs and the retries.

    A job that ignores its cancel keeps [stop] waiting. A job that catches the
    cancel and returns succeeds. A second call waits for the end of the first
    stop. A stop before {!start} reports the jobs at once. A stopped queue stays
    stopped. A cancel of the scope of {!start} ends the queue with no grace,
    with the same reports.

    A fiber that a job forks must not call [stop] or [drain]: the queue cannot
    find it, and the job then waits for itself.

    @raise Invalid_argument if a job of [t] calls it.
    @raise e
      the first exception of [on_failure] or [on_error], for a stop before
      {!start}. *)

val drain : t -> unit
(** [drain t] waits until no job waits, is delayed or runs, or until [t]
    stopped. It is for tests: in [Weft_sim], the delayed jobs need no real time.
    Do not call it in a handler: a [drain] that a cancel ends keeps its waiter
    until no job waits, is delayed or runs.

    @raise Invalid_argument if a job of [t] calls it. *)

val service : t -> Caravan.Server.service
(** [service t] is [t] as a service of {!Caravan.Server.run}: it starts and
    stops with the server. *)

(** {1 Statistics} *)

type state = [`Created | `Running | `Stopping | `Stopped]

(** One snapshot of a queue. [waiting], [delayed] and [running] are the jobs at
    this time. The other counts are totals since {!create}. *)
type stats =
  { state: state
  ; waiting: int
  ; delayed: int  (** The delayed jobs and the retries that wait. *)
  ; running: int
  ; succeeded: int
  ; retried: int  (** The retries that the queue planned. *)
  ; failed: int
  ; discarded: int
  ; lost: int }

val stats : t -> stats
(** [stats t] is the state of [t], for example for a health check. *)
