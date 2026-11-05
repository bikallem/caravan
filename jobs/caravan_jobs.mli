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

(** {1:durable Durable jobs} *)

(** Jobs in a store of the application, for example a table of its database. A
    job survives a restart, and many processes can share one store.

    {[
    let send_code =
      Caravan_jobs.Durable.job ~name:"send_code"
        ~encode:(fun (address, code) -> address ^ "\n" ^ code)
        ~decode:(fun s ->
          match String.split_on_char '\n' s with
          | [address; code] ->
              (address, code)
          | _ ->
              failwith "send_code: bad payload" )
        (fun _info (address, code) -> Mail.send address code)

    let jobs =
      Caravan_jobs.Durable.create ~jobs:[Job send_code] (My_db.job_store db)

    (* In a handler: *)
    let _id = Caravan_jobs.Durable.enqueue jobs send_code (address, code)
    ]}

    A job runs at least one time. A worker holds a lease on its job and extends
    it while the job runs. If the process stops with no grace (a crash), the
    lease ends after at most [lease], and another process runs the job again.
    Thus make each job safe to run again: {!info} gives a stable id, for example
    for the idempotency key of a payment service.

    A job type that changes the format of its payload must change its name too,
    for example ["send_code.v2"]: during a deploy, the old processes do not
    claim the names that they do not know. *)
module Durable : sig
  (** {2 Failures} *)

  (** Why a job did not succeed. *)
  type reason =
    | Failed of exn * Printexc.raw_backtrace
        (** The last attempt raised this exception. After the stop of the queue,
            an attempt that raises gets its retry as usual: the store keeps it.
        *)
    | Discarded of exn * Printexc.raw_backtrace
        (** The [on] of the retry policy gave [false]. *)
    | Undecodable of exn * Printexc.raw_backtrace
        (** [decode] raised at the last attempt. *)
    | Abandoned
        (** The claims of the job used all of its attempts with no result: each
            attempt ended its process (a crash), or lost its lease. *)

  type failure =
    { name: string  (** The name of the job type. *)
    ; id: string  (** The id that the store gave. *)
    ; attempts: int  (** The claims of the job. *)
    ; reason: reason }

  (** {2 Job types} *)

  (** What a job knows about its run. *)
  type info =
    { id: string  (** The id of the job in the store: the same at each run. *)
    ; attempt: int  (** 1 for the first claim. *) }

  (** A job type with an argument of type ['a]. *)
  type 'a job

  val job :
       name:string
    -> encode:('a -> string)
    -> decode:(string -> 'a)
    -> ?retry:retry
    -> ?on_failure:('a -> failure -> unit)
    -> (info -> 'a -> unit)
    -> 'a job
  (** [job ~name ~encode ~decode ~retry ~on_failure f] is a job type. The store
      keeps [encode arg], and a worker runs [f info (decode payload)]. A
      [decode] that raises follows the [retry] policy too (default [retry ()]):
      [on] gets its exception. [on_failure] gets the jobs whose payload decoded,
      before the [on_error] of the queue. *)

  (** A job type of any argument, for the list of a queue. *)
  type any = Job : 'a job -> any

  (** {2 Stores} *)

  (** A new job, for {!store} [push]. *)
  type row =
    { name: string
    ; payload: string
    ; due: Weft.Clock.posix  (** The job runs at this time or later. *) }

  (** A job that a worker claimed. *)
  type claim =
    { id: string
    ; name: string
    ; payload: string
    ; attempts: int  (** The claims, with this one. *)
    ; lease: string  (** The token of this claim. *) }

  (** The place of the jobs. The fields are the functions of {!val-store}: read
      them to wrap a store, for example to count its calls. A wrapper must give
      each optional function too, else the functions that need it raise. *)
  type store = private
    { push: row -> string
    ; claim: names:string list -> max:int -> lease:Weft.Span.t -> claim list
    ; extend: id:string -> lease:string -> Weft.Span.t -> bool
    ; complete: id:string -> lease:string -> bool
    ; retry: id:string -> lease:string -> due:Weft.Clock.posix -> bool
    ; release: id:string -> lease:string -> bool
    ; fail: id:string -> lease:string -> error:string -> bool
    ; next_due: (names:string list -> Weft.Clock.posix option) option
    ; push_unique: (row -> key:string -> string option) option
    ; push_tick:
        (row -> entry:string -> tick:Weft.Clock.posix -> string option) option
    }

  val store :
       push:(row -> string)
    -> claim:(names:string list -> max:int -> lease:Weft.Span.t -> claim list)
    -> extend:(id:string -> lease:string -> Weft.Span.t -> bool)
    -> complete:(id:string -> lease:string -> bool)
    -> retry:(id:string -> lease:string -> due:Weft.Clock.posix -> bool)
    -> release:(id:string -> lease:string -> bool)
    -> fail:(id:string -> lease:string -> error:string -> bool)
    -> ?next_due:(names:string list -> Weft.Clock.posix option)
    -> ?push_unique:(row -> key:string -> string option)
    -> ?push_tick:(row -> entry:string -> tick:Weft.Clock.posix -> string option)
    -> unit
    -> store
  (** A store of the application. The contract:
      - [push row] adds a job and gives its new id.
      - [claim ~names ~max ~lease] takes at most [max] jobs whose name is in
        [names], whose due time is now or before, and that have no live lease.
        It takes them in the order of their due time, then of their [push]. For
        each, it adds 1 to the attempts, makes a new lease token, and sets the
        end of the lease to now + [lease]. "Now" is the clock of the store (for
        example [now()] of the database), thus the lease does not depend on the
        clocks of the processes. Two calls, also of two processes, never take
        the same job while its lease lives. The claim must be committed when
        [claim] returns.
      - The other functions change a job only if it has the lease token [lease],
        and give [true] then. Else they change nothing and give [false]: another
        worker has the job, or it ended. No function adds a job.
      - [extend ~id ~lease span] sets the end of the lease to now + [span].
      - [complete] and [fail ~error] end the job: delete it, or keep it with a
        state (for example a table of failed jobs).
      - [retry ~due] removes the lease and sets the due time.
      - [release] removes the lease and subtracts 1 from the attempts. The due
        time stays.
      - [next_due ~names] (optional) is the first time at which a job of [names]
        can be claimed: its due time, or the end of its live lease. Only
        {!drain} uses it.
      - [push_unique row ~key] (optional) adds the job only if the store has no
        job of the same name and [key], and gives its id, or [None]. A job holds
        its key from its push to its end: during its delay, its retries and its
        run. Thus a unique key prevents a second job, but it is not a debounce:
        a change during the run gets [None]. A store that keeps the ended jobs
        must free their keys (see the SQL). Only {!enqueue_unique} uses it.
      - [push_tick row ~entry ~tick] (optional), in one atomic step: if [tick]
        is after the last tick of [entry] (or [entry] has no tick), it sets the
        last tick of [entry] to [tick], adds the job and gives its id. Else it
        gives [None]. The ticks do not depend on the life of the jobs. The store
        must also give [None] for a tick after its own time plus a tolerance (5
        min in the memory store and the SQL): else a process whose clock is far
        ahead sets a future tick, and the entry gets no job in any process until
        that time. Only {!enqueue_tick} uses it.
      - The functions must be safe on many domains. Their exceptions are
        failures of the store: see {!create}. A function that blocks its domain
        (a C library of a database) stops the fibers of that domain. Give each
        function a timeout: a [claim] that does not return keeps {!stop} waiting
        until a crash, and a call after a job runs in a shield.

      With PostgreSQL, for example ([$n] are the parameters; a span goes to SQL
      as [make_interval(secs => $n)], in seconds ([Weft.Span.to_ns] / 1e9)):
      {v
      CREATE TABLE jobs (
        id bigserial PRIMARY KEY, name text NOT NULL, payload bytea NOT NULL,
        due timestamptz NOT NULL, attempts int NOT NULL DEFAULT 0,
        lease bigint, lease_until timestamptz);
      CREATE INDEX jobs_due ON jobs (name, due, id);
      CREATE SEQUENCE job_leases;

      -- push
      INSERT INTO jobs (name, payload, due)
        VALUES ($1, $2, to_timestamp($3)) RETURNING id;

      -- claim: $1 the names, $2 max, $3 the lease
      WITH next AS MATERIALIZED (
        SELECT id FROM jobs
        WHERE name = ANY($1) AND due <= now()
          AND (lease IS NULL OR lease_until <= now())
        ORDER BY due, id LIMIT $2 FOR UPDATE SKIP LOCKED)
      UPDATE jobs SET attempts = attempts + 1,
        lease = nextval('job_leases'), lease_until = now() + make_interval(secs => $3)
      FROM next WHERE jobs.id = next.id
      RETURNING jobs.id, name, payload, attempts, lease;

      -- extend: UPDATE jobs SET lease_until = now() + make_interval(secs => $3)
      --   WHERE id = $1 AND lease = $2
      -- complete, fail: DELETE FROM jobs WHERE id = $1 AND lease = $2
      -- retry: UPDATE jobs SET lease = NULL, due = to_timestamp($3)
      --   WHERE id = $1 AND lease = $2
      -- release: UPDATE jobs SET lease = NULL, attempts = attempts - 1
      --   WHERE id = $1 AND lease = $2
      -- next_due: SELECT extract(epoch FROM min(CASE WHEN lease IS NOT NULL
      --   AND lease_until > now() THEN lease_until ELSE due END))
      --   FROM jobs WHERE name = ANY($1)

      -- push_unique: a column "key text" (NULL for the other jobs: NULLs
      -- are distinct) and a unique index. A store that keeps ended jobs
      -- with a column "state" makes the index partial, and gives the
      -- same predicate to ON CONFLICT.
      CREATE UNIQUE INDEX jobs_key ON jobs (name, key);
      INSERT INTO jobs (name, payload, due, key)
        VALUES ($1, $2, to_timestamp($3), $4)
        ON CONFLICT (name, key) DO NOTHING RETURNING id;

      -- push_tick: $1 the entry, $2 the tick, then the job
      CREATE TABLE job_ticks (
        entry text PRIMARY KEY, last timestamptz NOT NULL);
      WITH t AS (
        INSERT INTO job_ticks SELECT $1, to_timestamp($2)
          WHERE to_timestamp($2) <= now() + interval '5 minutes'
        ON CONFLICT (entry) DO UPDATE SET last = EXCLUDED.last
          WHERE job_ticks.last < EXCLUDED.last
        RETURNING 1)
      INSERT INTO jobs (name, payload, due)
        SELECT $3, $4, to_timestamp($5) WHERE EXISTS (SELECT 1 FROM t)
        RETURNING id;
      v}
      A [Weft.Clock.posix] goes to SQL through [Weft.Time.posix_to_unix_s], and
      back through [Weft.Time.posix_of_unix_s] (a float: do not compare two
      converted times for equality). Use [bytea], not [text], if a payload can
      contain a NUL byte. With SQLite (3.35 or later for [RETURNING]), claim in
      [BEGIN IMMEDIATE], with WAL and a busy timeout. *)

  val memory_store : unit -> store
  (** [memory_store ()] keeps the jobs in the memory of the process, for tests.
      It has [next_due]. Its [complete] and [fail] delete the job. Each claim
      looks at all jobs: it is not for many jobs. *)

  (** {2 Queues} *)

  (** A queue: the workers of this process on a store. *)
  type t

  val create :
       ?workers:int
    -> ?poll:Weft.Span.t
    -> ?lease:Weft.Span.t
    -> ?on_error:(failure -> unit)
    -> ?on_store_error:(exn -> unit)
    -> jobs:any list
    -> store
    -> t
  (** [create ~workers ~poll ~lease ~on_error ~on_store_error ~jobs store] is a
      queue that runs the jobs of the types [jobs] from [store].
      - At most [workers] jobs (default 4) run at the same time.
      - When the store has no job for its free workers, the queue waits [poll]
        (default 1 s, at most 1 h), with a jitter of 0 to 10 %, or until an
        {!enqueue} with no delay or a {!wake} of this queue. Thus a delayed job,
        or a job of another process, starts at most about [poll] after its due
        time. Each {!start} gets a new seed of the jitters: the processes do not
        claim at the same times.
      - The leases last [lease] (default 60 s, from 1 s to 1 day). The queue
        extends them each [lease / 3], also after the end of the grace of a
        stop, until the jobs end. After a crash, the jobs wait at most [lease]
        before another process runs them. If an extend gives [false], another
        worker has the job: the queue cancels it.
      - [on_error] gets each job that ended with no success (default: one line
        on the standard error). It has the rules of the [on_error] of a
        {!Caravan_jobs.t} queue.
      - [on_store_error] gets each exception of the store (default: one line on
        the standard error), with the same rules. After the failure [k] of
        [claim] in a row (k = 1, 2, ...), the queue waits [poll * 2^(k-1)], at
        most 1 min, less a jitter of 0 to 15 %. A claim that gives a job of an
        unknown name, or more than [max] jobs, is a failure too: the queue
        releases these jobs. After a failure at the end of a job, the lease ends
        and the job runs again.

      @raise Invalid_argument
        if [workers] is less than 1, [poll] or [lease] is out of its range,
        [jobs] is empty, or two job types have one name. *)

  val row : ?delay:Weft.Span.t -> 'a job -> 'a -> row
  (** [row ~delay job arg] is the row of a new job, due after [delay] (default:
      now) on the POSIX clock of this process. Push it in a transaction of the
      application, so that the job and the data of the application commit
      together. Then call {!wake}.

      @raise Invalid_argument
        if [delay] is less than zero, or the due time is after the end of the
        range of the clock. The exceptions of [encode] go out. *)

  val enqueue : t -> ?delay:Weft.Span.t -> 'a job -> 'a -> string
  (** [enqueue t ~delay job arg] pushes {!row} [~delay job arg] to the store of
      [t], and gives the id. A job with no delay wakes [t]. It works also before
      {!start} and after {!stop}: the store keeps the job. Call it from fiber
      code.

      [enqueue] has no limit: the store keeps each job. Limit the jobs that
      clients can make, for example with the rate limit of [caravan.middleware].

      @raise Invalid_argument as {!row}. The exceptions of the store go out. *)

  val enqueue_unique :
    t -> ?delay:Weft.Span.t -> key:string -> 'a job -> 'a -> string option
  (** [enqueue_unique t ~delay ~key job arg] is {!enqueue}, but with
      [push_unique ~key]: [None] if the store has a job of [job] and [key].

      @raise Invalid_argument
        as {!row}, or if the store has no [push_unique]. The exceptions of the
        store go out. *)

  val enqueue_tick :
    t -> entry:string -> tick:Weft.Clock.posix -> 'a job -> 'a -> string option
  (** [enqueue_tick t ~entry ~tick job arg] is {!enqueue} (due now), but with
      [push_tick ~entry ~tick]: [None] if [entry] has a tick at [tick] or after.
      {!Caravan_jobs.Recurring.durable} uses it.

      @raise Invalid_argument
        if the store has no [push_tick]. The exceptions of [encode] and of the
        store go out. *)

  val wake : t -> unit
  (** [wake t] makes the queue claim now, for example after the commit of a
      transaction with a new job. After a failure of the store, the queue waits
      for its backoff first. *)

  val start : t -> Weft.Fiber.scope -> unit
  (** [start t sw] starts the queue in a new fiber of [sw].

      @raise Invalid_argument if [t] started or stopped before.
      @raise Weft.Fiber.Scope_closed
        if [sw] ended or is cancelled. Then the queue is stopped.
      @raise Weft.Fiber.Not_in_fiber outside fiber code. *)

  val stop : ?grace:Weft.Span.t -> t -> unit
  (** [stop ~grace t] stops [t] and waits until it ended. The queue claims no
      more jobs. The running jobs get [grace] (default 10 s). Then the queue
      cancels them, and gives them back to the store with [release]: they use no
      attempt, and the next start (here or in another process) runs them. A job
      that ignores its cancel keeps [stop] waiting. A job that catches the
      cancel and returns succeeds. A cancel of the scope of {!start} (a crash)
      releases no job: the leases end. A claim in progress at the end of the
      grace ends, then the queue releases its jobs.

      @raise Invalid_argument if a job of [t] calls it. *)

  val drain : t -> unit
  (** [drain t] waits until no job of [t] runs and the store has no job of the
      types of [t], also the delayed jobs, the retries, and the jobs of a lease
      that has not ended, or until [t] stopped. It looks at the store after each
      change of [t], and at least each [poll]. It is for tests: in [Weft_sim],
      the delays need no real time.

      @raise Invalid_argument
        if a job of [t] calls it, or if the store has no [next_due]. *)

  val service : t -> Caravan.Server.service
  (** [service t] is [t] as a service of {!Caravan.Server.run}. *)

  (** One snapshot of a queue. [running] is the jobs that run at this time. The
      other counts are totals of this process since {!create}. *)
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
          (** The jobs that the queue gave back to the store: at the end of the
              grace of a stop, or after a failure of the queue. *)
    ; lease_lost: int
          (** The jobs that another worker took, after their lease ended. *)
    ; store_errors: int }

  val stats : t -> stats
end

(** {1:schedule Schedules} *)

(** When a recurring job runs: for {!Recurring}. All times are UTC. *)
module Schedule : sig
  (** A schedule. *)
  type t

  val every : Weft.Span.t -> t
  (** [every span] ticks at the multiples of [span] since the POSIX epoch
      (1970-01-01 00:00:00 UTC). Thus all processes compute the same ticks.
      [every (Span.s 300)] ticks at :00, :05, ... of each hour. A span that does
      not divide one hour or one day does not start at :00 of each hour or day:
      for example [every (Span.s 420)] (7 min). Use {!cron} for times of the
      calendar.

      @raise Invalid_argument
        if [span] is not whole seconds, or is not from 1 s to 1 day. *)

  val cron : string -> t
  (** [cron text] is a cron expression of 5 fields: the minute (0-59), the hour
      (0-23), the day of the month (1-31), the month (1-12, or [jan]-[dec]), and
      the day of the week (0-7 or [sun]-[sat]; 0 and 7 are Sunday), in UTC.
      Spaces and tabs separate the fields.
      - Each field is a list of items [x,y,...]. An item is [*], a number [n], a
        range [a-b], or a step [*/n] or [a-b/n]. Names have any case, also in
        ranges ([mon-fri]).
      - The macros: [@yearly] and [@annually] ([0 0 1 1 *]), [@monthly]
        ([0 0 1 * *]), [@weekly] ([0 0 * * 0]), [@daily] and [@midnight]
        ([0 0 * * *]), [@hourly] ([0 * * * *]).
      - The rule of Vixie cron for the days: a field that starts with [*] is not
        restricted. If the day of the month and the day of the week are both
        restricted, a day matches if one of them matches. Else the two must
        match. Thus [0 0 1,15 * 1] is the 1st, the 15th and each Monday, and
        [0 0 */2 * 1] is each Monday of an odd day.
      - There is no [?], [L], [W], [#] or [@reboot].

      @raise Invalid_argument
        for a bad expression, with the field and the reason, or for an
        expression that matches no date (for example [0 0 30 2 *]). *)

  val of_string : string -> (t, string) result
  (** [of_string text] is {!cron}, with an [Error] in place of the exception:
      for a schedule from a configuration. *)

  val next : t -> Weft.Clock.posix -> Weft.Clock.posix option
  (** [next t time] is the first tick of [t] strictly after [time], or [None]
      after the end of the range of the clock (the year 2262). A cron tick is at
      the start of its minute. *)
end
