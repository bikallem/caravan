(** The harness of the tests on the simulator of weft.

    It depends only on [weft] and [weft.sim]. It does not use caravan or
    windtrap. A check of the harness raises {!Failed}. The module registers a
    printer for {!Failed} and for the exceptions of [Weft_sim].

    No helper waits in real time or polls. The time moves only at a quiescence
    (auto mode) or with {!advance_to} (manual mode). *)

open Weft

(** A check of the harness failed. The text describes the failure. *)
exception Failed of string

(** {1 Runs}

    Each run ends with the checks of [Weft_sim.run]. A call that failed and that
    no fiber awaited makes the run raise [Weft_sim.Unawaited]. A test must
    examine each failure. Thus a test awaits each call that can fail. A cancel
    prevents [Unawaited] only before the call ends.

    [Fiber.scope] waits for all its fibers, and the wait ignores cancels. Thus a
    fiber that does not end (for example a server loop) prevents the end of its
    scope. Then [run] raises {!Failed}, and [manual] raises [Weft_sim.Deadlock].
    Fork such a fiber with [Fiber.fork_daemon], or cancel its scope before the
    body of the scope ends. *)

val guard : Span.t
(** The default guard: 2 s of virtual time.

    A [Fiber.Timeout] of the code under test is not a guard failure: {!run}
    raises it unchanged. *)

val run : ?seed:int -> ?domains:int -> ?guard:Span.t -> (Weft_sim.t -> 'a) -> 'a
(** [run f] runs [f sim] in [Weft_sim.run] in auto mode. The seed is 0 and the
    simulator has 1 virtual domain by default.

    If [f] ends at [guard] of virtual time or later, [run] raises {!Failed}. The
    virtual time decides, thus the result is the same for each seed. The text
    gives the guard and the fibers that wait.

    Until the guard expires, a deadlock gives a guard failure. A wait that
    ignores the cancel of the guard (a shield) then gives [Weft_sim.Deadlock].
*)

val race :
  ?seeds:int -> ?domains:int -> ?guard:Span.t -> (Weft_sim.t -> unit) -> unit
(** [race f] runs [f] as {!run} does, one time for each seed from 1 to [seeds].
    The simulator has [domains] virtual domains (4 by default). The default of
    [seeds] is the value of the environment variable [CARAVAN_RACE_SEEDS], or
    100 if it has no value. The mutation runs set it to 10. A value that is not
    an integer of 1 or more makes [race] raise {!Failed}.

    The first failure stops the loop. Then [race] raises {!Failed}. The text
    starts with "seed ", and gives the seed, the number of domains and the
    exception with its backtrace. If the environment variable [CARAVAN_SEED] has
    a value, [race] runs only that seed. The name of the variable is fixed, also
    when the harness runs without caravan. *)

val manual : ?seed:int -> ?domains:int -> (Weft_sim.t -> 'a) -> 'a
(** [manual f] runs [f sim] in [Weft_sim.run ~auto:false]. Only {!advance_to}
    and {!advance} move the time. [manual] has no guard. A wait that nothing can
    end raises [Weft_sim.Deadlock]. Its first item is the text "manual mode:
    only advance_until moves the time".

    Keep the latency of the network at 0 (the default). With a latency, a packet
    in flight at a quiescence with no advance also gives [Weft_sim.Deadlock]. *)

(** {1 Calls} *)

(** A function that runs in a fiber of its own. *)
type 'a call

val call : ?name:string -> Fiber.scope -> (unit -> 'a) -> 'a call
(** [call s f] runs [f] in a new fiber of [s] ([Fiber.fork_promise]). The call
    records the end of [f] with a value, an exception or a cancel. *)

val ended : Weft_sim.t -> _ call -> bool
(** [ended sim c] settles ([Weft_sim.settle]), then tells if [c] ended. After
    the settle, the result is exact. *)

val await : 'a call -> 'a
(** [await c] gives the value of [c], or raises the exception of [c] with its
    backtrace ([Promise.await]). *)

val cancel : _ call -> unit
(** [cancel c] cancels the fiber of [c] ([Promise.cancel]). *)

(** {1 Exact time}

    These helpers are for manual mode. In auto mode, [Weft_sim.advance_until]
    raises [Invalid_argument]. *)

val ( +: ) : Clock.mono -> Span.t -> Clock.mono
(** [t +: d] is the time [d] after [t].

    @raise Failed on overflow. *)

val just_before : Clock.mono -> Clock.mono
(** [just_before t] is the time 1 ns before [t].

    @raise Failed on overflow. *)

val advance_to : Weft_sim.t -> Clock.mono -> unit
(** [advance_to sim at] calls [Weft_sim.advance_until sim at], then
    [Weft_sim.settle sim]. [advance_until] ends at the same time as the fibers
    that wake at [at]. The settle lets these fibers run before the test examines
    the state.

    @raise Failed if [at] is before the time now. *)

val advance : Weft_sim.t -> Span.t -> unit
(** [advance sim d] is [advance_to sim (Time.now Mono +: d)]. *)

val ends_at : Weft_sim.t -> t0:Clock.mono -> Span.t -> 'a call -> 'a
(** [ends_at sim ~t0 d c] makes sure that [c] did not end at the time now, that
    [c] did not end at [t0 + d - 1 ns], and that [c] ended at [t0 + d]. Then it
    gives the result of [c] (as {!await}).

    @raise Failed
      if a step is false, if [d] is less than 1 ns, or if the time now is after
      [t0 + d - 1 ns]. *)

val holds_from :
  Weft_sim.t -> t0:Clock.mono -> Span.t -> what:string -> (unit -> bool) -> unit
(** [holds_from sim ~t0 d ~what p] checks a probe in the way that {!ends_at}
    checks a call. It makes sure that [p ()] is false at the time now and at
    [t0 + d - 1 ns], and true at [t0 + d]. [what] names the probe in the text of
    {!Failed}.

    [p] must not wait: use for example [Net.read_now], [Ivar.peek] or [Var.get].
    In manual mode, a probe that waits raises [Weft_sim.Deadlock]. *)

(** {1 Connected pairs and fds} *)

val with_pair : Weft_sim.t -> (Net.t -> Net.t -> 'a) -> 'a
(** [with_pair sim f] gives [f] the two sides of [Weft_sim.socket_pair sim]: a
    TCP connection. The first side is the client. [with_pair] closes the two
    sides when [f] ends, also when [f] raises. *)

val no_fd_leak : Weft_sim.t -> (unit -> 'a) -> 'a
(** [no_fd_leak sim f] runs [f]. Then it settles and compares
    [Weft_sim.open_fds sim] with the number before [f].

    @raise Failed with the difference if the two numbers are not equal. *)

(**/**)

val race_with :
     replay:string option
  -> ?seeds:int
  -> ?domains:int
  -> ?guard:Span.t
  -> (Weft_sim.t -> unit)
  -> unit
(** [race_with ~replay f] is {!race} with [replay] in place of the value of
    [CARAVAN_SEED]. The tests of the harness use it. Thus their result does not
    change when [CARAVAN_SEED] has a value. *)
