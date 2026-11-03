(** Tests of a handler through the real server and client of caravan.

    {[
    let () =
      Caravan_testing.run
        (fun _req -> Caravan.Response.string "hi")
        (fun t ->
          let r = Caravan_testing.get t "/" in
          assert (r.body = "hi") )
    ]}

    {!run} starts a server and a client in the simulator of weft. The time is
    virtual: a test of a 30 s timeout takes no real time. A run is the same for
    a seed. The simulator ({!sim}) can add latency, network faults and names.

    By default, an error of the handler fails the test: {!run} raises
    {!Unchecked_errors}, unless the test takes the error with {!take_errors}. *)

(** The type for a server and a client in a run. *)
type t

(** The function of {!run} did not end before the guard. *)
exception Guard_expired of Weft.Span.t

(** At the end of {!run}, the server had errors that no {!take_errors} took. *)
exception Unchecked_errors of (exn * Printexc.raw_backtrace) list

val run :
     ?config:Caravan.Config.t
  -> ?engine:Caravan.Engine.t
  -> ?on_error:(exn -> Printexc.raw_backtrace -> unit)
  -> ?client_config:Caravan_client.Config.t
  -> ?seed:int
  -> ?domains:int
  -> ?manual:bool
  -> ?stop_on_signals:bool
  -> ?guard:Weft.Span.t
  -> Caravan.Handler.t
  -> (t -> 'a)
  -> 'a
(** [run handler f] serves [handler] on [127.0.0.1:80] of a new simulator, and
    calls [f] with a client. Then it closes the client ([Caravan_client.run]
    gives request bodies its [close_grace]) and aborts the server: the handlers
    that still run get a cancel. In manual mode, the server aborts first, then
    the client closes with no grace (see {!section-manual}).
    - [config] is for the server ({!Caravan.Config.default} by default). [run]
      replaces its [addr], also a Unix socket address, and its [on_error].
    - [engine] is that of [Caravan.Server.start].
    - [on_error] gets each error of the server after [run] records it. The
      default does nothing. The server does not report a cancel.
    - [client_config] is for the client.
    - [seed] (0 by default) and [domains] (1 by default) are those of
      [Weft_sim.run].
    - [manual] (default [false]) runs the simulator in manual mode: see
      {!section-manual}.
    - [stop_on_signals] (default [false]) stops the server at SIGINT and
      SIGTERM, as [Caravan.Server.run] does: see {!section-signals}.
    - [guard] limits [f] only, not the close of the client and the server after
      [f]. In auto mode it is 10 min of virtual time by default. While [f] runs
      and the client is open, the guard is then the only stop: the reaper of the
      client always has a timer, thus the virtual time always moves. Thus a test
      that waits for nothing is cancelled at the guard. A manual run has no
      guard by default.

    Before it closes the client, [run] cancels the fibers of {!background}.

    Call [run] outside a weft run: it makes its own simulator. The result is the
    first of these that occurs:
    + An exception of [on_error]. It cancels all connections of the server, thus
      it is the cause of the other failures.
    + {!Guard_expired}, if [f] did not end before [guard].
    + An exception of [f].
    + An exception of [Caravan.Server.wait], for example a failure of the
      listener.
    + {!Unchecked_errors}, with the errors that no {!take_errors} took.
    + The value of [f].

    The exceptions of [Caravan.Server.start] (for example [Invalid_argument])
    and of [Weft_sim.run] go out unchanged. *)

val seed : t -> int
(** [seed t] is the seed of the simulator of [t]. *)

val domains : t -> int
(** [domains t] is the number of virtual domains of the simulator of [t]. *)

(** {1:background Background fibers} *)

val background : t -> (unit -> 'a) -> 'a Weft.Promise.t
(** [background t g] runs [g] in a new fiber, while the function of {!run}
    continues. A failure of a background fiber must be awaited. If not, the run
    raises [Weft_sim.Unawaited] (in {!race}, in {!Race_failed}).

    The end of the function of {!run} cancels the background fibers: a return,
    an exception or the end of the guard. Then [run] waits for their end. A
    request in the background is cancelled as a client that leaves: the client
    closes the connection, and the handler gets a cancel.
    - The cancel is not a failure. But a fiber that raises another exception
      after its cancel, for example a [Failure] in a cleanup, fails. If no fiber
      awaits it, the run raises [Weft_sim.Unawaited].
    - weft checks a promise when the GC finalizes it. Thus the run never reports
      a promise that the test keeps, for example in the value of the function.
    - A background fiber that ignores its cancel or waits in a shield keeps the
      end of the run. In manual mode, that is a [Weft_sim.Deadlock].

    @raise Weft.Fiber.Scope_closed after the end of the function of {!run}. *)

(** {1:signals Signals}

    With [run ~stop_on_signals:true], the simulator watches SIGINT and SIGTERM,
    and a daemon fiber calls [Caravan.Server.stop_on_signals]:
    - The first signal stops the server with [config.shutdown_grace]. The server
      refuses new connections: a request raises a [Caravan_client.Error] (the
      case depends on the domains and the pool). A request in progress gets the
      grace, but the [response_timeout] of the client (30 s by default) can end
      it first. In manual mode, only an advance, a second signal or the end of
      {!run} ends the grace.
    - The second signal aborts the server.
    - The third signal ends the program.

    A signal that ends the program ends the run at once: [Weft_sim.run] raises
    [Weft_sim.Killed] from its scheduler. Thus there is no teardown: {!run} does
    not check the errors and does not wait for the server. In {!race}, the error
    is in {!Race_failed}.

    A signal that the world does not watch gets its default action at once, as
    in a program that does not watch it. With no [~stop_on_signals], the world
    watches no signal. Most signals end the program, but SIGCHLD, SIGWINCH,
    SIGCONT, SIGSTOP and SIGTSTP do nothing, and the world ignores SIGPIPE (see
    [Weft_sim.program]).

    A test that also calls [Caravan.Server.stop_on_signals (server t)] makes a
    second watcher: each signal goes to one of the two. *)

val signal : t -> Weft.Signal.t -> unit
(** [signal t s] sends [s] to the program of the run, then lets each fiber that
    can run, run ({!settle}). Thus after a first signal, the server has started
    to stop: its [Caravan.Conn_info.stopping] is ready. Two calls are a first
    and a second signal. Call it from a fiber of the run. *)

(** {1:race Many seeds} *)

(** A run of {!race} raised [error]. *)
exception
  Race_failed of
    {seed: int; domains: int; error: exn; backtrace: Printexc.raw_backtrace}

val race :
     ?seeds:int
  -> ?replay:int
  -> ?domains:int
  -> ?config:Caravan.Config.t
  -> ?engine:Caravan.Engine.t
  -> ?on_error:(exn -> Printexc.raw_backtrace -> unit)
  -> ?client_config:Caravan_client.Config.t
  -> ?manual:bool
  -> ?stop_on_signals:bool
  -> ?guard:Weft.Span.t
  -> Caravan.Handler.t
  -> (t -> unit)
  -> unit
(** [race handler f] calls [run ~seed ~domains handler f] for each seed from 1
    to [seeds] (100 by default), on [domains] virtual domains (4 by default).
    The seed selects the order of the fibers, thus each seed can show another
    race. With [~replay:s], it runs only the seed [s], and [seeds] has no
    effect. The other arguments go to {!run}.

    The first run that raises stops the loop: {!race} raises {!Race_failed},
    with the backtrace of the error. Its text tells the [~replay] of that seed.

    [handler] and [f] are the same values in each run. Thus a replay is exact
    only if they keep no state from one run to the next: for example a
    [Caravan_middleware.Request_id.middleware] value has its own counter.

    @raise Invalid_argument if [seeds] or [domains] is less than 1. *)

(** {1:manual Manual time}

    In a manual run ([run ~manual:true]), only {!advance} and {!advance_to} move
    the virtual time. Thus a test can check a timeout at its exact time. A wait
    that nothing can end raises [Weft_sim.Deadlock]: the timers of the client
    and the server do not move the time.

    At the end of a manual run, the client closes with no [close_grace]: a
    request body that still goes stops. A handler or a request body producer
    that ignores its cancel or waits in a shield makes the end a
    [Weft_sim.Deadlock], which then hides an exception of [f]. *)

val advance : t -> Weft.Span.t -> unit
(** [advance t d] moves the virtual time by [d], then lets the fibers that wake
    run ({!settle}). The advance stops at each deadline on its way, for example
    the reaper of the client each half of [idle_timeout]: an advance of days
    takes many steps. One advance at a time, from any fiber of the function of
    {!run}.

    @raise Invalid_argument
      if the run is not manual, if [d] is negative, after the end of the mono
      line, or if another advance is active at the same time (with the text of
      [Weft_sim]). *)

val advance_to : t -> Weft.Clock.mono -> unit
(** [advance_to t at] is {!advance} to the time [at].

    @raise Invalid_argument
      if the run is not manual, if [at] is in the past, or if another advance is
      active at the same time (with the text of [Weft_sim]). *)

val settle : t -> unit
(** [settle t] lets each fiber that can run, run, with no move of the time. It
    also works in auto mode. *)

val client : t -> Caravan_client.t
(** [client t] is the client of [t]. *)

val server : t -> Caravan.Server.t
(** [server t] is the server of [t]. *)

val sim : t -> Weft_sim.t
(** [sim t] is the simulator of [t]. For example,
    [Weft_sim.set_dns (sim t) "example.com" (Addresses ["127.0.0.1"])] sends the
    requests for [example.com] to the server. *)

val authority : t -> string
(** [authority t] is ["127.0.0.1"], the authority of the server. *)

val take_errors : t -> (exn * Printexc.raw_backtrace) list
(** [take_errors t] gives the errors of the server since the last call, in their
    order, and removes them. *)

val request : t -> Caravan.Request.t -> Caravan.Response.t
(** [request t req] is [Caravan_client.request (client t) req]. A request with
    no authority gets {!authority}. The caller must read the body to its end or
    close it. *)

val with_response : t -> Caravan.Request.t -> (Caravan.Response.t -> 'a) -> 'a
(** [with_response t req f] is {!request}, then [f] with the response. Then it
    closes the body, also when [f] raises. *)

(** A response with its full body. *)
type reply = {status: Caravan.Status.t; headers: Caravan.Headers.t; body: string}

val fetch : t -> Caravan.Request.t -> reply
(** [fetch t req] sends [req] as {!request} does, and reads the full body. *)

val get : ?headers:Caravan.Headers.t -> t -> string -> reply
(** [get t path] is {!fetch} of a [GET] of [path], for example ["/a?b=c"].

    @raise Invalid_argument
      if [path] does not start with ['/'], from [Caravan_client.request]. *)
