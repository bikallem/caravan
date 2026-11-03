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
  -> ?guard:Weft.Span.t
  -> Caravan.Handler.t
  -> (t -> 'a)
  -> 'a
(** [run handler f] serves [handler] on [127.0.0.1:80] of a new simulator, and
    calls [f] with a client. Then it closes the client ([Caravan_client.run]
    gives request bodies its [close_grace]) and aborts the server: the handlers
    that still run get a cancel.
    - [config] is for the server ({!Caravan.Config.default} by default). [run]
      replaces its [addr], also a Unix socket address, and its [on_error].
    - [engine] is that of [Caravan.Server.start].
    - [on_error] gets each error of the server after [run] records it. The
      default does nothing. The server does not report a cancel.
    - [client_config] is for the client.
    - [seed] (0 by default) and [domains] (1 by default) are those of
      [Weft_sim.run].
    - [guard] is 10 min of virtual time by default. It limits [f] only, not the
      close of the client and the server after [f]. While [f] runs and the
      client is open, the guard is the only stop: the reaper of the client
      always has a timer, thus the virtual time always moves. Thus a test that
      waits for nothing is cancelled at the guard.

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
