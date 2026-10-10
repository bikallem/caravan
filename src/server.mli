(** HTTP servers.

    {[
    let () = Server.run (fun _req -> Response.string "hello\n")
    ]}

    {!run} serves until Ctrl-C. For more control, {!start} servers inside
    [Affect_unix.Unix.main] and stop them with {!shutdown}. *)

(** The type for running servers. *)
type t

val start : ?config:Config.t -> ?engine:Engine.t -> Handler.t -> t
(** [start ~config ~engine handler] listens on [config.addr] (default
    {!Config.default}) and serves connections with [engine] (default HTTP/1.1),
    calling [handler] for each request. At most [config.max_connections]
    connections are served at once.

    The server runs in an asynchronous function call made by the caller, so the
    calling function does not return before the server has stopped.

    @raise Unix.Unix_error
      if the address cannot be bound, for example with [EADDRINUSE]. *)

val local_addr : t -> Unix.sockaddr
(** [local_addr s] is the address [s] listens on, with its actual port when
    [config.addr] has port [0]. *)

val shutdown : t -> unit
(** [shutdown s] stops [s]. It closes the listening socket, so that new clients
    are refused, and idle connections close. Requests in progress get
    [config.shutdown_grace] to finish, their responses ending their connections;
    then they are cancelled. A handler can see the stop with
    {!Conn_info.stopping}. [shutdown] returns at once; see {!wait}. It takes a
    lock: do not call it from an OCaml signal handler. *)

val abort : t -> unit
(** [abort s] is like {!shutdown} but cancels the connections in progress at
    once, without a grace period. *)

val wait : t -> unit
(** [wait s] waits until [s] has stopped and all its connections are closed.

    @raise Unix.Unix_error if accepting connections failed, which stops [s] too.
*)

(** {1:run Running} *)

val run :
     ?config:Config.t
  -> ?engine:Engine.t
  -> ?domain_count:int
  -> ?on_start:(t -> unit)
  -> Handler.t
  -> unit
(** [run ~config ~engine ~domain_count ~on_start handler] serves [handler] until
    the process gets [SIGINT] (Ctrl-C) or [SIGTERM]:
    + It installs the {!Stop_signal}s and runs [Affect_unix.Unix.main] with
      [domain_count].
    + It {!start}s the server and calls [on_start] with it, for example to print
      its address.
    + On the first signal it calls {!shutdown}, on a second one {!abort}. A
      third one ends the process as the signal would, for handlers that ignore
      cancellation.

    It returns once the server has stopped, the signals {!Stop_signal.release}d.
    Call it from the main thread, outside [Affect_unix.Unix.main].

    @raise Unix.Unix_error if the address cannot be bound. *)

(**/**)

(** {1:seam Listener seam}

    For tests, not part of the public API. A test gives a fake listener that
    makes [accept] errors that a real socket cannot make on demand, for example
    [EMFILE]. *)

(** The listening socket of a server. {!Tcp.Listener} has this signature. *)
module type LISTENER = sig
  type t

  val addr : t -> Unix.sockaddr

  val wait_acceptable : t -> 'tag -> 'tag Affect.Action.t
  (** As {!Tcp.Listener.wait_acceptable}. It can be enabled spuriously. *)

  val try_accept : t -> (Unix.file_descr * Unix.sockaddr) option
  (** As {!Tcp.Listener.try_accept}. The descriptor must be non-blocking. *)

  val close : t -> unit
end

val open_connections : t -> int
(** [open_connections s] is the number of connections that [s] serves at this
    time. *)

val start_with :
     ?config:Config.t
  -> ?engine:Engine.t
  -> (module L : LISTENER) -> L.t -> Handler.t -> t
(** [start_with ~config ~engine (module L) listener handler] is like {!start},
    but serves the connections of [listener]. It ignores [config.addr] and
    [config.backlog]. It closes [listener] when the server stops, or before it
    raises if the server cannot start.

    [(module L : LISTENER)] is a module-dependent argument (OCaml 5.5): the type
    of [listener] is the type [L.t] of the module given. Production code gives
    [(module Tcp.Listener)]. *)
