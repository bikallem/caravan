(** TCP connections as {!Flow.t} values, and listening sockets.

    All waiting is cooperative, through affect: a blocked read, write, connect
    or accept suspends only the asynchronous function that makes it. *)

val flow : Unix.file_descr -> Flow.t
(** [flow fd] is the flow of the connected, non-blocking socket [fd]:
    - Reads and writes wait cooperatively when the socket is not ready, and
      raise {!Flow.Timeout} if the flow's deadline passes first.
    - A reset or broken connection raises {!Flow.Closed}.
    - Each read and write first checks whether the calling asynchronous
      function is cancelled, and raises {!Affect.Fun.Async.Cancelled} if so:
      a socket that is always ready would otherwise never notice.
    - {!Flow.shutdown_send} half-closes the socket and {!Flow.close} closes
      it. *)

val connect : Unix.sockaddr -> Flow.t
(** [connect addr] connects to [addr] and is the connection's flow.

    @raise Unix.Unix_error if the connection fails, for example with
    [ECONNREFUSED]. *)

(** Listening sockets. *)
module Listener : sig
  type t
  (** The type for listening sockets. *)

  val listen : ?backlog:int -> Unix.sockaddr -> t
  (** [listen ~backlog addr] binds a socket to [addr], with [SO_REUSEADDR],
      and listens on it with a queue of [backlog] connections (default 1024).
      Bind to port [0] to get a free port, see {!addr}.

      @raise Unix.Unix_error if the address cannot be bound. *)

  val addr : t -> Unix.sockaddr
  (** [addr l] is the address [l] is bound to, with its actual port. *)

  val accept : t -> Unix.file_descr * Unix.sockaddr
  (** [accept l] waits for a connection on [l] and is its socket, made
      non-blocking and close-on-exec, with [TCP_NODELAY] on inet sockets, and
      the peer's address. *)

  val try_accept : t -> (Unix.file_descr * Unix.sockaddr) option
  (** [try_accept l] is like {!accept} but does not wait: it is [None] if no
      connection is pending, or if the one accepted could not be set up, in
      which case it is closed.

      @raise Unix.Unix_error as accept(2) does, which includes errors of the
      pending connection that callers should retry on. *)

  val wait_acceptable : t -> 'tag -> 'tag Affect.Action.t
  (** [wait_acceptable l tag] is an action enabled with [tag] when {!accept}
      would probably not wait. It can be enabled spuriously: follow it with
      {!try_accept}. *)

  val close : t -> unit
  (** [close l] stops listening. *)
end
