(** Protocol engines.

    An engine serves one connection: it reads requests from the flow of the
    connection, calls the handler, and writes the responses. HTTP/1.1 is the
    first engine. HTTP/2 will be another. *)

(** What a server gives an engine for a connection. *)
type ctx =
  { config: Config.t  (** The configuration of the server. *)
  ; conn: Conn_info.t  (** The connection that the engine serves. *)
  ; is_stopping: unit -> bool
        (** [true] when the server starts to stop, and then always [true]. It
            does not wait. An engine waits for the stop with
            {!Conn_info.stopping} of [conn]: it finishes the request in progress
            and takes no new request. *) }

(** The type for engines. [engine ctx flow handler] serves the connection [flow]
    until it must close, then it returns. The server closes [flow].

    The engine runs in the fiber of the connection. It forks fibers only into
    scopes that it makes. An abort, the end of the shutdown grace and a cancel
    of the scope of the server raise [Weft.Fiber.Cancelled] at its next [sync].

    For each upgrade response ({!Response.upgrade}) that the engine gets, it
    runs the function of the upgrade or, on each path where it does not switch,
    calls {!Response.abandon}. *)
type t = ctx -> Flow.t -> Handler.t -> unit
