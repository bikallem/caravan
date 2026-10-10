(** Protocol engines.

    An engine serves one connection: it reads requests from the connection's
    flow, calls the handler, and writes the responses. HTTP/1.1 is the first
    engine; HTTP/2 will be another. *)

(** The type for what a server gives an engine for a connection. *)
type ctx =
  { config: Config.t  (** The server's configuration. *)
  ; conn: Conn_info.t  (** The connection being served. *)
  ; stopping: unit Affect.Action.t
        (** Enabled soon after the server starts to stop: an engine finishes the
            request in progress and stops taking new ones. The server makes it
            from the same cell as {!Conn_info.stopping} of [conn], one cell for
            each connection. *)
  ; is_stopping: unit -> bool
        (** Whether the server is stopping. It is [true] from the moment the
            server starts to stop, also a little before {!stopping} is enabled.
        *) }

(** The type for engines. [engine ctx flow handler] serves the connection [flow]
    until it should close, then returns; the server closes [flow]. *)
type t = ctx -> Flow.t -> Handler.t -> unit
