(** HTTP servers.

    {[
      let () =
        Affect_unix.Unix.main @@ fun () ->
        let server = Server.start (fun _req -> Response.string "hello\n") in
        Server.wait server
    ]}

    A server must be started inside [Affect_unix.Unix.main]. *)

type t
(** The type for running servers. *)

val start : ?config:Config.t -> ?engine:Engine.t -> Handler.t -> t
(** [start ~config ~engine handler] listens on [config.addr] (default
    {!Config.default}) and serves connections with [engine] (default HTTP/1.1),
    calling [handler] for each request. At most [config.max_connections]
    connections are served at once.

    The server runs in an asynchronous function call made by the caller, so
    the calling function does not return before the server has stopped.

    @raise Unix.Unix_error if the address cannot be bound, for example with
    [EADDRINUSE]. *)

val local_addr : t -> Unix.sockaddr
(** [local_addr s] is the address [s] listens on, with its actual port when
    [config.addr] has port [0]. *)

val shutdown : t -> unit
(** [shutdown s] stops [s]: it closes the listening socket, so that new
    clients are refused, and cancels the connections in progress, which
    close. It returns at once; see {!wait}. *)

val wait : t -> unit
(** [wait s] waits until [s] has stopped and all its connections are closed.

    @raise Unix.Unix_error if accepting connections failed, which stops [s]
    too. *)
