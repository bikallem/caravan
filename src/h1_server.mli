(* Internal. The HTTP/1.1 engine (RFC 9112). *)

val limits : Config.t -> H1_parse.limits
(** [limits c] are the parser limits of [c]. *)

val serve : Engine.t
(** [serve ctx flow handler] serves HTTP/1.1 requests on [flow]: one at a
    time, kept alive and pipelined, each handled in its own asynchronous
    function call. It returns when the connection should close: the client
    closed it or asked to, a request was malformed, a response could not be
    completed, or the server is stopping. *)
