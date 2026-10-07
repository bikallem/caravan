(** Request handlers and middleware.

    The same handlers serve every HTTP version. *)

(** The type for handlers: a request's response. A handler may block
    cooperatively, for example while reading the request body or calling another
    server. Handlers can run in parallel on several domains, so state they share
    must be synchronized. *)
type t = Request.t -> Response.t

(** The type for middleware: a handler that wraps another. *)
type middleware = t -> t

val compose : middleware list -> t -> t
(** [compose ms h] is [h] wrapped in [ms], the first middleware outermost:
    [compose [a; b] h] is [a (b h)]. *)

val not_found : t
(** [not_found] responds [404 Not Found] to every request. *)
