(** HTTP requests.

    A request carries the same parts in every HTTP version: those of an
    HTTP/2 request's pseudo-header fields ([:method], [:scheme],
    [:authority], [:path]), then the header fields and the body. *)

type t
(** The type for requests. *)

val make :
  ?version:Version.t ->
  ?scheme:string ->
  ?authority:string ->
  ?headers:Headers.t ->
  ?body:Body.t ->
  ?conn:Conn_info.t ->
  Method.t ->
  string ->
  t
(** [make meth path] is a request for [path] with method [meth], where:
    - [path] is the path and query, as in ["/search?q=ocaml"], or ["*"] for a
      server-wide [OPTIONS], or [""] for [CONNECT], whose target is the
      [authority].
    - [version] defaults to {!Version.Http_1_1}.
    - [scheme] defaults to ["http"].
    - [authority] is the target's [host[:port]], if known. HTTP/1.1 takes it
      from an absolute-form target or the [Host] field.
    - [headers] defaults to {!Headers.empty}, [body] to {!Body.empty}.
    - [conn] is the connection the request arrived on, if any.

    @raise Invalid_argument if [path] or [authority] contains a space, a control
    character or DEL. *)

val meth : t -> Method.t
(** [meth r] is the method of [r]. *)

val version : t -> Version.t
(** [version r] is the HTTP version [r] was received with. *)

val scheme : t -> string
(** [scheme r] is the scheme of [r], ["http"] or ["https"]. *)

val authority : t -> string option
(** [authority r] is the [host[:port]] that [r] targets, if known. *)

val path : t -> string
(** [path r] is the path and query of [r]. *)

val path_only : t -> string
(** [path_only r] is the path of [r] without its query. *)

val query : t -> string option
(** [query r] is the query of [r], the part after the first ['?'], if any. It
    is [Some ""] for ["/a?"]. *)

val headers : t -> Headers.t
(** [headers r] are the header fields of [r], as received. *)

val body : t -> Body.t
(** [body r] is the body of [r]. *)

val conn : t -> Conn_info.t option
(** [conn r] is the connection [r] arrived on, if any. *)

(** {1:rewriting Rewriting}

    Middleware and proxies use these to derive new requests. The original
    request is unchanged. *)

val with_headers : Headers.t -> t -> t
(** [with_headers h r] is [r] with headers [h]. *)

val with_body : Body.t -> t -> t
(** [with_body b r] is [r] with body [b]. *)

val with_path : string -> t -> t
(** [with_path p r] is [r] with path [p].

    @raise Invalid_argument as {!make} does. *)

val with_authority : string option -> t -> t
(** [with_authority a r] is [r] with authority [a].

    @raise Invalid_argument as {!make} does. *)

val pp : Format.formatter -> t -> unit
(** [pp] formats a request line and headers, for inspection. *)
