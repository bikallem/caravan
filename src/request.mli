(** HTTP requests.

    A request carries the same parts in every HTTP version: those of an HTTP/2
    request's pseudo-header fields ([:method], [:scheme], [:authority],
    [:path]), then the header fields and the body. *)

(** The type for requests. *)
type t

val make :
     ?version:Version.t
  -> ?scheme:string
  -> ?authority:string
  -> ?headers:Headers.t
  -> ?body:Body.t
  -> ?conn:Conn_info.t
  -> Method.t
  -> string
  -> t
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

    @raise Invalid_argument
      if [path] or [authority] contains a space, a control character or DEL. *)

val meth : t -> Method.t
(** [meth r] is the method of [r]. *)

val version : t -> Version.t
(** [version r] is the HTTP version [r] was received with. *)

val scheme : t -> string
(** [scheme r] is the scheme of [r], ["http"] or ["https"]. For a request
    received by a server it is the connection's scheme: a request whose target
    names another one is refused with [421]. A middleware can change it with
    {!with_scheme}, for example behind a proxy that ends TLS. *)

val authority : t -> string option
(** [authority r] is the [host[:port]] that [r] targets, if known. *)

val path : t -> string
(** [path r] is the path and query of [r]. *)

val path_only : t -> string
(** [path_only r] is the path of [r] without its query. *)

val query : t -> string option
(** [query r] is the query of [r], the part after the first ['?'], if any. It is
    [Some ""] for ["/a?"]. *)

val query_params : t -> Form.t
(** [query_params r] is [Form.parse] of the {!query} of [r], or [[]] if [r] has
    no query. For example, the pairs of ["/a?x=1&x=2"] are
    [[("x", "1"); ("x", "2")]]. It has no limit of its own: the limits of the
    server on the head of a request also limit the query. *)

val headers : t -> Headers.t
(** [headers r] are the header fields of [r], as received. *)

val cookies : t -> (string * string) list
(** [cookies r] is the pairs of the [Cookie] fields of [r] ({!Cookie.parse}), in
    the order of the fields. A user agent sends one field (RFC 6265 §5.4), and
    HTTP/2 can split it. *)

val cookie : string -> t -> string option
(** [cookie name r] is the value of the first pair of {!cookies} with the name
    [name], if any ({!Cookie.find}). The compare is case-sensitive.

    The first pair wins, and the user agent gives the order. A cookie that a
    sibling subdomain sets can come first (cookie tossing). For a security
    cookie, use a name with the prefix [__Host-]: a user agent of RFC 6265bis
    then makes sure that only this host set it. *)

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
(** [with_authority a r] is [r] with authority [a]. It checks only the
    characters that {!make} refuses: use {!is_valid_authority} for a value from
    a peer.

    @raise Invalid_argument as {!make} does. *)

val with_scheme : [`Http | `Https] -> t -> t
(** [with_scheme s r] is [r] with the scheme [s]. *)

val is_valid_authority : string -> bool
(** [is_valid_authority s] is [true] iff [s] is [host[:port]] as the server
    accepts it in a [Host] field: a [reg-name] or an IP literal in brackets (RFC
    3986), then an optional [:] and zero or more digits of a port. For example
    [a.example:80] and [[::1]:8080] are valid, and [evil.com/x], [a@b] and [x?y]
    are not. *)

(** {1:forms Forms} *)

val form : ?max_bytes:int -> ?max_fields:int -> t -> (Form.t, Form.error) result
(** [form ~max_bytes ~max_fields r] reads the body of [r] and gives its
    [application/x-www-form-urlencoded] pairs ({!Form.parse}). Call it one time
    and keep the result: [form] reads the body, thus a second call does not give
    the pairs again.

    - [Error Unsupported_media_type] if [r] does not have exactly one
      [Content-Type] field, or if its media type is not {!Form.media_type}. The
      media type is the part before the first [;], with no white space at the
      two ends. The compare ignores the case of the ASCII letters. The
      parameters, for example [charset], have no effect. Thus a request with no
      [Content-Type] gives this error, also with an empty body. The body is not
      read.
    - [Error Too_large] if the body has more than [max_bytes] bytes (default 1
      MiB, 1048576). If the body declares a larger length, [form] does not read
      it: thus the server sends no [100 Continue] to a request that expects it,
      and closes the connection after the response. The server discards the rest
      of a body as for each handler (see [Config.max_drain]).
    - [Error Too_many_fields] if the body has more than [max_fields] pairs
      (default 1000).

    {!Form.error_status} gives the status of a response for an error. The other
    exceptions of the body, for example {!Body.Truncated}, go out of [form].

    @raise Invalid_argument if [max_bytes] or [max_fields] is negative. *)

val with_form : Form.t -> t -> t
(** [with_form l r] is [r] with the body [Form.encode l], with its length, and
    the field [Content-Type: application/x-www-form-urlencoded], which replaces
    the [Content-Type] fields of [r]. For a client or a test. *)

(** {1:attributes Attributes}

    Typed values that middleware attach to a request, see {{!Attrs}[Attrs]}.
    They never go on the wire. *)

val attrs : t -> Attrs.t
(** [attrs r] are the attributes of [r]. A request that the server receives has
    no attributes. *)

val with_attrs : Attrs.t -> t -> t
(** [with_attrs m r] is [r] with the attributes [m]. *)

val find_attr : 'a Attrs.key -> t -> 'a option
(** [find_attr k r] is [Attrs.find k (attrs r)]. *)

val with_attr : 'a Attrs.key -> 'a -> t -> t
(** [with_attr k v r] is [r] with the value [v] for [k]. The other attributes
    stay. *)

val pp : Format.formatter -> t -> unit
(** [pp] formats a request line, the headers and the names of the attributes
    ({!Attrs.pp}), for inspection. It prints a line only for headers or
    attributes that exist. *)
