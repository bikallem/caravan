(* Internal. HTTP/1.1 request bodies (RFC 9112 §6, §7), read from the
   connection's buffered reader. *)

val fixed : Buf_reader.t -> int -> Body.t
(** [fixed r n] is the body of exactly [n] bytes read from [r], as declared by
    [Content-Length]. Its {!Body.length} is [Some n]. Reads return at most the
    bytes that remain, then [0] without reading [r] again, so that the next
    request on the connection stays in [r].

    Reads raise {!Body.Truncated} if [r] ends before [n] bytes. *)

val chunked : ?max_body:int -> limits:H1_parse.limits -> Buf_reader.t -> Body.t
(** [chunked ~max_body ~limits r] is the chunked body (RFC 9112 §7.1) read
    from [r]. Chunk extensions are checked and ignored. The trailer fields,
    parsed within the header limits of [limits], are the body's
    {!Body.trailers} once it is read to its end. Reading stops right after the
    body, so that the next request on the connection stays in [r].

    Reads raise {!H1_error.E} with {!H1_error.Bad_request} on malformed
    framing, with {!H1_error.Content_too_large} when a chunk would take the
    body past [max_body] bytes as sent, chunk framing included (default: no
    limit), and {!Body.Truncated} if [r] ends first. *)

(** {1:handle Request body handles} *)

type t
(** The type for the engine's handle on a request body: the body the handler
    reads, and what the engine needs once the handler is done. *)

val make :
  ?max_body:int ->
  ?expect_continue:(unit -> unit) ->
  limits:H1_parse.limits ->
  Buf_reader.t ->
  H1_parse.framing ->
  t
(** [make ~max_body ~expect_continue ~limits r framing] is the request body
    delimited by [framing]: {!Body.empty}, {!fixed} or {!chunked}. If the
    request expects [100-continue], [expect_continue ()] runs once, just
    before the handler's first read of the body; the engine sends
    [100 Continue] from it.

    The handler's reads raise {!H1_error.E} for a malformed body or one that
    timed out ([408]), and {!Body.Truncated} when the client ended or reset
    the connection before the body's end.

    @raise H1_error.E with {!H1_error.Content_too_large} if the framing
    declares more than [max_body] bytes, before anything is read. *)

val body : t -> Body.t
(** [body t] is the body for the handler. *)

val completed : t -> unit Affect.Action.t
(** [completed t] is enabled once the handler has read the body to its end,
    at once if there is none. *)

val is_complete : t -> bool
(** [is_complete t] is [true] iff the body was read to its end, or there is
    none. A [Content-Length] body is complete once its last byte is read. *)

val unread : t -> int option
(** [unread t] is the number of bytes of the body left to read, if known: for
    a [Content-Length] body, or a body read to its end. *)

val failed : t -> bool
(** [failed t] is [true] once a read of the body raised: the framing was
    malformed or truncated, or the connection failed. Later reads raise the
    same error again, and the connection cannot be reused. *)

val drain : t -> max:int -> bool
(** [drain t ~max] reads and discards what remains of the body, so that the
    next request on the connection can be read. It is [true] iff the
    connection can be reused: the body ended within [max] more bytes, counted
    as sent, chunk framing included. It is
    [false], and the connection must close, if more remains, if the framing
    is malformed or truncated, if a read already {!failed}, or if
    [100-continue] was expected but never
    sent, because the client may still be holding the body back. Nothing is
    read in that case, or if more than [max] bytes are known to remain. *)
