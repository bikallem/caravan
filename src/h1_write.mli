(* Internal. HTTP/1.1 response serialization (RFC 9112). *)

type framing =
| No_body
(** No body and no framing field: 1xx, 204 and 304 responses, 2xx responses
    to [CONNECT], and upgrades. *)
| Head_length of int
(** The response to a [HEAD] request whose body length is known: a
    [Content-Length] field, and no body. *)
| Length of int (** A body of this length, with [Content-Length]. *)
| Chunked (** A chunked body, for HTTP/1.1 clients. *)
| Close_delimited
(** A body of unknown length for an HTTP/1.0 client: it ends when the
    connection closes. *)
(** The type for how a response body is sent. *)

val framing : meth:Method.t -> version:Version.t -> Response.t -> framing
(** [framing ~meth ~version r] is how [r] is sent in response to a request with
    method [meth] and version [version] (RFC 9112 §6.3, RFC 9110 §6.4.1). *)

val wants_close : Response.t -> bool
(** [wants_close r] is [true] iff the handler set [Connection: close] on [r]. *)

val head :
  Buf_writer.t ->
  ?date:string ->
  version:Version.t ->
  keep_alive:bool ->
  framing ->
  Response.t ->
  unit
(** [head w ~date ~version ~keep_alive framing r] writes the head of [r] to
    [w], for a request of version [version]:
    - The status line, always [HTTP/1.1].
    - [Date: date], unless [date] is [None] or [r] has a [Date] field.
    - The fields of [r], in order, except those the connection owns:
      [Content-Length], [Transfer-Encoding], [Connection], [Keep-Alive],
      [Proxy-Connection], [TE], and [Upgrade] except on a 101 response.
    - The framing field of [framing], if any.
    - [Connection: Upgrade] on a 101 response; otherwise [Connection: close]
      if not [keep_alive] or the body is {!Close_delimited}, or
      [Connection: keep-alive] for an HTTP/1.0 request that is kept alive.
    - The empty line that ends the head. *)

(** {1:request Requests, for a client} *)

val request_framing : Request.t -> framing
(** [request_framing req] is how the body of [req] is sent (RFC 9110
    §8.6, RFC 9112 §6.2):
    - A body of length 0: no framing field ({!No_body}) for [GET], [HEAD],
      [DELETE], [OPTIONS], [TRACE] and [CONNECT], and [Content-Length: 0]
      ([Length 0]) for the other methods.
    - A body of known length [n]: [Length n].
    - A body of unknown length: {!Chunked}. *)

val request_closes : Request.t -> bool
(** [request_closes req] is [true] iff [req] has a [Connection] field with
    the token [close]. *)

val request_head : Buf_writer.t -> framing -> Request.t -> unit
(** [request_head w framing req] writes the head of [req] to [w]:
    - The request line [METHOD SP target SP HTTP/1.1]. The target is the
      path of [req], ["/"] for an empty path, the authority for [CONNECT],
      and ["*"] for a server-wide [OPTIONS].
    - [Host], the authority of [req], as the first field.
    - The fields of [req], in order, except [Host], the fields that the
      connection owns ([Content-Length], [Transfer-Encoding], [Connection],
      [Keep-Alive], [Proxy-Connection], [TE], [Upgrade]), and the fields that
      [Connection] names.
    - The framing field of [framing], if any.
    - [Connection: close] if {!request_closes} [req].
    - The empty line that ends the head.

    @raise Invalid_argument if [req] has no authority, or a path that is
    not empty and does not start with ["/"] (except ["*"] for [OPTIONS]
    and any path for [CONNECT]), or a path with ["#"]. *)

(** {1:body Bodies} *)

exception Length_mismatch
(** Raised by {!body} when a body produces fewer or more bytes than its
    [Content-Length]. The response is broken: the connection must close. *)

val body : ?before:(unit -> unit) -> Buf_writer.t -> framing -> Body.t -> unit
(** [body ~before w framing b] writes [b] to [w] as [framing] says, and
    flushes [w]. [before ()] runs once, just before the first byte or flush
    of the body reaches [w]: the engine writes the head from it, so that
    the response stays uncommitted while [b] has produced nothing.
    Framings:
    - {!No_body} and {!Head_length} write nothing; a writer body never runs.
    - [Length n] writes exactly [n] bytes. It raises {!Length_mismatch}
      before writing a byte past [n], or at the end if [b] was shorter.
    - {!Chunked} writes each write of [b] as a chunk, then the last chunk,
      the trailers of [b] and the final CRLF. Trailers that RFC 9110 §6.5.1
      does not allow, such as [Content-Length] or [Set-Cookie], are
      dropped.
    - {!Close_delimited} writes the bytes of [b] as they are.

    Each chunk of a source body, and each {!Body.Writer.flush} of a writer
    body, flushes [w], so that streamed data is not held back. [b] is closed
    when [body] returns or raises, which releases a proxied source. *)
