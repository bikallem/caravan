(* Internal. HTTP/1.1 message parsing (RFC 9112). The parser is strict: what it
   does not accept gets an error response rather than a guess, which closes the
   door to request smuggling. *)

(** The type for parsing limits. *)
type limits =
  { max_request_line: int  (** Longer request lines get 414. *)
  ; max_header_bytes: int
        (** Larger header sections, CRLFs included, get 431. *)
  ; max_header_count: int  (** More header fields get 431. *) }

(** The type for request heads: the request line and the header fields. *)
type head =
  { meth: Method.t
  ; target: string  (** As received: origin, absolute, authority or ["*"]. *)
  ; version: Version.t
  ; headers: Headers.t }

val request_head : limits -> Buf_reader.t -> head
(** [request_head limits r] parses a request line and a header section from [r].
    Up to four empty lines before the request line are skipped (RFC 9112 §2.2).

    @raise H1_error.E if the head is invalid or exceeds [limits].
    @raise Buf_reader.Eof
      if the input ends first; {!Buf_reader.consumed} tells whether any of the
      request was received. *)

val field_lines : max_bytes:int -> max_count:int -> Buf_reader.t -> Headers.t
(** [field_lines ~max_bytes ~max_count r] parses field lines up to and including
    the empty line that ends them. Each field line, CRLF included, counts
    against [max_bytes]. Request heads and chunked trailers use it.

    @raise H1_error.E
      on an invalid line, or with {!H1_error.Header_fields_too_large} past the
      limits.
    @raise Buf_reader.Eof if the input ends first. *)

(** {1:response Response heads} *)

(** The type for response heads: the status line and the header fields. *)
type response_head =
  {version: Version.t; status: Status.t; reason: string; headers: Headers.t}

val response_head : limits -> Buf_reader.t -> response_head
(** [response_head limits r] parses a status line and a header section from [r]
    (RFC 9112 §4), for a client:
    - The version is [HTTP/1.0] or [HTTP/1.1]. A later [HTTP/1.x] is HTTP/1.1.
    - The status code has 3 digits, from 100 to 599.
    - The reason phrase can be empty. A line that ends after the code is also
      correct.
    - The status line counts against [limits.max_request_line], and the fields
      against the header limits.
    - An empty line before the status line, and obs-fold in a field, are errors.
      RFC 9112 §5.2 tells a user agent to replace obs-fold, but this parser is
      strict.

    @raise H1_error.E
      with {!H1_error.Bad_request} if the head is not correct, or
      {!H1_error.Header_fields_too_large} past the limits.
    @raise Buf_reader.Eof if the input ends first. *)

(** {1:framing Message framing} *)

(** The type for how a request body is delimited. *)
type framing =
  | No_body
  | Fixed of int  (** A body of exactly this many bytes. *)
  | Chunked  (** A chunked body (RFC 9112 §7.1). *)

val request_framing : head -> framing
(** [request_framing h] is how the body of the request [h] is delimited, by its
    [Transfer-Encoding] and [Content-Length] fields (RFC 9112 §6). The rules are
    strict, against request smuggling:
    - [Transfer-Encoding] must be exactly [chunked]. A request with both fields,
      or with [Transfer-Encoding] in HTTP/1.0, is rejected.
    - All [Content-Length] values, across fields and lists, must be the same
      number.

    @raise H1_error.E
      with {!H1_error.Bad_request}, or {!H1_error.Not_implemented} for a
      transfer coding other than [chunked]. *)

(** The type for how a response body is delimited. *)
type response_framing =
  | No_body  (** No body: a response to [HEAD], 1xx, 204 or 304. *)
  | Fixed of int  (** A body of exactly this many bytes. *)
  | Chunked  (** A chunked body. *)
  | Until_close  (** A body that ends when the connection closes. *)
  | Tunnel
      (** A 101, or a 2xx to [CONNECT]: the connection is no longer HTTP. *)

val response_framing : meth:Method.t -> response_head -> response_framing
(** [response_framing ~meth h] is how the body of the response [h] to a request
    with method [meth] is delimited (RFC 9112 §6.3). The rules, in order:
    + A 101, or a 2xx to [CONNECT]: {!Tunnel}.
    + A response to [HEAD], other 1xx, 204 and 304: {!No_body}, whatever their
      fields.
    + [Transfer-Encoding] and [Content-Length] together, or [Transfer-Encoding]
      in HTTP/1.0: an error.
    + [Transfer-Encoding: chunked]: {!Chunked}. A different coding:
      {!H1_error.Not_implemented}.
    + [Content-Length]: {!Fixed}, if all its values are the same number.
    + No framing field: {!Until_close}.

    @raise H1_error.E
      with {!H1_error.Bad_request} or {!H1_error.Not_implemented}. *)

(** {1:target Request target} *)

(** The type for request targets, in the parts of a {!Request.t}. *)
type target =
  { scheme: string
  ; authority: string option  (** [host[:port]], if known. *)
  ; path: string  (** Path and query, ["*"], or [""] for [CONNECT]. *) }

val request_target : scheme:string -> head -> target
(** [request_target ~scheme h] is the target of [h] (RFC 9112 §3.2), where
    [scheme] is the connection's scheme:
    - Origin-form ["/p?q"] takes its authority from the [Host] field.
    - Absolute-form ["http://a.example/p"] takes its authority from the target
      and ignores [Host]. Its scheme must be the connection's.
    - Authority-form ["a.example:443"] is only for [CONNECT].
    - Asterisk-form ["*"] is only for [OPTIONS].

    An HTTP/1.1 request must have exactly one [Host] field, any request at most
    one, and its value must be a valid [host[:port]]. An empty [Host] means no
    authority.

    @raise H1_error.E
      with {!H1_error.Misdirected_request} for an absolute-form target with
      another scheme, {!H1_error.Bad_request} otherwise. *)
