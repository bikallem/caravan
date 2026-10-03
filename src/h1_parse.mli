(* Internal. HTTP/1.1 message parsing (RFC 9112). The parser is strict: what
   it does not accept gets an error response rather than a guess, which closes
   the door to request smuggling. *)

type limits = {
  max_request_line : int; (** Longer request lines get 414. *)
  max_header_bytes : int;
  (** Larger header sections, CRLFs included, get 431. *)
  max_header_count : int; (** More header fields get 431. *)
}
(** The type for parsing limits. *)

val default_limits : limits
(** [default_limits] are 8 KiB, 16 KiB and 100 fields. *)

type head = {
  meth : Method.t;
  target : string; (** As received: origin, absolute, authority or ["*"]. *)
  version : Version.t;
  headers : Headers.t;
}
(** The type for request heads: the request line and the header fields. *)

val request_head : limits -> Buf_reader.t -> head
(** [request_head limits r] parses a request line and a header section from
    [r]. Up to four empty lines before the request line are skipped
    (RFC 9112 §2.2).

    @raise H1_error.E if the head is invalid or exceeds [limits].
    @raise Buf_reader.Eof if the input ends first; {!Buf_reader.consumed}
    tells whether any of the request was received. *)

val field_lines : max_bytes:int -> max_count:int -> Buf_reader.t -> Headers.t
(** [field_lines ~max_bytes ~max_count r] parses field lines up to and
    including the empty line that ends them. Each field line, CRLF included,
    counts against [max_bytes]. Request heads and chunked trailers use it.

    @raise H1_error.E on an invalid line, or with {!H1_error.Header_fields_too_large}
    past the limits.
    @raise Buf_reader.Eof if the input ends first. *)

(** {1:framing Message framing} *)

type framing =
| No_body
| Fixed of int (** A body of exactly this many bytes. *)
| Chunked (** A chunked body (RFC 9112 §7.1). *)
(** The type for how a request body is delimited. *)

val request_framing : head -> framing
(** [request_framing h] is how the body of the request [h] is delimited, by its
    [Transfer-Encoding] and [Content-Length] fields (RFC 9112 §6). The rules
    are strict, against request smuggling:
    - [Transfer-Encoding] must be exactly [chunked]. A request with both
      fields, or with [Transfer-Encoding] in HTTP/1.0, is rejected.
    - All [Content-Length] values, across fields and lists, must be the same
      number.

    @raise H1_error.E with {!H1_error.Bad_request}, or
    {!H1_error.Not_implemented} for a transfer coding other than [chunked]. *)

(** {1:target Request target} *)

type target = {
  scheme : string;
  authority : string option; (** [host[:port]], if known. *)
  path : string; (** Path and query, ["*"], or [""] for [CONNECT]. *)
}
(** The type for request targets, in the parts of a {!Request.t}. *)

val request_target : scheme:string -> head -> target
(** [request_target ~scheme h] is the target of [h] (RFC 9112 §3.2), where
    [scheme] is the connection's scheme:
    - Origin-form ["/p?q"] takes its authority from the [Host] field.
    - Absolute-form ["http://a.example/p"] takes its scheme and authority from
      the target and ignores [Host].
    - Authority-form ["a.example:443"] is only for [CONNECT].
    - Asterisk-form ["*"] is only for [OPTIONS].

    An HTTP/1.1 request must have exactly one [Host] field, any request at
    most one, and its value must be a valid [host[:port]]. An empty [Host]
    means no authority.

    @raise H1_error.E with {!H1_error.Bad_request} otherwise. *)
