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
