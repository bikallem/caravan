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
    body past [max_body] bytes (default: no limit), and {!Body.Truncated} if
    [r] ends first. *)

val of_framing :
  ?max_body:int -> limits:H1_parse.limits -> Buf_reader.t -> H1_parse.framing -> Body.t
(** [of_framing ~max_body ~limits r framing] is the request body delimited by
    [framing]: {!Body.empty}, {!fixed} or {!chunked}.

    @raise H1_error.E with {!H1_error.Content_too_large} if the framing
    declares more than [max_body] bytes, before anything is read. *)
