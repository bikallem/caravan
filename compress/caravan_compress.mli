(** The compression of responses: gzip and deflate (RFC 9110 §8.4.1), with the
    pure OCaml library decompress.

    {[
    let app = Caravan_compress.middleware () handler
    ]}

    decompress needs checksums from checkseum. The default implementation of
    checkseum has C code: add [checkseum.ocaml] to the libraries of an
    executable for pure OCaml. *)

(** The content codings. [Deflate] is the zlib format of RFC 1950, the HTTP
    coding [deflate]. *)
type coding = Gzip | Deflate

val negotiate : string list -> coding option
(** [negotiate values] is the coding for the values of the [Accept-Encoding]
    fields of a request (RFC 9110 §12.5.3), or [None] for no coding.
    - The elements are [coding] or [coding;q=weight] (with optional white space
      around [;]), separated by commas. The names have no case, and [x-gzip] is
      [gzip]. The first element of a coding wins. [*] gives its weight to each
      coding that the field does not name.
    - The coding with the highest weight above 0 wins, [Gzip] on a tie.
    - No field, or an element that does not parse: [None]. *)

val default_compressible : string -> bool
(** [default_compressible t] is [true] for the media types (lower case, with no
    parameters) that the middleware compresses by default: [text/*] except
    [text/event-stream], [application/json], [application/javascript],
    [application/xml], [image/svg+xml], and each [+json] or [+xml] type. *)

val skip : unit Caravan.Attrs.key
(** A response with this attribute is not compressed, for example a response
    that has a secret and text of the client (BREACH). *)

val middleware :
     ?min_size:int
  -> ?max_size:int
  -> ?max_active:int
  -> ?streams:bool
  -> ?compressible:(string -> bool)
  -> ?level:int
  -> unit
  -> Caravan.Handler.middleware
(** [middleware ()] compresses each response that it can, with the coding of
    {!negotiate}.

    A response goes out with no change if: it is an upgrade, the request is
    [CONNECT] or HTTP/1.0, it has the attribute {!skip}, a [Content-Encoding], a
    [Content-Range], [Cache-Control: no-transform], a digest field
    ([Content-Digest], [Repr-Digest], [Content-MD5]) or a [Trailer] field that
    names one, no [Content-Type] or a media type that [compressible] (default
    {!default_compressible}) refuses, a status of 204, 205 or 206, a known
    length below [min_size] (default 2048) or above [max_size] (default 8 MiB),
    or no known length (a stream) and not [streams].

    The other responses depend on [Accept-Encoding]: they get
    [Vary: Accept-Encoding] (with no duplicate, and not with [Vary: *]). With a
    coding, and a free encoder ([max_active], default 64, work at the same
    time), the response gets [Content-Encoding], no [Content-Length], no
    [Accept-Ranges], and a compressed body. Its strong ETags become weak:
    [W/"x"] for gzip, [W/"x-deflate"] for deflate (the bytes differ, and a cache
    must not give one to a client of the other). With no free encoder, and for a
    304, only the ETags change: a cache keeps one validator for the resource.
    For a client of deflate, the middleware changes each [W/"t-deflate"] of
    [If-None-Match] to [W/"t"] before the handler, thus the handler compares the
    tags of its own bytes. The 304 of caravan.static has the [Content-Length] of
    its file, thus the size limits apply to it as to its 200; a 304 with no
    [Content-Length] is a stream. [level] (0 to 9, default 4) is the level of
    the two codings.

    - [If-Match] never matches a weak ETag: give the routes that use it the
      attribute {!skip}. A handler that checks [If-None-Match] itself must use
      the weak comparison.
    - decompress has no flush of a part of the stream: with [~streams:true], a
      stream gets its compressed bytes in large blocks, never at each
      {!Caravan.Body.Writer.flush}. Thus streams are not compressed by default.
    - The handler must write its body from one fiber at a time: the encoder has
      one state. A middleware before this one (outside it) must write the body
      or close it: the encoder slot is free only then.
    - The cost: about 770 KiB of memory and 0.1 to 0.2 ms for each compressed
      response, and about 17 to 35 MiB/s of input on one core at level 4. The
      encoder yields after each 4 KiB of input, thus the other fibers of the
      domain go on.
    - The digest fields of the trailers ([Content-Digest], [Repr-Digest],
      [Content-MD5]) do not go out: they are of the bytes before the
      compression.
    - BREACH: a compressed response with a secret and text of the client (for
      example a reflected query) can leak the secret over HTTPS. The CSRF tokens
      of caravan.session are masked; for other secrets, use {!skip}.

    @raise Invalid_argument
      if [min_size] is negative, [max_size] is less than [min_size],
      [max_active] is less than 1, or [level] is not in 0 to 9. *)

(** {1:requests Request bodies} *)

val decompress : ?max_bytes:int -> unit -> Caravan.Handler.middleware
(** [decompress ()] decodes the body of a request with [Content-Encoding: gzip]
    (or [x-gzip]) or [deflate] (the zlib format of RFC 1950; raw deflate is not
    [deflate] and gives 400). The names of the codings have no case, and
    [identity] elements are ignored.
    - A request with no coding, a body of known length 0, or a [Content-Range]:
      no change.
    - Another coding, or two codings: [415 Unsupported Media Type] with
      [Accept-Encoding: gzip, deflate], and the body is not read.
    - Else the next handler gets a body that decodes the old one, with no
      [Content-Encoding], no [Content-Length], and no digest field in the fields
      or the trailers. gzip members one after the other are one body (RFC 1952
      §2.2).
    - More than [max_bytes] (default 1 MiB) of decoded bytes in the body, or
      more than [max_bytes] + 64 KiB of compressed bytes: a read raises
      {!Caravan.Body.Too_large} ({!Caravan.Request.form} gives
      [Error Too_large]). A [Content-Length] over that limit gives
      [413 Content Too Large] with no read.
    - A body that does not decode: a read raises an exception of this module.
      The check is strict: an empty body with no known length, zero bytes after
      the last gzip member, or a bad CRC give it.
    - After an error of the decoder, each later read raises it again. If the
      handler lets it out of [next], the middleware gives
      [413 Content Too Large] or [400 Bad Request]. After the response (for
      example in a stream producer), it goes out as other exceptions. An error
      of the old body (the client left, the [max_body] limit of the server) goes
      out with no change: the server maps it.
    - The decoder yields after each 4 KiB of input or output. From the first
      read, a decoded body uses about 60 KiB of memory.

    Put it after [Caravan_middleware.Error_page] in a list of
    {!Caravan.Handler.compose}: else the error page takes the errors of the
    decoder as errors of the handler (500).

    @raise Invalid_argument if [max_bytes] is negative. *)
