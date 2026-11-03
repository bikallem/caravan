(** [multipart/form-data] bodies, for example the file uploads of an HTML form
    ({{:https://www.rfc-editor.org/rfc/rfc7578}RFC 7578}, with the syntax of
    {{:https://www.rfc-editor.org/rfc/rfc2046#section-5.1}RFC 2046 §5.1}).

    {!fold} streams the parts of a body, and {!read_all} keeps them in memory.
    {!Request.multipart} and {!Request.multipart_form} check the [Content-Type]
    of a request first. *)

val media_type : string
(** [media_type] is ["multipart/form-data"]. *)

val boundary : string -> string option
(** [boundary v] is the [boundary] parameter of the [Content-Type] value [v], if
    it is there one time and is correct: 1 to 70 characters of the [bchars] of
    RFC 2046, with no space at the end. The value is a token or a quoted string.
*)

(** {1:parts Parts} *)

(** The type for the parts of a body. A part is valid only in the callback of
    {!fold}. *)
type part

val name : part -> string
(** [name p] is the [name] parameter of the [Content-Disposition] field of [p].
    The client gives it: the HTML Standard encodes a double quote, CR and LF as
    [%22], [%0D] and [%0A], and caravan does not decode them. *)

val filename : part -> string option
(** [filename p] is the [filename] parameter of the [Content-Disposition] field
    of [p], if any, as {!name} gives it. The client gives it: it is not a safe
    path. It can contain [..], [/] or [\ ], be an absolute path, have a drive
    letter, be the full path of an old browser, or be a reserved name of an
    operating system. *)

val content_type : part -> string option
(** [content_type p] is the [Content-Type] field of [p], if any. [None] means
    [text/plain] (RFC 7578 §4.4). The client gives it: do not send a file back
    with this type. *)

val headers : part -> Headers.t
(** [headers p] are the fields of the head of [p]. *)

val body : part -> Body.t
(** [body p] is the content of [p]. It is valid only in the callback of {!fold}:
    copy it to keep it. If the body of the request ends before the next
    delimiter, or if the bytes after a boundary are not correct, its read raises
    [Body.Truncated]. Thus a copy of a part does not end as if it were complete.
*)

(** {1:errors Errors} *)

(** The type for the errors of a parse. *)
type error =
  | Unsupported_media_type
      (** The request does not have exactly one [Content-Type] field with the
          media type {!media_type} and a correct {!boundary}. *)
  | Malformed of string
      (** The syntax of the body is not correct. The text tells the rule, and
          contains no bytes of the body. *)
  | Too_many_parts  (** The body has more parts than the limit. *)
  | Header_too_large  (** The head of a part has more bytes than the limit. *)
  | Too_large  (** The parts have more bytes than the limit of {!read_all}. *)

val error_status : error -> Status.t
(** [error_status e] is the status of a response for [e]:
    {!Status.unsupported_media_type} (415) for [Unsupported_media_type],
    {!Status.bad_request} (400) for [Malformed], and {!Status.content_too_large}
    (413) for the others. *)

val pp_error : Format.formatter -> error -> unit
(** [pp_error] formats an error. *)

(** {1:fold The streaming parse} *)

val fold :
     ?max_parts:int
  -> ?max_header_bytes:int
  -> ?buffer_size:int
  -> boundary:string
  -> Body.t
  -> init:'a
  -> ('a -> part -> 'a)
  -> ('a, error) result
(** [fold ~boundary b ~init f] reads the parts of the body [b] and gives
    [f (... (f init p1) ...) pn]. It keeps at most [buffer_size] bytes of the
    body and the head of one part (default 16384).

    - When [f] returns, [fold] discards the rest of the part. A later read of
      the body of the part raises [Invalid_argument]. If a read of the body of
      the part is in progress when [f] returns, for example in a fiber that [f]
      started, [fold] raises [Invalid_argument].
    - An exception of [f], also a cancel, goes out of [fold], with no more
      reads. But if [f] lets out the [Body.Truncated] that a read of the part
      raised for an error of the syntax, [fold] gives [Error (Malformed _)]. If
      [f] reads only a part of the body, the discard can find the error: then
      [fold] also gives [Error (Malformed _)].
    - An exception of [b], for example [Body.Truncated], goes out of [fold]: of
      the read of the part in [f], and of [fold] after [f].
    - Before the callback of a part, [fold] reads the head and the next
      [String.length boundary + 2] bytes, to find an empty part with no CRLF
      before its delimiter (RFC 2046). An exception of [b] or an error of the
      syntax in these bytes ends [fold] before the callback.

    The limits:
    - [max_parts] (default 1000): more parts give [Error Too_many_parts], before
      the callback of the part after the limit.
    - [max_header_bytes] (default 16384): the spaces and tabs after a boundary,
      the lines of the head with their CRLF, and the empty line. More give
      [Error Header_too_large]. The preamble (before the first boundary) also
      has this limit, and gives [Error (Malformed _)]. After the last boundary,
      [fold] reads at most this number of bytes, and the bytes of one more read
      of its buffer. Then the server discards the rest of the body.

    The parse is linear in the length of the body. Call [fold] one time: the
    state of [b] after an error is not specified.

    @raise Invalid_argument
      if a limit is negative, if [boundary] is not correct (see {!boundary}), or
      if [buffer_size] is less than [2 * (4 + String.length boundary) + 4]. *)

(** {1:buffered The buffered parse} *)

(** The type for the parts of {!read_all} and {!encode}. *)
type field =
  { name: string
  ; filename: string option
  ; content_type: string option
  ; data: string }

val read_all :
     ?max_parts:int
  -> ?max_header_bytes:int
  -> ?max_bytes:int
  -> boundary:string
  -> Body.t
  -> (field list, error) result
(** [read_all ~boundary b] is the parts of [b], in order, as {!fold} reads them.
    [max_bytes] (default 8 MiB, the default of [Config.max_body]) counts the
    head and the data of each part: more give [Error Too_large], with no more
    reads. The names and the heads of many empty parts also use memory. *)

val find : string -> field list -> field option
(** [find name l] is the first field of [l] with the name [name], if any. *)

val encode : field list -> string * string
(** [encode l] is a boundary and the text of a body with the fields [l], as an
    HTML form sends them. The boundary is [caravan-] and a short binary number
    that no [CRLF--caravan-] of the data has after it: thus no data contains a
    delimiter, and the boundary has at most 70 characters. In a name and a file
    name, a double quote, CR and LF become [%22], [%0D] and [%0A].

    @raise Invalid_argument
      if a [content_type], or the [Content-Disposition] field of a name and a
      file name, is not a valid field value, for example with a control
      character. *)
