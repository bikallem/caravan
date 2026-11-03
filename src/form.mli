(** Queries and [application/x-www-form-urlencoded] forms.

    The parse and the encode follow the
    {{:https://url.spec.whatwg.org/#application/x-www-form-urlencoded}WHATWG URL
     Standard}. {!Request.query_params} and {!Request.form} give the pairs of a
    request. *)

(** The type for the name and value pairs of a query or a form, in the order of
    the input, with duplicates. *)
type t = (string * string) list

val parse : string -> t
(** [parse s] is the pairs of [s]. The parse never fails:
    - The sequences between the [&] characters are the pairs. An empty sequence
      gives no pair.
    - The name is the part before the first [=], and the value is the part after
      it, or [""] if the sequence has no [=].
    - In the name and the value, each [+] becomes a space. Then each [%XX], with
      two hexadecimal digits, becomes its byte. A [%] with no two hexadecimal
      digits after it stays as it is.

    For example, [parse "a=1&a=2&&b"] is [[("a", "1"); ("a", "2"); ("b", "")]],
    and [parse "a+b=%2B%zz"] is [[("a b", "+%zz")]].

    The names and the values are bytes. Caravan does not validate their UTF-8:
    the WHATWG parse replaces incorrect UTF-8 with U+FFFD, and [parse] keeps the
    bytes. *)

val count : string -> int
(** [count s] is the number of pairs of [s], [List.length (parse s)], with no
    decode. For example, a handler can refuse a query with too many pairs before
    the parse. *)

val encode : t -> string
(** [encode l] is the text of the pairs [l], as an HTML form sends them. In each
    name and value, an ASCII letter or digit and the characters [*-._] stay, a
    space becomes [+], and each other byte becomes [%XX], with upper-case
    hexadecimal digits. Each pair gives [name=value], and [&] joins the pairs.
    Thus [parse (encode l)] is [l]. *)

(** {1:lookup Lookup}

    Names are case-sensitive, as in HTML forms. *)

val find : string -> t -> string option
(** [find name l] is the value of the first pair of [l] with the name [name], if
    any. *)

val find_all : string -> t -> string list
(** [find_all name l] is the values of the pairs of [l] with the name [name], in
    order. *)

val mem : string -> t -> bool
(** [mem name l] is [true] iff [l] has a pair with the name [name]. *)

(** {1:body Form bodies} *)

val media_type : string
(** [media_type] is ["application/x-www-form-urlencoded"]. *)

(** The type for the errors of {!Request.form}. *)
type error =
  | Unsupported_media_type
      (** The request does not have exactly one [Content-Type] field with the
          media type {!media_type}. *)
  | Too_large  (** The body has more bytes than the limit. *)
  | Too_many_fields  (** The body has more pairs than the limit. *)

val error_status : error -> Status.t
(** [error_status e] is the status of a response for [e]:
    {!Status.unsupported_media_type} (415) for [Unsupported_media_type], and
    {!Status.content_too_large} (413) for the two others. *)

val pp_error : Format.formatter -> error -> unit
(** [pp_error] formats the name of an error. *)
