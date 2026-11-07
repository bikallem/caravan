(* Internal. The rules of a message that do not depend on the HTTP version: the
   content of a response, and the fields that the connection owns. The HTTP/1.1
   engine (H1_write) and the HTTP/2 engine (caravan.http2) share them. *)

(** The type for the content of a response. *)
type content =
  | No_content
      (** No content and no [Content-Length] field: 1xx, 204 and 304 responses,
          2xx responses to [CONNECT], and upgrades. *)
  | Length_only of int
      (** A [Content-Length] field and no content: the response to a [HEAD]
          request whose body length is known, or whose body has no known length
          and whose handler gives a valid [Content-Length] ({!content_length}),
          and a 205 response ([Length_only 0]). *)
  | Length of int  (** Content of this length, with [Content-Length]. *)
  | Unknown_length  (** Content with no known length. *)

val content : meth:Method.t -> Response.t -> content
(** [content ~meth r] is the content of [r] in response to a request with method
    [meth] (RFC 9110 §6.4.1, §8.6, §15.3.6). *)

val content_length : Headers.t -> [`Absent | `Length of int | `Invalid]
(** [content_length h] is the value of the [Content-Length] fields of [h]:
    - [`Absent] if [h] has no [Content-Length] field.
    - [`Length n] if each value is the same number [n] of 1 to 18 digits. A
      value can be a list, for example ["5, 5"].
    - [`Invalid] for each other value. *)

val is_connection_field : string -> bool
(** [is_connection_field name] is [true] iff [name] is the name of a field that
    the connection owns: [content-length], [transfer-encoding], [connection],
    [keep-alive], [proxy-connection], [te] or [upgrade]. [name] must be in
    lowercase. An engine drops the fields of a handler with these names. *)

val is_not_on_304 : string -> bool
(** [is_not_on_304 name] is [true] iff a 304 response does not send the field
    [name]: [content-type], [content-encoding] and [content-language] (RFC 9110
    §15.4.5), and [content-range] (RFC 9110 §14.4). [name] must be in lowercase.
    A cache updates its stored response from a 304. *)
