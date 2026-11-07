(** The messages of HTTP/2 (RFC 9113 §8): the fields of a request head to a
    request, and a response to its fields.

    The functions do no I/O and keep no state. The fields are the decoded fields
    of a header block ({!Hpack}), in their order. *)

open Caravan

(** {1:requests Requests} *)

(** The type for the head of a request. The engine makes the
    {!Caravan.Request.t} with
    [Request.make ~version:Http_2 ~scheme ?authority ~headers ~body ~conn meth
     path], where [scheme] is the scheme of the connection, as in HTTP/1.1. *)
type request_head =
  { meth: Method.t  (** The method, from [:method]. *)
  ; authority: string option
        (** The authority, from [:authority], else from the [host] field, if
            any. {!Caravan.Request.is_valid_authority} accepts it. With no
            [:authority], an empty [host] field gives [None], as in HTTP/1.1. *)
  ; path: string
        (** The path, from [:path], or [""] for [CONNECT]. It has only the bytes
            0x21 to 0x7e, as an HTTP/1.1 request target. [Request.make] accepts
            it. *)
  ; headers: Headers.t
        (** The regular fields, in their order, with names in lowercase. The
            [cookie] fields are one field at the position of the first, with
            ["; "] between the values (RFC 9113 §8.2.3). An empty [cookie] value
            adds nothing. *)
  ; content_length: int option  (** The value of [content-length], if any. *)
  ; expect: [`None | `Continue | `Unknown]
        (** The expectation of the request (RFC 9110 §10.1.1), from the elements
            [Headers.tokens "expect" headers], as in HTTP/1.1:
            - [`None] if there is no element, also with no [expect] field.
            - [`Continue] if each element is [100-continue] (case-insensitive).
              The engine sends a [100 Continue] before it gives the body to the
              handler.
            - [`Unknown] for each other value. The engine sends a 417, as
              HTTP/1.1 does. *) }

(** The type for the refusals of a request. *)
type refusal =
  | Malformed of string
      (** A malformed request (RFC 9113 §8.1.1): the engine sends RST_STREAM
          PROTOCOL_ERROR. The text tells the rule. It can hold a field name, but
          never a field value: a value can be a secret. *)
  | Misdirected
      (** A [:scheme] other than [http] and [https]: the engine sends a 421. *)

val request : (string * string) list -> (request_head, refusal) result
(** [request fields] is the head of the request with the fields [fields] (RFC
    9113 §8.2, §8.3).

    [request] accepts a [:scheme] of [http] and of [https] (case-insensitive) on
    each connection: a proxy that ends TLS and forwards h2c sends [https]. A
    [:scheme] with another value gives [Error Misdirected], only if no rule of
    [Malformed] applies.

    It gives [Error (Malformed reason)] for:
    - A pseudo-field after a regular field, an unknown pseudo-field, a [:status]
      field, or a pseudo-field two times.
    - A field name with an uppercase letter.
    - A name or a value that {!Caravan.Headers.of_list} refuses: a name that is
      not a token, or a value that {!Caravan.Headers.is_valid_value} refuses.
      Thus a value with a space or a tab at its start or end is malformed. RFC
      9113 §8.2.1 permits this check, which is stricter than its own. The text
      is ["an invalid field name or value"], because the text of
      [Invalid_argument] holds the value.
    - A connection-specific field: [connection], [keep-alive],
      [proxy-connection], [transfer-encoding] or [upgrade]. A [te] field with a
      value other than [trailers] (case-insensitive).
    - No [:method], or a [:method] that is not a token.
    - For a method other than [CONNECT]: no [:scheme], no [:path], an empty
      [:path], a [:path] with a byte that is not 0x21 to 0x7e (the bytes of an
      HTTP/1.1 request target), or a [:path] that does not start with ['/'],
      except [*] for [OPTIONS].
    - For [CONNECT]: a [:scheme] or a [:path], no [:authority], or an
      [:authority] that is not [host:port], as in HTTP/1.1.
    - An authority that {!Caravan.Request.is_valid_authority} refuses, for
      example one with userinfo.
    - A [host] field that is not equal to the authority. The compare ignores the
      case of the ASCII letters. With no [:authority], the first [host] field
      gives the authority, and each other [host] field must be equal to it.
    - [content-length] values that are not all the same number of 1 to 18 digits
      ({!Caravan.Private.Message_rules.content_length}).

    [request] never raises. *)

val trailers : (string * string) list -> (Headers.t, string) result
(** [trailers fields] is the trailers [fields] of a request. It gives
    [Error reason] for a pseudo-field, a connection-specific field, a [te] field
    with a value other than [trailers], a name with an uppercase letter, a name
    that is not a token, or a value that {!Caravan.Headers.is_valid_value}
    refuses. The engine then sends RST_STREAM PROTOCOL_ERROR. [trailers] never
    raises. *)

(** {1:responses Responses} *)

(** The type for the body of a response on the stream. *)
type body =
  | No_body  (** No DATA: the head ends the stream. *)
  | Length of int  (** A body of this length. *)
  | Unknown_length  (** A body with no known length. *)

(** The type for what the engine does with a response. *)
type plan =
  | Send of (string * string) list * body
      (** Send the head with these fields, then the body. *)
  | Fail_500 of string
      (** Do not send the response: abandon it ({!Caravan.Response.abandon}),
          send a 500 and report the text. *)

val response : meth:Method.t -> date:string option -> Response.t -> plan
(** [response ~meth ~date r] is the plan for [r], in response to a request with
    the method [meth]:
    - A response that {!Caravan.Response.upgrade} made gives [Fail_500]: the
      engine has no upgrades and no tunnels.
    - A 2xx response to [CONNECT] gives [Fail_500], as in HTTP/1.1.
    - Else [Send (fields, body)]. The fields are [:status] first, then [date] if
      [date] is [Some d] and no [date] field of [r] stays, then the fields of
      [r] that stay, in their order, with names in lowercase, then
      [content-length] if the content has a length.

    [response] drops these fields of [r]: the connection-specific fields
    ({!Caravan.Private.Message_rules.is_connection_field}, thus also
    [content-length]), the fields that the [connection] field of [r] names, and
    on a 304 the fields of {!Caravan.Private.Message_rules.is_not_on_304}.

    {!Caravan.Private.Message_rules.content} decides [content-length] and the
    body, as in HTTP/1.1: no body for 204, 205 (with [content-length: 0]) and
    304, and no body for [HEAD], with [content-length] if the length is known.
    The engine closes a body that it does not send, thus its hook runs. The
    engine gives [Some date] if [config.date_header] is [true]. *)

val interim : int -> (string * string) list
(** [interim code] are the fields of an interim response with status [code], for
    example [interim 100] for a [100 Continue].

    @raise Invalid_argument
      if [code] is not 100 to 199, or is 101: HTTP/2 has no 101 (RFC 9113 §8.6).
*)
