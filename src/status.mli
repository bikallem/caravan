(** HTTP status codes
    ({{:https://www.rfc-editor.org/rfc/rfc9110#section-15} RFC 9110 §15}). *)

(** The type for status codes. A value is always in the range \[[100];[599]\].
*)
type t = private int

val v : int -> t
(** [v n] is the status code [n].

    @raise Invalid_argument if [n] is not in the range \[[100];[599]\]. *)

val code : t -> int
(** [code s] is the integer value of [s]. *)

val reason : t -> string
(** [reason s] is the standard reason phrase of [s], for example ["Not Found"]
    for [404]. It is [""] for codes that are not registered. *)

val kind :
  t -> [`Informational | `Success | `Redirection | `Client_error | `Server_error]
(** [kind s] is the class of [s], given by its first digit
    ({{:https://www.rfc-editor.org/rfc/rfc9110#section-15}RFC 9110 §15}). *)

val equal : t -> t -> bool
(** [equal s s'] is [true] iff [s] and [s'] are the same code. *)

val compare : t -> t -> int
(** [compare] orders status codes numerically. *)

val pp : Format.formatter -> t -> unit
(** [pp] formats a status code with its reason phrase, if any, for example
    [404 Not Found]. *)

(** {1:informational 1xx Informational} *)

val continue : t
(** [100 Continue] *)

val switching_protocols : t
(** [101 Switching Protocols] *)

val early_hints : t
(** [103 Early Hints] (RFC 8297) *)

(** {1:success 2xx Success} *)

val ok : t
(** [200 OK] *)

val created : t
(** [201 Created] *)

val accepted : t
(** [202 Accepted] *)

val non_authoritative_information : t
(** [203 Non-Authoritative Information] *)

val no_content : t
(** [204 No Content] *)

val reset_content : t
(** [205 Reset Content] *)

val partial_content : t
(** [206 Partial Content] *)

(** {1:redirection 3xx Redirection} *)

val multiple_choices : t
(** [300 Multiple Choices] *)

val moved_permanently : t
(** [301 Moved Permanently] *)

val found : t
(** [302 Found] *)

val see_other : t
(** [303 See Other] *)

val not_modified : t
(** [304 Not Modified] *)

val temporary_redirect : t
(** [307 Temporary Redirect] *)

val permanent_redirect : t
(** [308 Permanent Redirect] *)

(** {1:client_error 4xx Client error} *)

val bad_request : t
(** [400 Bad Request] *)

val unauthorized : t
(** [401 Unauthorized] *)

val payment_required : t
(** [402 Payment Required] *)

val forbidden : t
(** [403 Forbidden] *)

val not_found : t
(** [404 Not Found] *)

val method_not_allowed : t
(** [405 Method Not Allowed] *)

val not_acceptable : t
(** [406 Not Acceptable] *)

val proxy_authentication_required : t
(** [407 Proxy Authentication Required] *)

val request_timeout : t
(** [408 Request Timeout] *)

val conflict : t
(** [409 Conflict] *)

val gone : t
(** [410 Gone] *)

val length_required : t
(** [411 Length Required] *)

val precondition_failed : t
(** [412 Precondition Failed] *)

val content_too_large : t
(** [413 Content Too Large] *)

val uri_too_long : t
(** [414 URI Too Long] *)

val unsupported_media_type : t
(** [415 Unsupported Media Type] *)

val range_not_satisfiable : t
(** [416 Range Not Satisfiable] *)

val expectation_failed : t
(** [417 Expectation Failed] *)

val misdirected_request : t
(** [421 Misdirected Request] *)

val unprocessable_content : t
(** [422 Unprocessable Content] *)

val upgrade_required : t
(** [426 Upgrade Required] *)

val precondition_required : t
(** [428 Precondition Required] (RFC 6585) *)

val too_many_requests : t
(** [429 Too Many Requests] (RFC 6585) *)

val request_header_fields_too_large : t
(** [431 Request Header Fields Too Large] (RFC 6585) *)

val unavailable_for_legal_reasons : t
(** [451 Unavailable For Legal Reasons] (RFC 7725) *)

(** {1:server_error 5xx Server error} *)

val internal_server_error : t
(** [500 Internal Server Error] *)

val not_implemented : t
(** [501 Not Implemented] *)

val bad_gateway : t
(** [502 Bad Gateway] *)

val service_unavailable : t
(** [503 Service Unavailable] *)

val gateway_timeout : t
(** [504 Gateway Timeout] *)

val http_version_not_supported : t
(** [505 HTTP Version Not Supported] *)

val network_authentication_required : t
(** [511 Network Authentication Required] (RFC 6585) *)
