(* Internal. Errors in HTTP/1.1 requests, and the responses they get. *)

(** The type for request errors. *)
type t =
  | Bad_request of string  (** 400; the string says what was wrong, for logs. *)
  | Uri_too_long  (** 414: the request line is too long. *)
  | Header_fields_too_large  (** 431 *)
  | Content_too_large  (** 413 *)
  | Request_timeout  (** 408: the request did not arrive in time. *)
  | Expectation_failed  (** 417: an [Expect] other than [100-continue]. *)
  | Misdirected_request
      (** 421: a target with another scheme than the connection's. *)
  | Not_implemented of string
      (** 501, for example an unknown transfer coding. *)
  | Version_not_supported  (** 505 *)

(** The exception raised by the HTTP/1.1 parsers. *)
exception E of t

val status : t -> Status.t
(** [status e] is the response status for [e]. *)

val pp : Format.formatter -> t -> unit
(** [pp] formats an error as its status and, if any, its detail. *)
