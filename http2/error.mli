(** The error codes of HTTP/2 (RFC 9113 section 7), and the exceptions that more
    than one module of the library raises. *)

(** The type for error codes. *)
type code =
  | No_error
  | Protocol_error
  | Internal_error
  | Flow_control_error
  | Settings_timeout
  | Stream_closed
  | Frame_size_error
  | Refused_stream
  | Cancel
  | Compression_error
  | Connect_error
  | Enhance_your_calm
  | Inadequate_security
  | Http_1_1_required
  | Unknown_code of int
      (** A code that RFC 9113 does not define, 0 to [2^32 - 1], as received.
          RFC 9113 section 7: an unknown code has the same meaning as
          [INTERNAL_ERROR]. *)

val code_to_int : code -> int
(** [code_to_int c] is the 32-bit value of [c]. *)

val code_of_int : int -> code
(** [code_of_int n] is the code with the value [n]. A value that RFC 9113 does
    not define gives [Unknown_code n].

    @raise Invalid_argument if [n] is not in the range 0 to [2^32 - 1]. *)

val pp_code : Format.formatter -> code -> unit
(** [pp_code] prints a code with its RFC name, for example [PROTOCOL_ERROR], or
    [0x%x] for an unknown code. *)

(** The HPACK decoder (RFC 7541) raises it for a decoding error, which is a
    connection error of type [COMPRESSION_ERROR] (RFC 9113 section 4.3). The
    text tells the cause. *)
exception Hpack of string
