(** The test helpers of caravan.http2 that more than one suite uses: the texts
    of the frames and the frames built by hand. *)

open Caravan_http2.Private

val show_code : Error.code -> string
(** [show_code c] is the RFC name of [c], for example ["PROTOCOL_ERROR"]. *)

val flags : (bool * string) list -> string
(** [flags l] is the names of the flags of [l] that are set, each after a space.
    For example [flags [(true, "ACK")]] is [" ACK"]. *)

val show : Frame.t -> string
(** [show f] is a text of one line for [f], with its type, its stream, its flags
    and its fields. For example [DATA 1 END_STREAM "ab" flow_length=2]. *)

val frame : Frame.t Windtrap.Testable.t
(** [frame] compares two frames, and prints them with {!show}. *)

val raw :
  ?length:int -> kind:int -> ?flags:int -> stream:int -> string -> string
(** [raw ~length ~kind ~flags ~stream payload] is the bytes of a frame built by
    hand, with no help from {!Frame}: the header of 9 bytes, then [payload].
    [length] is the length of [payload] by default. [flags] is 0 by default. *)
