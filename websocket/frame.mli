(* Internal. WebSocket frames
   ({{:https://www.rfc-editor.org/rfc/rfc6455#section-5.2}RFC 6455 §5.2}),
   without extensions. *)

open Caravan.Private

type opcode = Continuation | Text | Binary | Close | Ping | Pong

type t = { fin : bool; opcode : opcode; payload : string }
(** The type for frames. [payload] is unmasked. *)

type error =
| Protocol_error of string
(** A frame that breaks the protocol. The connection closes with 1002. *)
| Too_big  (** A payload over the limit. The connection closes with 1009. *)

exception Error of error

val is_control : opcode -> bool
(** [is_control op] is [true] for [Close], [Ping] and [Pong]. *)

val read : Buf_reader.t -> masked:bool -> max:int -> t
(** [read r ~masked ~max] reads a frame. The payload of a data frame is at
    most [max] bytes. The payload of a control frame is at most 125 bytes,
    whatever [max] is. A server reads with [~masked:true], a client with
    [~masked:false].

    @raise Error if the frame has a reserved bit or opcode, is a control frame
    that is fragmented or longer than 125 bytes, is masked otherwise than
    [masked] says, or has a 64-bit length with its top bit set
    ([Protocol_error]), or if it is a data frame with a length over [max]
    ([Too_big]). [read] reads nothing after the length then. The payload
    buffer grows as the bytes arrive: before its payload, a declared length
    allocates at most 64 KiB.
    @raise End_of_file if the input ends before the frame does. *)

val write : ?mask:string -> Buf_writer.t -> t -> unit
(** [write ~mask w f] writes [f] with the shortest length encoding, masked
    with the 4-byte key [mask] if given, as a client must.

    @raise Invalid_argument if [mask] is not 4 bytes long, or [f] is a control
    frame that is not final or has more than 125 bytes. *)
