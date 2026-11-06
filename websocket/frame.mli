(* Internal. WebSocket frames
   ({{:https://www.rfc-editor.org/rfc/rfc6455#section-5.2}RFC 6455 §5.2}),
   without extensions. *)

type opcode = Continuation | Text | Binary | Close | Ping | Pong

(** The type for frames. [payload] is unmasked. *)
type t = {fin: bool; opcode: opcode; payload: string}

type error =
  | Protocol_error of string
      (** A frame that breaks the protocol. The connection closes with 1002. *)
  | Too_big  (** A payload over the limit. The connection closes with 1009. *)

exception Error of error

val is_control : opcode -> bool
(** [is_control op] is [true] for [Close], [Ping] and [Pong]. *)

val read :
     ?on_data:(opcode -> bytes -> int -> int -> unit)
  -> Weft_flow.Reader.t
  -> masked:bool
  -> max:int
  -> t
(** [read ~on_data r ~masked ~max] reads a frame. The payload of a data frame is
    at most [max] bytes. The payload of a control frame is at most 125 bytes,
    whatever [max] is. A server reads with [~masked:true], a client with
    [~masked:false].

    For a data frame, [on_data opcode buf off len] gets each part of the payload
    that a read gives, unmasked, in order, before the next read: the [len] bytes
    of [buf] from [off], at most valid until [on_data] returns. The parts are
    not empty and make the whole payload. An exception from [on_data] goes out
    of [read]: the frame is then not read to its end.

    [read] consumes the header from [r] only when all of it came. Thus if [read]
    raises in the header, for example for a cancel or a timeout, it consumed
    nothing of [r], and a later [read] reads the same frame.

    @raise Error
      if the frame has a reserved bit or opcode, is a control frame that is
      fragmented or longer than 125 bytes, is masked otherwise than [masked]
      says, or has a 64-bit length with its top bit set ([Protocol_error]), or
      if it is a data frame with a length over [max] ([Too_big]). [read] does
      not wait for the bytes after the length then. The payload buffer grows as
      the bytes arrive: before its payload, a declared length allocates at most
      64 KiB.
    @raise End_of_file if the input ends before the frame does. *)

val write : ?mask:string -> Weft_flow.Writer.t -> t -> unit
(** [write ~mask w f] writes [f] with the shortest length encoding, masked with
    the 4-byte key [mask] if given, as a client must.

    @raise Invalid_argument
      if [mask] is not 4 bytes long, or [f] is a control frame that is not final
      or has more than 125 bytes. *)
