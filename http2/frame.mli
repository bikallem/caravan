(** The frames of HTTP/2
    ({{:https://www.rfc-editor.org/rfc/rfc9113#section-4}RFC 9113 section 4} and
    {{:https://www.rfc-editor.org/rfc/rfc9113#section-6}section 6}).

    This module reads and writes the frames on a {!Weft_flow.Reader.t} and a
    {!Weft_flow.Writer.t}. It does the checks of the framing only: the size of a
    frame, the stream of a frame type, and the fixed lengths of the payloads.
    The rules of the streams and of the connection state are not in this module.
*)

(** The type for error codes: {!Error.code}. *)
type code = Error.code

(** The priority fields of a HEADERS or a PRIORITY frame (RFC 9113 section 6.3).
    RFC 9113 deprecates the priority scheme. The engine reads these fields only
    for the checks. *)
type priority =
  { exclusive: bool  (** The E flag. *)
  ; dependency: int  (** The stream dependency, 31 bits. *)
  ; weight: int
        (** The value of the Weight field, 0 to 255. The weight of the stream is
            [weight + 1]. *) }

(** A setting: the identifier (16 bits) and the value (32 bits), as received.
    This module does not interpret the values. *)
type setting = int * int

(** The type for frames. In each frame, [stream] is the stream identifier, 31
    bits. [read] ignores the reserved bit. *)
type t =
  | Data of {stream: int; end_stream: bool; data: string; flow_length: int}
      (** A DATA frame. [data] is the content without the padding. [flow_length]
          is the length of the full payload, with the Pad Length byte and the
          padding. Flow control counts this length (RFC 9113 section 6.9.1). *)
  | Headers of
      { stream: int
      ; end_stream: bool
      ; end_headers: bool
      ; priority: priority option
      ; block: string }
      (** A HEADERS frame. [block] is the field block fragment, without the
          padding. [priority] is [Some] when the PRIORITY flag is set. *)
  | Priority of {stream: int; priority: priority}  (** A PRIORITY frame. *)
  | Rst_stream of {stream: int; code: code}  (** A RST_STREAM frame. *)
  | Settings of {ack: bool; settings: setting list}
      (** A SETTINGS frame, on stream 0. A frame with [ack] has no settings.
          [settings] has only the known identifiers, in the order of the
          payload: 1 to 6 (RFC 9113 section 6.5.2) and 8
          (SETTINGS_ENABLE_CONNECT_PROTOCOL, RFC 8441). [read] ignores the other
          identifiers, as RFC 9113 section 6.5.2 tells. *)
  | Push_promise of {stream: int}
      (** A PUSH_PROMISE frame. [read] does not decode its payload: a server
          refuses each PUSH_PROMISE. *)
  | Ping of {ack: bool; data: string}
      (** A PING frame, on stream 0. [data] has 8 bytes. *)
  | Goaway of {last_stream: int; code: code; debug: string}
      (** A GOAWAY frame, on stream 0. [last_stream] has 31 bits: [read] ignores
          the reserved bit. *)
  | Window_update of {stream: int; increment: int}
      (** A WINDOW_UPDATE frame. [increment] has 31 bits: [read] ignores the
          reserved bit. An increment of 0 is a rule of the connection state, not
          of the framing. *)
  | Continuation of {stream: int; end_headers: bool; block: string}
      (** A CONTINUATION frame. *)
  | Unknown of {kind: int; stream: int}
      (** A frame of a type that RFC 9113 does not define. [kind] is the type.
          [read] consumes its payload. RFC 9113 section 4.1: an endpoint ignores
          it. *)
  | Malformed of {stream: int; code: code; reason: string}
      (** A stream error of the framing with [code] (RFC 9113 section 5.4.2).
          [read] consumed the frame, thus the next [read] gives the next frame.
          The engine resets the stream only if the stream is not idle: RFC 9113
          section 6.4 forbids a RST_STREAM frame on an idle stream. The
          connection state decides the idle case. The text tells the cause. *)

(** {!read} raises it for a connection error of the framing (RFC 9113 section
    5.4.1). [read] consumed nothing then. The connection must end with a GOAWAY
    frame with [code]. The text tells the cause. *)
exception Connection_error of {code: code; reason: string}

val preface : string
(** [preface] is the start of the client connection preface (RFC 9113 section
    3.4): [PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n], 24 bytes. A SETTINGS frame comes
    after it. *)

val read : Weft_flow.Reader.t -> max_size:int -> t
(** [read r ~max_size] reads the next frame from [r]. [max_size] is the maximum
    payload length, the value of SETTINGS_MAX_FRAME_SIZE that the engine sent. A
    reader with a [size] of less than [max_size + 9] bytes needs more than one
    read of the flow for each large frame. The engine uses a size of 65536
    bytes.

    [read] consumes a frame only when all of its bytes came. Thus if [read]
    raises before the end of a frame, for example for a cancel or a timeout, it
    consumed nothing of [r], and a later [read] reads the same frame.

    [read] gives {!Malformed}, and consumes the frame, for:
    - A PRIORITY frame with a payload of a length other than 5
      ([FRAME_SIZE_ERROR]).
    - A PRIORITY frame that depends on its own stream ([PROTOCOL_ERROR]).

    [read] ignores the flags that a frame type does not define.

    @raise Connection_error
      with [FRAME_SIZE_ERROR] for:
      - A frame longer than [max_size].
      - A PADDED frame with no Pad Length byte.
      - A HEADERS frame with PRIORITY and less than 5 bytes after the Pad Length
        byte.
      - A RST_STREAM or WINDOW_UPDATE frame with a payload other than 4 bytes.
      - A PING frame with a payload other than 8 bytes.
      - A SETTINGS frame with ACK and a payload.
      - A SETTINGS frame with a payload that is not a multiple of 6 bytes.
      - A GOAWAY frame with less than 8 bytes.
    @raise Connection_error
      with [PROTOCOL_ERROR] for:
      - A DATA, HEADERS, PRIORITY, RST_STREAM, PUSH_PROMISE or CONTINUATION
        frame on stream 0.
      - A SETTINGS, PING or GOAWAY frame on another stream.
      - A padding that is longer than the rest of the payload.
    @raise End_of_file
      if the input ends before the frame does. [read] raises it also at an end
      between two frames: then [Weft_flow.Reader.buffered r] is [0].
    @raise Weft_flow.Flow.Timeout
      and the other exceptions of {!Weft_flow.Reader.ensure}. *)

val write : Weft_flow.Writer.t -> t -> unit
(** [write w f] writes the frame [f] to [w]. It does not flush [w], but [w] can
    write the bytes before [f] to its flow (see
    {!Weft_flow.Writer.start_frame}). It writes no padding: it ignores the
    [flow_length] of a [Data] frame. It does not compare the length of the
    payload with the SETTINGS_MAX_FRAME_SIZE of the peer: the caller makes sure
    that it is correct.

    Of the integer fields, [write] checks only the range of [stream]. A value
    that does not fit in the bytes of its field raises [Invalid_argument].
    [write] writes the bit 31 of [dependency], [last_stream] and [increment] as
    given.

    If [write] raises, [w] holds no part of [f].

    @raise Invalid_argument
      for:
      - [Push_promise], [Unknown] and [Malformed].
      - A stream identifier that is not in the range 0 to [2^31 - 1].
      - A DATA, HEADERS, PRIORITY, RST_STREAM or CONTINUATION frame on stream 0.
      - A SETTINGS frame with [ack] and settings.
      - PING data that does not have 8 bytes.
      - A payload of [2^24] bytes or more.
    @raise Weft_flow.Writer.Too_large
      and the other exceptions of the writes of {!Weft_flow.Writer}. *)

val write_data :
     Weft_flow.Writer.t
  -> stream:int
  -> end_stream:bool
  -> bytes
  -> int
  -> int
  -> unit
(** [write_data w ~stream ~end_stream buf off len] writes a DATA frame with the
    [len] bytes of [buf] from [off], and with no padding. It is as {!write} with
    [Data], but it does not copy [buf] into a string.

    @raise Invalid_argument
      if [off] and [len] are not a valid range of [buf], and as {!write}. *)

val write_header_block :
     Weft_flow.Writer.t
  -> stream:int
  -> end_stream:bool
  -> max_size:int
  -> string
  -> unit
(** [write_header_block w ~stream ~end_stream ~max_size block] writes the field
    block [block] in a HEADERS frame, then in CONTINUATION frames as necessary.
    Each frame has a payload of [max_size] bytes, except the last one, which has
    the remaining bytes. The last frame has END_HEADERS. Only the HEADERS frame
    has END_STREAM, if [end_stream]. An empty block gives one HEADERS frame with
    no payload. The HEADERS frame has no priority and no padding.

    If [write_header_block] raises, the frames before the frame that raised stay
    in [w].

    @raise Invalid_argument if [max_size < 1], and as {!write}. *)
