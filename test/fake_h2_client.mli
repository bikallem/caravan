(** A fake HTTP/2 client for the tests of the engine: the checks of the hard
    cases.

    It talks to a server over a flow, for example a TCP pair of the simulator.
    It uses {!Caravan_http2.Private.Frame} and an HPACK decoder of its own. A
    reader fiber reads the frames of the server, does the checks, and puts each
    frame in a mailbox. {!next}, {!frames_until} and {!response} take the frames
    from the mailbox.

    The checks on each frame of the server:
    - The first frame is a SETTINGS frame (the server preface).
    - The size of the frame is not more than SETTINGS_MAX_FRAME_SIZE of the
      client, and the framing is correct ({!Caravan_http2.Private.Frame.read}).
    - A DATA frame is in the windows that the client gave: the connection window
      and the stream window.
    - A header block is contiguous: after a HEADERS frame with no END_HEADERS,
      the next frame is a CONTINUATION frame of the same stream.
    - After the END_STREAM flag of the server on a stream, only RST_STREAM,
      WINDOW_UPDATE and PRIORITY frames come on the stream.
    - After a RST_STREAM frame of the server on a stream, no frame comes on the
      stream.
    - No RST_STREAM frame comes on a stream that ended in the two directions:
      the server got the END_STREAM flag of the client, and the END_STREAM flag
      of the server came. The client sends a PING after its END_STREAM flag on a
      DATA frame or on trailers: the server got that flag at the PING ACK.
      Before it, a RST_STREAM NO_ERROR of the server can cross the flag legally.
    - After a {!reset} of the client, no frame of the stream comes after the
      PING ACK that follows the RST_STREAM frame (the barrier).
    - The server sends no PUSH_PROMISE frame, no frame on an even stream, and no
      frame on a stream that the client did not open.

    The reader sends the ACK of each SETTINGS frame of the server. These frames
    do not go in the mailbox: the SETTINGS frames of the server, its SETTINGS
    ACK frames, its WINDOW_UPDATE frames on stream 0 before the PING ACK of
    {!connect}, the PING ACK of a barrier, and the PING ACK after an END_STREAM
    flag of the client.

    A failure of a check raises {!Sim_support.Failed} in the reader fiber. Then
    the reader stops, and each later call of the client raises the same
    exception. *)

open Weft
open Caravan_http2.Private

(** The type for clients. *)
type t

val connect : ?settings:Frame.setting list -> Fiber.scope -> Caravan.Flow.t -> t
(** [connect ~settings s flow] sends the preface, a SETTINGS frame with
    [settings] (none by default) and a PING on [flow], and starts the reader
    fiber in [s]. It waits for the ACK of that PING. The server writes its start
    frames (its SETTINGS frame, and WINDOW_UPDATE frames on stream 0) before
    that ACK: these go in no mailbox, and the WINDOW_UPDATE frames add to the
    connection credit of the server.

    The settings of the client apply to the checks: SETTINGS_MAX_FRAME_SIZE and
    SETTINGS_INITIAL_WINDOW_SIZE. *)

val server_settings : t -> Frame.setting list
(** [server_settings c] is the settings of the first SETTINGS frame of the
    server. *)

val send : t -> Frame.t -> unit
(** [send c f] writes [f] and flushes. A WINDOW_UPDATE frame adds to the windows
    of the checks. A SETTINGS frame with SETTINGS_INITIAL_WINDOW_SIZE changes
    the stream windows of the checks: a larger value at once, a smaller value
    when the SETTINGS ACK of the server comes. *)

val send_all : t -> Frame.t list -> unit
(** [send_all c l] is as {!send} for each frame of [l], with one flush at the
    end: the server reads the frames with no wait between them. *)

val send_raw : t -> string -> unit
(** [send_raw c s] writes the bytes [s] and flushes. The checks do not change.
*)

val request :
  ?end_stream:bool -> t -> stream:int -> (string * string) list -> unit
(** [request ~end_stream c ~stream fields] sends the header block of [fields]
    (HPACK with no dynamic table) on [stream]: a HEADERS frame, then
    CONTINUATION frames if the block is larger than 16384 bytes. [end_stream] is
    [true] by default. *)

val data : ?end_stream:bool -> t -> stream:int -> string -> unit
(** [data ~end_stream c ~stream s] sends [s] in DATA frames of at most 16384
    bytes. It waits for the credit of the server: its windows, from its SETTINGS
    frame and its WINDOW_UPDATE frames. The last frame has END_STREAM if
    [end_stream] ([false] by default). An empty [s] gives one DATA frame.

    @raise Sim_support.Failed if the server reset [stream] meanwhile. *)

val credit : t -> stream:int -> int -> unit
(** [credit c ~stream n] sends a WINDOW_UPDATE frame of [n] on [stream], then on
    stream 0. *)

val next : t -> Frame.t
(** [next c] takes the next frame from the mailbox. It waits for one.

    @raise Sim_support.Failed
      if the server closed the connection and the mailbox is empty, or if a
      check failed. *)

val frames_until : t -> (Frame.t -> bool) -> Frame.t list
(** [frames_until c p] takes the frames from the mailbox, up to the first frame
    [f] with [p f], and gives them in order, with [f]. *)

val take_all : t -> Frame.t list
(** [take_all c] takes all frames of the mailbox, in order. It does not wait:
    settle the simulator first. *)

(** The type for the responses of a stream. *)
type response =
  { interim: (string * string) list list
        (** The header blocks of the interim responses (1xx), in order. *)
  ; head: (string * string) list  (** The fields of the final head. *)
  ; body: string  (** The data of the DATA frames. *)
  ; trailers: (string * string) list  (** The fields of the trailers. *)
  ; reset: Frame.code option
        (** The code of a RST_STREAM frame of the server before the end of the
            stream, if any. *) }

val response : t -> stream:int -> response
(** [response c ~stream] takes from the mailbox the frames of [stream] up to the
    END_STREAM flag of the server or its RST_STREAM frame. The other frames stay
    in the mailbox, in their order. *)

val reset : ?also:Frame.t list -> t -> stream:int -> Frame.code -> unit
(** [reset ~also c ~stream code] sends RST_STREAM [code] on [stream], then the
    frames [also] (none by default, as {!send_all}), then a PING, in one write.
    The frames of [stream] that come before the PING ACK are legal: they were in
    flight. A frame of [stream] after the PING ACK is a failure. The PING ACK
    does not go in the mailbox. *)

val pause_reading : t -> unit
(** [pause_reading c] stops the reads of the client after the frame that the
    reader reads at this time. Thus the writes of the server wait when the
    buffers are full. *)

val resume_reading : t -> unit
(** [resume_reading c] starts the reads again. *)

val close : t -> unit
(** [close c] sends the end of the stream to the server. *)

val finish : t -> unit
(** [finish c] waits for the end of the stream of the server, and for the end of
    the reader fiber.

    @raise Sim_support.Failed if a check failed. *)
