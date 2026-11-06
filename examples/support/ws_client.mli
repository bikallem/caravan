(** A small WebSocket client
    ({{:https://www.rfc-editor.org/rfc/rfc6455}RFC 6455}) for the tests of the
    examples. It is for tests only:
    - It sends the same key in each handshake, and masks each frame with the
      same key. A browser makes a new random key for each.
    - It does not join fragments: a fragmented message is a [Failure].
    - It does not answer a Ping or a Close frame. The test does that, thus it
      can examine each frame. *)

(** The type for connections. *)
type t

(** The server did not accept the handshake: the status code of its response. *)
exception Refused of int

val connect :
     ?origin:string
  -> ?protocols:string list
  -> ?host:string
  -> Weft.Net.addr
  -> string
  -> t
(** [connect addr path] connects to the server at [addr] with
    [Caravan.Tcp.connect], and sends the handshake for [path].
    - [origin] is the [Origin] field, as a browser page sends it. By default,
      the handshake has no [Origin], as a client that is not a browser.
    - [protocols] are the subprotocols for [Sec-WebSocket-Protocol], in order of
      preference. By default, the handshake has none.
    - [host] is the [Host] field. By default, it is [addr].

    @raise Refused if the status of the response is not [101].
    @raise Failure if the [Sec-WebSocket-Accept] field is not correct. *)

val protocol : t -> string option
(** [protocol c] is the subprotocol that the server chose for [c], if one. *)

(** The type for frames. *)
type frame =
  | Text of string
  | Binary of string
  | Close of int * string
      (** The code and the reason. A Close frame with no code has the code 1005.
      *)
  | Ping of string
  | Pong of string

val send : t -> frame -> unit
(** [send c f] sends [f] masked, as a client must. *)

val recv : t -> frame
(** [recv c] is the next frame from the server.

    @raise End_of_file
      when the server closes or resets the TCP connection ([Caravan.Flow.Closed]
      of a read).
    @raise Failure if the frame is masked or is a part of a fragmented message.
*)

val close : t -> unit
(** [close c] closes the TCP connection, with no Close frame. *)
