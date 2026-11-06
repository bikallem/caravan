(** Bidirectional byte streams: the flows of weft.

    [Caravan.Flow] is [Weft_flow.Flow] of the library [weft.flow], with
    {!linger}. Thus the type {!t} and the exceptions {!Timeout} and {!Closed}
    are those of [Weft_flow.Flow]. A protocol engine of caravan reads requests
    from a flow and writes responses to it. A TCP connection ([Weft_flow.Tcp])
    and a TLS session ([Weft_tls]) are flows. Upgrade callbacks and [CONNECT]
    tunnels get the raw flow of their connection.

    {!linger} stays in caravan because it is a procedure of HTTP (RFC 9112
    section 9.6). It uses only the public functions of [Weft_flow.Flow]. *)

include module type of struct
  include Weft_flow.Flow
end

val linger : t -> max:int -> unit
(** [linger flow ~max] prepares [flow] for a {!close} when the peer can send
    more input. On Linux and in the simulator, a close of a socket with unread
    input sends a reset. The reset can delete our last bytes before the peer
    reads them. Thus [linger] calls {!shutdown_send}. Then it reads and discards
    the input until the end of the stream, for at most [max] bytes. RFC 9112
    section 9.6 gives this procedure for HTTP.

    [linger] stops at the read deadline of [flow]. Before the shutdown, it sets
    the read timeout of [flow] to [None], because each read of the linger is a
    new call. The flow ends after the linger, so no caller needs the old
    timeout. It ignores {!Timeout} and {!Closed}, but not
    [Weft.Fiber.Cancelled].

    Set the read deadline before the call, and close [flow] after it. No other
    fiber can read [flow] during the call. *)
