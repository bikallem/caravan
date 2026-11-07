(** The HTTP/2 server engine (RFC 9113).

    Three kinds of fibers serve a connection:
    - R, the fiber of the connection, reads the frames and coordinates.
    - W, the writer, is the only fiber that writes the flow.
    - A stream fiber, one for each request, runs the handler and gives the
      frames of the response to W.

    A mutex protects the state that the fibers share. Each critical section
    gives a value and never raises.

    This version does not have these parts yet: the stop of the server, the
    timers of the connection (the preface, a header block, idle), the rules of a
    GOAWAY frame of the client, and the limits of the flood protections.

    A request body error (a content-length rule, malformed trailers) resets the
    stream, and a read of the body raises [Body.Truncated], but the engine does
    not cancel the handler: its fiber stays live, and counts for
    SETTINGS_MAX_CONCURRENT_STREAMS, until the handler ends. *)

val default_local : Caravan.Config.t -> Conn_state.local
(** [default_local config] is the default settings of the server: 100 concurrent
    streams, an initial window of 65535, a header table of 4096 bytes, and
    [config.max_header_bytes] for the header list. *)

(** What the tests can examine after the end of the engine: the connection
    state, the number of the stream records of the engine, the number of the
    live stream fibers, the bytes of the buffers of the unread request DATA of
    all streams, and the number of the scratch buffers of W. *)
type probe =
  { state: Conn_state.t
  ; records: unit -> int
  ; live: unit -> int
  ; inbound: unit -> int
  ; scratch: unit -> int }

val serve :
  ?local:Conn_state.local -> ?inspect:(probe -> unit) -> Caravan.Engine.t
(** [serve ~local ~inspect ctx flow handler] serves the HTTP/2 connection [flow]
    with [handler], and returns when the connection ends. The client must start
    with the connection preface (prior knowledge, or ALPN [h2]). [local] is
    [default_local ctx.config] by default. [Conn_state.validate] must accept it.

    The engine calls [inspect] one time, at its start, with the probe of the
    connection. For the tests. *)
