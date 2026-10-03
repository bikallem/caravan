(** Server configuration.

    Start from {!default} and change what you need:
    {[
      let config = { Config.default with max_body = Some (1024 * 1024) }
    ]} *)

type t = {
  addr : Unix.sockaddr;
  (** The address to listen on. Port [0] picks a free port. *)
  backlog : int; (** Length of the queue of pending connections. *)
  max_connections : int;
  (** Connections served at once; more wait to be accepted. *)
  buffer_size : int;
  (** Read and write buffer size per connection, in bytes. Each buffer is
      allocated when first needed; the read buffer grows to hold a long
      request line or header section. *)
  max_request_line : int; (** Longer request lines get 414. *)
  max_header_bytes : int; (** Larger header sections get 431. *)
  max_header_count : int; (** More header fields get 431. *)
  max_body : int option;
  (** Larger request bodies get 413 (or end the connection once the response
      has started). [None] for no limit. *)
  max_drain : int;
  (** At most this many bytes of a request body that the handler did not read
      are discarded to keep the connection open; larger ones close it. *)
  header_timeout : Affect_unix.Mtime.Span.t;
  (** Time allowed to receive a request head, and for a new connection to
      start its first one. A head that is late gets 408. *)
  idle_timeout : Affect_unix.Mtime.Span.t;
  (** Time a kept-alive connection may wait for its next request. *)
  read_timeout : Affect_unix.Mtime.Span.t;
  (** Time allowed for each read of a request body. A late read gets 408 if
      the response has not started yet. *)
  write_timeout : Affect_unix.Mtime.Span.t;
  (** Time allowed for each write of a response. *)
  linger_timeout : Affect_unix.Mtime.Span.t;
  (** Time a connection the server ends keeps reading, at most [max_drain]
      bytes, for the client to close it first (RFC 9112 §9.6). *)
  shutdown_grace : Affect_unix.Mtime.Span.t;
  (** Time in-flight requests get to finish when the server stops. *)
  date_header : bool; (** Whether responses get a [Date] field. *)
  cancel_on_disconnect : bool;
  (** Whether a handler is cancelled when its client closes the connection
      before the response, once the request body has been read. A client
      that only half-closes, sending its request and then ending its side of
      the stream, looks the same: turn this off to serve such clients. *)
  on_error : exn -> Printexc.raw_backtrace -> unit;
  (** Called with exceptions raised by handlers and response bodies. It may
      be called from several domains at once. *)
}
(** The type for server configurations. *)

val default : t
(** [default] listens on [127.0.0.1:8080] with a backlog of 1024 and at most
    1024 connections, 4 KiB buffers, default limits (8 KiB request
    line, 16 KiB and 100 header fields), no body limit, 64 KiB drains,
    timeouts of 10s (head), 30s (idle, read, write), 2s lingering, a 10s
    shutdown grace, a [Date] field, handlers cancelled when their client
    leaves, and errors printed on [stderr]. *)
