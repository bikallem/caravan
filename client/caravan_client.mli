(** An HTTP/1.1 client for caravan (work in progress). *)

module Config : sig
  type t = Config.t = {
    connect_timeout : Affect_unix.Mtime.Span.t;
    (** The time limit of the lookup and all the connects. *)
    response_timeout : Affect_unix.Mtime.Span.t;
    (** The time limit from the end of the request to the end of the
        response head. *)
    read_timeout : Affect_unix.Mtime.Span.t; (** The time limit of each read of the body. *)
    write_timeout : Affect_unix.Mtime.Span.t; (** The time limit of each write. *)
    idle_timeout : Affect_unix.Mtime.Span.t;
    (** The time that an idle connection stays in the pool. *)
    max_idle_per_host : int; (** The idle connections for each host and port. *)
    max_status_line : int; (** The size limit of the status line. *)
    max_header_bytes : int; (** The size limit of the response fields. *)
    max_header_count : int; (** The limit of the number of response fields. *)
  }

  val default : t
  (** [default] has the limits 10 s (connect), 30 s (response, read,
      write), 5 s (idle), 8 idle connections, 8 KiB, 64 KiB and 200
      fields. *)
end

type error = Error.t =
| Connect of string (** The connect failed, with the cause. *)
| Timeout of [ `Connect | `Response | `Read | `Write ] (** A time limit. *)
| Closed
(** The connection ended before a byte of the response came. A retry of
    an idempotent request is safe. *)
| Truncated (** The response body ended too early, or with a reset. *)
| Bad_response of string (** The response is not correct HTTP/1.1. *)
| Unsupported of string (** For example [https], or a protocol switch. *)
(** The type for errors. *)

exception Error of error
(** The exception for errors. *)

(**/**)

module Private : sig
  module Conn = Conn
  module Writers = Writers
  module Exchange = Exchange
end
