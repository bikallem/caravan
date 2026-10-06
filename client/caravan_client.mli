(** An HTTP/1.1 client for caravan servers and programs.

    {[
      Affect_unix.Unix.main @@ fun () ->
      Caravan_client.run @@ fun client ->
      Caravan_client.with_response client
        (Caravan.Request.make ~authority:"example.com" Caravan.Method.GET "/")
        (fun resp -> print_string (Caravan.Body.to_string (Caravan_client.body resp)))
    ]}

    The client keeps idle connections for each host and port. The writer
    of a request body belongs to the client, not to the fiber that makes
    the request: a request returns its response when the response head
    comes, while the request body continues. Thus a server handler can
    forward a request and stream the two bodies at the same time.

    Version 1 sends HTTP/1.1 over TCP to [http] URLs only. It has no TLS,
    no proxy support, no redirects, no cookies and no content codings. *)

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

type t
(** The type for clients. *)

val make : ?config:Config.t -> unit -> t
(** [make ~config ()] is a client. It must run inside the affect runtime.
    It starts a fiber, a child of the fiber that calls [make], that writes
    the request bodies. Thus the fiber that calls [make] returns only
    after {!close}. Use {!run} if possible.

    Make one client for a program, outside the handlers of a server, for
    example with {!run} around [Server.start]. Do not make it in [on_start]
    of [Server.run]: nothing then closes it, and [Server.run] does not
    return. A client is for one [Affect_unix.Unix.main] only. *)

val run : ?config:Config.t -> (t -> 'a) -> 'a
(** [run ~config f] makes a client, calls [f] with it, and closes it, also
    when [f] raises. It returns after all the writers of request bodies
    end. *)

val close : t -> unit
(** [close t] closes [t]: its connections close, and the writers of
    request bodies stop. A request in progress gets [Error Closed],
    [Error Truncated] or [Invalid_argument]. A later {!request} raises
    [Invalid_argument]. *)

val request : t -> Caravan.Request.t -> Caravan.Response.t
(** [request t req] sends [req] and gives the response when its head
    comes:
    - The scheme of [req] must be [http], and [req] must have an
      authority: [host], [host:port] or [[ipv6]:port]. The port is 80 by
      default.
    - The body of [req] goes while the caller reads the response. If the
      response tells that the connection closes, the rest of the body
      does not go.
    - The caller must read the response body to its end, or call
      {!Caravan.Body.close}, or the connection stays in use. Use
      {!with_response} if possible.
    - The producer of the request body must not read or close the
      response body of the same request.
    - If the server closed an idle connection, an idempotent request
      without a body goes again one time on a new connection.
    - If the caller is cancelled during [request] or a read of the body,
      the connection closes and the request body stops. If the caller is
      cancelled at a different time, the request body continues until the
      response body closes or the client closes.

    @raise Error for the errors of the network and of the server.
    @raise Invalid_argument if [req] has no authority or an invalid path,
    or if [t] is closed. An exception of the request body producer goes
    to the caller: from [request], or after the response head, from the
    read that ends the response body or from {!Caravan.Body.close}. *)

val with_response : t -> Caravan.Request.t -> (Caravan.Response.t -> 'a) -> 'a
(** [with_response t req f] calls [f] with the response to [req], then
    closes its body, also when [f] raises. An exception of [f] wins over
    an exception of the close. *)

val get : ?headers:Caravan.Headers.t -> t -> string -> Caravan.Response.t
(** [get t url] is the response to a [GET] of [url]:
    [http://host[:port][/path][?query][#fragment]]. The scheme can be in
    any case, and the fragment does not go.

    @raise Error with [Unsupported] for a different scheme.
    @raise Invalid_argument for a URL with user information, an empty
    host or a port out of the range 1 to 65535. *)

val body : Caravan.Response.t -> Caravan.Body.t
(** [body resp] is the body of [resp]. *)

(**/**)

module Private : sig
  module Conn = Conn
  module Writers = Writers
  module Exchange = Exchange
  module Resolver = Resolver
  module Connect = Connect
  module Pool = Pool

  val parse_url : string -> string * string
end
