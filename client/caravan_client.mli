(** An HTTP/1.1 client for caravan servers and programs.

    {[
    Weft.run (module Weft_os)
    @@ fun () ->
    Caravan_client.run
    @@ fun client ->
    Caravan_client.with_response client
      (Caravan.Request.make ~authority:"example.com" Caravan.Method.GET "/")
      (fun resp ->
        print_string (Caravan.Body.to_string (Caravan_client.body resp)) )
    ]}

    The client keeps idle connections for each host and port. The writer of a
    request body belongs to the client, not to the fiber that makes the request:
    a request returns its response when the response head comes, while the
    request body continues. Thus a server handler can forward a request and
    stream the two bodies at the same time.

    Version 1 sends HTTP/1.1 over TCP to [http] URLs only. It has no TLS, no
    proxy support, no redirects, no cookies and no content codings.

    Code that uses the client runs in a weft fiber.

    Limits:
    - Weft does each DNS lookup on the name pool of the world. The pool does 8
      lookups at a time, and the other lookups wait in a queue. A cancel or the
      connect timeout ends the wait at once. The lookup continues in its thread
      until getaddrinfo returns. [Weft_linux.run] does not wait for such a
      lookup at its end, because a lookup holds no fd. *)

module Config : sig
  (** All the time limits are on the [Mono] clock of the world. *)
  type t = Config.t =
    { connect_timeout: Weft.Span.t
          (** The time limit of the lookup and all the connects. *)
    ; response_timeout: Weft.Span.t
          (** The time limit from the end of the request to the end of the
              response head. *)
    ; read_timeout: Weft.Span.t
          (** The time limit of each read of the body that waits, from the start
              of its first wait. *)
    ; write_timeout: Weft.Span.t
          (** The time limit of each write of the request that waits, from the
              start of its first wait. A write of many bytes is one write. *)
    ; idle_timeout: Weft.Span.t
          (** The time that an idle connection stays in the pool. A timer of the
              client closes older idle connections each half of this time, but
              not more often than each 10 ms. A checkout does not take one. *)
    ; max_idle_per_host: int
          (** The maximum number of idle connections for each host and port. *)
    ; max_status_line: int  (** The size limit of the status line. *)
    ; max_header_bytes: int  (** The size limit of the response fields. *)
    ; max_header_count: int  (** The limit of the number of response fields. *)
    ; close_grace: Weft.Span.t
          (** The time that {!run} gives to the request bodies that still go at
              its end, before it cancels them. A grace of about 292 years or
              more never ends. *) }

  val default : t
  (** [default] has the time limits 10 s (connect), 30 s (response, read,
      write), 5 s (idle) and 5 s (close grace). It keeps 8 idle connections for
      each host and port. Its size limits are 8 KiB, 64 KiB and 200 fields. *)
end

(** The type for errors. *)
type error = Error.t =
  | Connect of string  (** The connect failed, with the cause. *)
  | Timeout of [`Connect | `Response | `Read | `Write]  (** A time limit. *)
  | Closed
      (** The connection ended before a byte of the response came. A retry of an
          idempotent request is safe. *)
  | Truncated  (** The response body ended too early, or with a reset. *)
  | Bad_response of string  (** The response is not correct HTTP/1.1. *)
  | Unsupported of string  (** For example [https], or a protocol switch. *)

(** The exception for errors. *)
exception Error of error

(** The type for clients. *)
type t

val make : ?config:Config.t -> Weft.Fiber.scope -> t
(** [make ~config sw] is a client in the scope [sw]. It forks one daemon fiber
    in [sw]. That fiber runs the scope of the client: the writers of request
    bodies and the timer that closes old idle connections. {!close} ends it.

    If all the other fibers of [sw] end first, [sw] cancels the daemon. Then the
    client closes as with {!close}: request bodies that still go stop with no
    grace. [make] waits until the daemon starts, so it is a cancellation point.
    A client is for one weft run only. Use {!run} if possible.

    Make one client for a program, outside the handlers of a server. A program
    that runs a server and a client makes the two in one [Fiber.scope] inside
    [Weft.run]. [Caravan.Server.run] gives no scope to [on_start].

    @raise Weft.Fiber.Scope_closed if [sw] ended or is cancelled. *)

val run : ?config:Config.t -> (t -> 'a) -> 'a
(** [run ~config f] makes a client, calls [f] with it, and closes it, also when
    [f] raises. At the end of [f], a request body can still go, for example when
    its response ended first. [run] waits for such bodies for at most
    [config.close_grace], then cancels them and returns. During this time, a new
    {!request} raises [Invalid_argument] before a connect (a request that starts
    at the same time as the grace can connect first), and no retry occurs. [run]
    makes its own scope. A cancel of the caller ends the grace at once, and
    [run] then raises [Weft.Fiber.Cancelled].

    @raise Weft.Fiber.Cancelled
      if the caller is cancelled, also before the client starts. *)

val close : t -> unit
(** [close t] closes [t] at once: its connections close, and the writers of
    request bodies stop. A request body that still goes, for example after its
    response ended, stops without an error to the caller. The server then gets a
    truncated body. {!run} gives such bodies a grace time first.

    A request in progress gets [Error Closed], [Error Truncated],
    [Error (Bad_response _)] for a part of a head, or [Invalid_argument]. A
    later {!request} raises [Invalid_argument]. A cancelled caller gets
    [Weft.Fiber.Cancelled].

    [close] does no sync, so a cancelled fiber can call it. Call it from fiber
    code. *)

val request : t -> Caravan.Request.t -> Caravan.Response.t
(** [request t req] sends [req] and gives the response when its head comes:
    - The scheme of [req] must be [http], and [req] must have an authority:
      [host], [host:port] or [[ipv6]:port]. The port is 80 by default.
    - The body of [req] goes while the caller reads the response. The head goes
      first, before the producer of the body runs: the producer can wait for the
      response. If the response tells that the connection closes, the rest of
      the body does not go.
    - A body of length 0 has no framing field for [GET], [HEAD], [DELETE],
      [OPTIONS], [TRACE] and [CONNECT] (RFC 9110 §8.6), thus its producer does
      not run. The close of such a body runs in [request], before the request
      goes.
    - The close of a source body runs when its reads end. For a chunked body,
      this is before the end of the body goes: the close must not wait for the
      response.
    - The caller must read the response body to its end, or call
      {!Caravan.Body.close}, or the connection stays in use. Use
      {!with_response} if possible.
    - The producer of the request body must not read or close the response body
      of the same request.
    - If the server closed a reused idle connection before a byte of the
      response, an idempotent request without a body goes again one time, on a
      new connection.
    - If the response ends before the request body, and the connection can stay,
      the end of the response does not wait for the producer of the request
      body. The request body continues, also after the response body closes, and
      the connection goes back to the pool when the producer ends. This also
      occurs, rarely, when the response ends during the flush of the last data
      of a body: the connection then comes back a little later. An exception of
      the producer at that time is lost, and the connection then closes.
    - If a write of the request body fails, a write of the producer raises an
      exception of the client. The producer must not catch it, or it must raise
      it again.
    - The read that ends the response body can wait for the last writes of the
      request, each with [write_timeout], when the user code of the request body
      ended. When the response has no body, [request] waits for them.
    - If the caller is cancelled during [request] or a read of the body, also
      during that wait, the connection closes and the request body stops. But a
      cancel during the close of a body of length 0 in [request] keeps the
      connection: nothing went. If the caller is cancelled at a different time,
      the request body continues until the response body closes (or, after a
      deferred release, until its producer ends), or until the client closes.
    - The producer or the close of the request body can raise
      [Weft.Fiber.Cancelled] of a different scope. Then the caller gets
      [Failure], because the caller is not cancelled.
    - A cancelled caller gets [Weft.Fiber.Cancelled] in place of the
      [Invalid_argument] of a closed client. This is also true if the client
      closes at the same time, for example at a cancel of the caller of {!run}.
      A bad request, for example a request with no authority, gives
      [Invalid_argument] also to a cancelled caller.

    @raise Error for the errors of the network and of the server.
    @raise Invalid_argument
      if [req] has no authority or an invalid path, or if [t] is closed. An
      exception of the request body producer, or of the close of a source body,
      goes to the caller: from [request], or after the response head, from the
      read that ends the response body or from {!Caravan.Body.close}. If
      [request] raises before the request goes, the request body closes. *)

val with_response : t -> Caravan.Request.t -> (Caravan.Response.t -> 'a) -> 'a
(** [with_response t req f] calls [f] with the response to [req], then closes
    its body, also when [f] raises. An exception of [f] wins over an exception
    of the close. *)

val get : ?headers:Caravan.Headers.t -> t -> string -> Caravan.Response.t
(** [get t url] is the response to a [GET] of [url]:
    [http://host[:port][/path][?query][#fragment]]. The scheme can be in any
    case, and the fragment does not go.

    @raise Error with [Unsupported] for a different scheme.
    @raise Invalid_argument
      for a URL with user information, an empty host or a port out of the range
      1 to 65535. *)

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

  val reap_interval : Config.t -> Weft.Span.t
end
