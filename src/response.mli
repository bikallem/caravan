(** HTTP responses. *)

(** The type for responses. *)
type t

(** The type for what follows a response's head. *)
type kind =
  | Body of Body.t  (** A response with a body, possibly empty. *)
  | Upgrade of (Flow.t -> unit)
      (** A response that takes over the connection: after the response head is
          sent, the function gets the connection's raw flow and the connection
          closes when it returns. Used for WebSocket and [CONNECT] tunnels. *)

val make : ?status:Status.t -> ?headers:Headers.t -> Body.t -> t
(** [make ~status ~headers body] is a response with [body]. [status] defaults to
    {!Status.ok}, [headers] to {!Headers.empty}. The protocol engine adds
    framing fields such as [Content-Length]; handlers do not set them.

    [Upgrade] is a field of the connection. The engine sends it only on a [101]
    and on a [426] (Upgrade Required), whose [Upgrade] field must name the
    protocols to upgrade to (RFC 9110 15.5.22). A handler that forwards an
    upstream [426] thus forwards its [Upgrade] field too.

    A [205] (Reset Content) has no content (RFC 9110 15.3.6): the engine sends
    [Content-Length: 0] and never produces the body.

    A [304] (Not Modified) has no content, and the engine does not send its
    [Content-Type], [Content-Encoding], [Content-Language] and [Content-Range]
    fields (RFC 9110 15.4.5): a cache updates its stored response from a 304.
    {!headers} still has them. A handler cannot send them on a 304.

    @raise Invalid_argument
      if [status] is [1xx]: interim responses such as [100 Continue] are the
      protocol engine's to send, and a protocol switch is an {!upgrade}. The
      same holds for {!string}, {!empty} and {!stream}. *)

val string :
  ?status:Status.t -> ?headers:Headers.t -> ?content_type:string -> string -> t
(** [string ~status ~headers ~content_type s] is a response whose body is [s].
    Its [Content-Type] is [content_type] if given, else the one in [headers],
    else ["text/plain; charset=utf-8"]. A [304] made from it does not send its
    [Content-Type] (see {!make}). *)

val empty : ?status:Status.t -> ?headers:Headers.t -> unit -> t
(** [empty ~status ~headers ()] is a response without a body. [status] defaults
    to {!Status.no_content}. *)

val stream :
     ?status:Status.t
  -> ?headers:Headers.t
  -> ?length:int
  -> (Body.Writer.t -> unit)
  -> t
(** [stream ~status ~headers ~length produce] is a response whose body [produce]
    writes, see {!Body.of_writer}. *)

val upgrade : ?status:Status.t -> ?headers:Headers.t -> (Flow.t -> unit) -> t
(** [upgrade ~status ~headers f] is a response that hands the connection to [f]
    once its head is sent. [status] defaults to {!Status.switching_protocols}; a
    [2xx] status is for accepting a [CONNECT] tunnel.

    [f] gets the connection's flow, starting with any bytes the client sent
    after its request, and without deadlines. The connection closes when [f]
    returns.

    A [101] must name the new protocol in an [Upgrade] field and answer an
    HTTP/1.1 request that asked for an upgrade; a [2xx] must answer a [CONNECT]
    request. Otherwise the client gets a [500] and the error is reported, as for
    a handler that raises.

    Before the switch, the engine reads and discards the rest of the request
    body. If the request expects [100-continue], the engine first sends
    [100 Continue]: always before a [101] (RFC 9110 7.8), and before a [2xx]
    only if a body is still owed. If the rest of the body is known to be larger
    than [Config.max_drain], the client gets a [500] with no [100].

    @raise Invalid_argument if [status] is neither [101] nor [2xx]. *)

val status : t -> Status.t
(** [status r] is the status of [r]. *)

val headers : t -> Headers.t
(** [headers r] are the header fields of [r]. *)

val kind : t -> kind
(** [kind r] is what follows the head of [r]. *)

val with_status : Status.t -> t -> t
(** [with_status s r] is [r] with status [s].

    @raise Invalid_argument
      if [s] is not allowed for [r]'s {!kind}: [1xx] for a body, anything but
      [101] or [2xx] for an upgrade. *)

val with_headers : Headers.t -> t -> t
(** [with_headers h r] is [r] with headers [h]. *)

val pp : Format.formatter -> t -> unit
(** [pp] formats a response's status and headers, for inspection. *)
