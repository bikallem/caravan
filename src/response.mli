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
    fields (RFC 9110 15.4.5, 14.4): a cache updates its stored response from a
    304. {!headers} keeps them. A handler cannot send them on a 304.

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
    HTTP/1.1 request that asked for an upgrade. Each protocol in the [Upgrade]
    field must be one that the request offered (RFC 9110 7.8): the names compare
    without case, and the versions must be the same text. A protocol without a
    version matches only a protocol without a version. A [2xx] must answer a
    [CONNECT] request. Otherwise the client gets a [500] and the error is
    reported, as for a handler that raises.

    Before the switch, the engine reads and discards the rest of the request
    body. If the request expects [100-continue], the engine first sends
    [100 Continue]: always before a [101] (RFC 9110 7.8), and before a [2xx]
    only if a body is still owed. If the rest of the body is known to be larger
    than [Config.max_drain], the client gets a [500] with no [100].

    While the server stops, the engine makes no switch: the client gets a [503]
    (Service Unavailable) and the connection closes. The engine examines this
    when the response arrives and again just before it writes the head. A stop
    can start after this last check, while the head is written or after the
    switch. Thus [f] must choose {!Conn_info.stopping} with its other actions.

    Thus [f] runs only if the switch happens: on a [503], a [500], or a client
    that left, it never runs. Put the setup that [f] must release (for example
    the connection to an upstream of a tunnel) inside [f].

    @raise Invalid_argument if [status] is neither [101] nor [2xx]. *)

val status : t -> Status.t
(** [status r] is the status of [r]. *)

val headers : t -> Headers.t
(** [headers r] are the header fields of [r]. *)

val kind : t -> kind
(** [kind r] is what follows the head of [r]. *)

val with_status : Status.t -> t -> t
(** [with_status s r] is [r] with status [s]. A [304] does not send some fields
    of [r] (see {!make}).

    @raise Invalid_argument
      if [s] is not allowed for [r]'s {!kind}: [1xx] for a body, anything but
      [101] or [2xx] for an upgrade. *)

val with_headers : Headers.t -> t -> t
(** [with_headers h r] is [r] with headers [h]. *)

val with_body : Body.t -> t -> t
(** [with_body b r] is [r] with the body [b]. The status, the fields and the
    attributes stay. The old body is not closed: the caller still owns it.

    @raise Invalid_argument if [r] is an upgrade response. *)

(** {1:attributes Attributes}

    Typed values that middleware attach to a response, see {{!Attrs}[Attrs]}.
    They never go on the wire. *)

val attrs : t -> Attrs.t
(** [attrs r] are the attributes of [r]. A new response has no attributes. *)

val with_attrs : Attrs.t -> t -> t
(** [with_attrs m r] is [r] with the attributes [m]. *)

val find_attr : 'a Attrs.key -> t -> 'a option
(** [find_attr k r] is [Attrs.find k (attrs r)]. *)

val with_attr : 'a Attrs.key -> 'a -> t -> t
(** [with_attr k v r] is [r] with the value [v] for [k]. The other attributes
    stay. *)

val pp : Format.formatter -> t -> unit
(** [pp] formats a response's status, the headers and the names of the
    attributes ({!Attrs.pp}), for inspection. It prints a line only for headers
    or attributes that exist. *)
