(** Middleware for the requests that come through a reverse proxy, and for
    request IDs.

    {[
    let app =
      Caravan.Handler.compose
        [ Caravan_middleware.Forwarded.middleware
            ~trusted:(Caravan_middleware.Forwarded.trust_ips ["10.0.0.1"])
            ()
        ; Caravan_middleware.Request_id.middleware () ]
        handler
    ]} *)

(** The fields [X-Forwarded-For], [X-Forwarded-Proto] and [X-Forwarded-Host] of
    a trusted reverse proxy.

    The middleware starts at the peer of the connection. While the node is
    trusted, it takes the last element of [X-Forwarded-For] that is left, at
    most [limit] times. The client is the node of the last element that it took.
    The values of [X-Forwarded-Proto] and [X-Forwarded-Host] at the same
    position from the right give the scheme and the authority. Thus the text
    that a client adds to the fields is always to the left of the elements of
    the trusted proxies, and the middleware does not read it.

    Each field that is on must be set or overwritten by the trusted proxy: if
    the proxy only passes a field through, a client can choose its value.

    The middleware does not read the RFC 7239 [Forwarded] field. A server behind
    a proxy that ends TLS gets [421] for an absolute-form [https://] target,
    before any middleware. *)
module Forwarded : sig
  (** The type for one hop. *)
  type node =
    | Ip of string
        (** An IP address in normal form: IPv6 in the form of RFC 5952 (lower
            case, the longest run of zero groups as [::]), and an IPv4 address
            mapped to IPv6 ([::ffff:a.b.c.d]) as [a.b.c.d]. An IPv4 octet has no
            leading zero. *)
    | Unix  (** A peer on a Unix socket. *)
    | Unknown
        (** A value that is not an IP address, for example [unknown] or an empty
            value, or the empty peer [Tcp ("", 0)] of macOS. *)

  val middleware :
       ?proto:bool
    -> ?host:bool
    -> ?limit:int
    -> trusted:(node -> bool)
    -> unit
    -> Caravan.Handler.middleware
  (** [middleware ~proto ~host ~limit ~trusted ()] gives each request the
      client, the scheme and the authority that its trusted proxies tell.
      - [trusted node] tells if [node] is a proxy of the application. The
        middleware never calls it with {!Unknown}: an [Unknown] node stops the
        walk. [fun _ -> true] trusts each client for [limit] hops.
      - [proto] (default [true]) reads [X-Forwarded-Proto]. A value that is not
        [http] or [https], in any case, also an empty value, makes the client
        {!Unknown} and stops the walk. The scheme of the earlier elements stays.
      - [host] (default [false]) reads [X-Forwarded-Host]. A value that
        {!Caravan.Request.is_valid_authority} refuses makes the client
        {!Unknown} and stops the walk. The authority of the earlier elements
        stays. A [CONNECT] request keeps its authority, the target of its
        tunnel.
      - [limit] (default 1) is the maximum number of elements that the
        middleware takes.

      A value of [X-Forwarded-For] is an IPv4 address with an optional port, or
      an IPv6 address, with or without brackets, and in brackets with an
      optional port. Each other value is {!Unknown}, also an IPv6 address with a
      zone ([%eth0]): the middleware takes it as the client, then it stops. Thus
      a bad value never gives a trusted proxy as the client. The field lines of
      one name join in order, and the middleware reads only the last [limit]
      elements of each field.

      If [X-Forwarded-Proto] (or [-Host]) has fewer elements than
      [X-Forwarded-For], an element of [X-Forwarded-For] with no element at its
      position keeps the value of the earlier elements. With no
      [X-Forwarded-For], the scheme and the authority do not change. The
      middleware does not change the fields. A request with no connection does
      not change.

      @raise Invalid_argument if [limit] is less than 1. *)

  val trust_ips : string list -> node -> bool
  (** [trust_ips ips node] is [true] iff [node] is [Ip a] and [a] is the normal
      form of an address of [ips]. Thus [2001:db8:0::1] matches the client
      [2001:db8::1]. *)

  val client : Caravan.Request.t -> node option
  (** [client req] is the client that the middleware found, or the node of the
      peer of the connection if it took no element, or [None] if [req] has no
      connection. *)
end

(** Request IDs, in a field such as [X-Request-Id]. *)
module Request_id : sig
  val middleware :
       ?field:string
    -> ?accept:(Caravan.Request.t -> bool)
    -> ?make:(Caravan.Request.t -> string)
    -> unit
    -> Caravan.Handler.middleware
  (** [middleware ~field ~accept ~make ()] gives each request an ID.
      - [field] is the name of the field, [X-Request-Id] by default.
      - If [accept req] (default: always) and [req] has the field, the ID is the
        clean form of its first field line. The clean form removes each comma,
        thus [abc, def] gives [abcdef].
      - Else, or if nothing stays, the ID is the clean form of [make req]. The
        default [make] gives the next value of a counter of the middleware, as
        decimal text, from 0. It is unique for one middleware in one process
        only: give a UUID function for more.
      - If that is empty too, the ID is the next value of the counter.

      The clean form removes each character that is not [A-Z a-z 0-9 - _ @],
      then it keeps at most 255 characters.

      The ID goes in the field of the request, thus a handler that forwards the
      request sends it, and in the field of the response, also of an upgrade
      response. The [500] that the server sends for a handler that raises, and
      the errors of the parser, have no ID.

      @raise Invalid_argument if [field] is not a valid field name. *)

  val find : Caravan.Request.t -> string option
  (** [find req] is the ID that the middleware gave to [req]. *)
end
