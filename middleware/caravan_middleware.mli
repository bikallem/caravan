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

(** An entry for each request, at the end of its response.

    Put the middleware after {!Forwarded} and {!Request_id}, and before
    {!Error_page} and the router, so that the entry has the client, the ID and
    the status of an error page:
    {[
    Caravan.Handler.compose
      [ Forwarded.middleware ~trusted ()
      ; Request_id.middleware ()
      ; Access_log.middleware
          ~route:(Caravan.Response.find_attr Caravan_router.template)
          ~log:(fun e -> print_string (Access_log.line e ^ "\n"))
          ()
      ; Error_page.middleware ~report () ]
      router
    ]}
    It must be outside each middleware that changes the method or the body. It
    needs a fiber, as each handler of the server has. *)
module Access_log : sig
  (** The type for the ends of a request. *)
  type outcome =
    | Complete
        (** The server got the whole body, or the response has no body (HEAD,
            204, 205, 304). A failure of the last flush after it is not seen.
            Thus a response with no body is [Complete] also when the client left
            before its head. *)
    | Incomplete
        (** The body was shorter than its length. The server then raises
            [Length_mismatch]. *)
    | Failed of exn
        (** The handler or the body raised. For a handler, the server sends 500,
            or the answer for a request body that failed. *)
    | Cancelled
        (** The client left (also during the body, also with a request body that
            was cut short) or the server stopped. An exception of a cleanup
            after it is not in the entry: the server reports it. *)
    | Not_sent
        (** The server did not send the body, for example a 2xx body to
            [CONNECT], which it refuses with 500. *)
    | Upgraded
        (** The handler gave an upgrade. The server can still refuse it (500,
            503). *)

  (** The type for entries. *)
  type entry =
    { start: Weft.Clock.posix  (** When the middleware got the request. *)
    ; duration: Weft.Span.t
          (** From [start] to the end: the end of the body, the return of a
              handler with an upgrade (not the end of the tunnel), or its
              exception. *)
    ; meth: Caravan.Method.t
    ; target: string  (** The path and query, or the authority of [CONNECT]. *)
    ; version: Caravan.Version.t
    ; client: Forwarded.node option  (** {!Forwarded.client}. *)
    ; request_id: string option  (** {!Request_id.find}. *)
    ; route: string option
          (** The [route] of the response, [None] for an exception. *)
    ; status: Caravan.Status.t option
          (** The status that the client is expected to get:
              - The status of the response of the handler.
              - 500 for a handler that raised.
              - The status of the error of a request body that failed, for
                example 413, also when the response streams that body, but only
                before the first byte of the body: after it, the client has the
                status of the handler.
              - [None] when the server sends nothing. A body that fails before
                its head gets the 500 of the server, but the entry has the
                status of the handler. *)
    ; bytes: int
          (** The bytes of the body that the server accepted, with no chunk
              framing. *)
    ; outcome: outcome }

  val middleware :
       ?route:(Caravan.Response.t -> string option)
    -> log:(entry -> unit)
    -> unit
    -> Caravan.Handler.middleware
  (** [middleware ~route ~log ()] calls [log] one time for each request that
      reaches it, if the server or an outer middleware consumes or closes the
      response body. The server answers some requests before the handler, for
      example 400, a 413 for a [Content-Length] that is too large, or 431: these
      have no entry. [route] reads the response, for example the template of the
      router; it gives [None] by default, and an exception of [route] gives
      [None].

      [log] runs in a shield, also in a cancelled fiber, and on many domains at
      the same time. It must end quickly: a stop of the server waits for it. An
      exception of [log] is dropped. *)

  val line : entry -> string
  (** [line e] is one line of text: the client, the request ID, then the method,
      the target and the version between double quotes, the status, the bytes,
      the duration as [<ms>ms] and the route, then the outcome if it is not
      [Complete]. A value that is not known is [-]. A text with a byte outside
      0x21 to 0x7E, a double quote or a backslash, and an empty text or [-], is
      an OCaml string literal. *)
end

(** A page for an exception of a handler, in place of the 500 of the server. *)
module Error_page : sig
  val middleware :
       report:(Caravan.Request.t -> exn -> Printexc.raw_backtrace -> unit)
    -> ?page:(Caravan.Request.t -> exn -> Caravan.Response.t)
    -> unit
    -> Caravan.Handler.middleware
  (** [middleware ~report ~page ()] calls [report req e bt], then gives
      [page req e] (default {!plain}), when the handler raises [e].
      - [report] is necessary: the server does not see the exception.
      - A cancel, and the error of a request body that failed (for example a
        body that is too large), go to the server as before, with no report and
        no page.
      - An exception of [report] or [page] goes to the server (500 and
        [on_error]).
      - An exception of a response body is not a page: it goes to the server.
      - The page keeps the connection, while the 500 of the server closes it.

      It needs a fiber, as each handler of the server has. *)

  val plain : Caravan.Request.t -> exn -> Caravan.Response.t
  (** [plain] gives [500 Internal Server Error] with a body that does not show
      the exception. *)
end

(** Cross-origin resource sharing (the CORS protocol of the Fetch standard).

    {[
    Cors.middleware ~origins:(Cors.Only ["https://app.example"])
      ~allow_methods:[PUT; DELETE]
      ~allow_headers:["content-type"; "authorization"]
      ~credentials:true ()
    ]}

    CORS is not access control. For an origin that is not permitted, the handler
    still runs (a form POST changes state): only the read of the response by the
    page is blocked. Same-origin POST, PUT and DELETE also send [Origin].

    The order:
    [[Forwarded; Request_id; Access_log; Cors; Error_page; authentication;
     router]]. Thus a page gets the fields, the router does not answer a
    preflight, and authentication is inside, because a preflight has no
    credentials. WebSocket is outside CORS: give {!permits} to
    [Caravan_websocket.accept ~origin]. *)
module Cors : sig
  (** The type for the origins that a policy permits. *)
  type origins =
    | Any
        (** Each origin, with the constant [Access-Control-Allow-Origin: *] on
            each actual response, also with no [Origin] (a cache can give that
            response to a later CORS fetch). *)
    | Only of string list
        (** The origins of the list, as ["https://app.example"] or
            ["http://localhost:8080"], compared as whole strings, ASCII
            case-insensitively. ["null"] matches only an entry ["null"]: it is
            the origin of sandboxed frames and files. *)
    | Matching of (string -> bool)
        (** The origins for which the predicate is [true]. It gets the first
            [Origin] field as it is, also ["null"]. Its exception goes out. A
            suffix check must have the scheme, for example
            [String.starts_with ~prefix:"https://"] and
            [String.ends_with ~suffix:".example.com"]. A predicate that is
            always [true], with [credentials], lets every site read the
            authenticated responses of a user, also a sandboxed frame with the
            origin ["null"]. *)

  val middleware :
       origins:origins
    -> ?allow_methods:Caravan.Method.t list
    -> ?allow_headers:string list
    -> ?expose_headers:string list
    -> ?credentials:bool
    -> ?max_age:int
    -> ?preflight_status:Caravan.Status.t
    -> unit
    -> Caravan.Handler.middleware
  (** [middleware ~origins ()] adds the CORS fields to the responses, and
      answers the preflights.
      - A preflight is an [OPTIONS] with [Origin] and
        [Access-Control-Request-Method]. The middleware answers it with
        [preflight_status] (204 by default), and the handler does not run: thus
        the [OPTIONS] routes of the application do not get preflights. The
        requested method is permitted if it is [GET], [HEAD] or [POST], or in
        [allow_methods] (compared case-sensitively). Each name of
        [Access-Control-Request-Headers] must be in [allow_headers] (compared
        case-insensitively), or [allow_headers] has ["*"]: a JSON body needs
        ["content-type"], and ["authorization"] must be in the list. With ["*"],
        the middleware gives the requested names back, thus ["*"] permits
        ["authorization"] too (unlike a literal [*] of Fetch). A refused
        preflight gets the same status with no CORS field.
      - An actual request with a permitted origin gets
        [Access-Control-Allow-Origin] (the [Origin] of the request, or [*] for
        {!Any}), [Access-Control-Allow-Credentials] (with [credentials]) and
        [Access-Control-Expose-Headers] (with [expose_headers]). These replace
        the fields of the handler. For an origin that is not permitted, the
        middleware removes each [Access-Control-] field of the response. Each
        actual response gets [Vary: Origin], except with {!Any}, where each
        actual response has the constant fields.
      - [max_age] is the time in seconds that a browser can keep a preflight.
        Browsers cap it, Chromium at 7200 s.
      - An exception of the handler goes out with no CORS field.

      @raise Invalid_argument
        for {!Any} with [credentials] (any site could read the authenticated
        responses of a user), an entry of {!Only} that is not ["null"] or
        [scheme://host[:port]] as a browser sends it (a host of letters, digits,
        [.], [-] and [_] or an IPv6 literal in the form of RFC 5952, a port of 1
        to 65535 with no leading zero and no default port, a scheme that starts
        with a letter and is not ws or wss), [CONNECT], [TRACE], [TRACK] or [*]
        in [allow_methods], a field name that is not a token or ["*"], ["*"] in
        [expose_headers] with [credentials], a negative [max_age], or a
        [preflight_status] that is not 2xx. *)

  val permits : origins -> string -> bool
  (** [permits origins o] is [true] if the [Origin] [o] is permitted. *)
end

(** A redirect of each [http] request to [https]. Put it after {!Forwarded}:
    behind a proxy that ends TLS, {!Forwarded} gives the request the scheme of
    the client. Put it after {!Request_id} and {!Access_log} too, so that each
    redirect has an ID and a log entry.

    {[
    Caravan.Handler.compose
      [ Caravan_middleware.Forwarded.middleware ~trusted ()
      ; Caravan_middleware.Request_id.middleware ()
      ; Caravan_middleware.Hsts.middleware ()
      ; Caravan_middleware.Https_redirect.middleware () ]
      handler
    ]} *)
module Https_redirect : sig
  val middleware :
    ?status:Caravan.Status.t -> ?port:int -> unit -> Caravan.Handler.middleware
  (** [middleware ~status ~port ()] gives a request with the scheme [https] to
      the next handler. Each other request gets [status] (default
      [307 Temporary Redirect]) with [Location: https://<host>[:<port>]<path>],
      and no body (RFC 9110 permits it).
      - [<host>] is the host of {!Caravan.Request.authority}, with no port. For
        an absolute-form target, it is the authority of the target, not the
        [Host] field (RFC 9112 §3.2.2).
      - [<port>] is [port], and nothing for 443 (the default).
      - [<path>] is {!Caravan.Request.path}: the path and the query.

      A request with no authority, with an authority that
      {!Caravan.Request.is_valid_authority} refuses, or with a path that does
      not start with [/] (for example [OPTIONS *]), gets [400 Bad Request]: no
      URL is possible, and the request does not go to the next handler over
      HTTP.

      A 307 is the default because a browser keeps a 308 or a 301 with no time
      limit: a fault in the first setup of HTTPS then stays. In production,
      [~status:Caravan.Status.permanent_redirect] (308) is better. A host filter
      goes before this middleware: the client chooses the [Host] field.

      @raise Invalid_argument
        if [status] is not 301, 302, 307 or 308, or if [port] is not in 1 to
        65535. *)
end

(** The [Strict-Transport-Security] field (HSTS, RFC 6797) on the responses of
    [https] requests. Its order with {!Https_redirect} does not matter. Put it
    before {!Error_page} if the error pages need it too. *)
module Hsts : sig
  val middleware :
       ?max_age:int
    -> ?include_subdomains:bool
    -> ?preload:bool
    -> ?exclude_hosts:string list
    -> unit
    -> Caravan.Handler.middleware
  (** [middleware ~max_age ~include_subdomains ~preload ~exclude_hosts ()] gives
      the response of a request with the scheme [https] the field
      [Strict-Transport-Security: max-age=N[; includeSubDomains][; preload]].
      - [max_age] is in seconds (default 31536000, 365 days). [0] tells the
        browser to remove its HSTS entry for the host.
      - The middleware adds no field to the response of an [http] request: a
        host must not send it over HTTP (RFC 6797 §7.2).
      - The middleware adds no field for a host of [exclude_hosts] (default
        [localhost], [127.0.0.1] and [[::1]]). A browser keeps HSTS for
        [localhost], and then it refuses HTTP for each later development server
        on that host. A browser ignores HSTS for an IP address (RFC 6797 §8.1),
        thus the two IP entries only stop a field with no effect. The compare
        has no case, removes the port and a dot at the end of the host, and the
        entry [localhost] also matches each host that ends with [.localhost].
      - A response that has the field keeps it: the handler decides.
      - An exception of the next handler goes on.

      @raise Invalid_argument
        if [max_age] is negative, if [preload] is [true] and
        [include_subdomains] is [false] or [max_age] is less than 31536000 (the
        rules of hstspreload.org), or if an entry of [exclude_hosts] has a port.
  *)
end
