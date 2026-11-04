(** Sessions and signed values.

    {[
    let () = Mirage_crypto_rng_unix.use_default ()

    let key = Caravan_session.Key.of_base64url (Sys.getenv "SESSION_KEY")

    let app =
      Caravan.Handler.compose
        [ Caravan_session.middleware ~keys:[key]
            ~store:(Caravan_session.memory_store ())
            () ]
        (fun req ->
          match Caravan_session.find "user" req with
          | Some user ->
              Caravan.Response.string ("hello " ^ user)
          | None ->
              Caravan_session.set "user" "ada" req ;
              Caravan.Response.string "hello" )
    ]}

    {1:rng The random number generator}

    The session IDs come from [Mirage_crypto_rng.default_generator]. Set it one
    time, before the middleware: [Mirage_crypto_rng_unix.use_default ()]. The
    generator must be safe on many domains: that generator is, a Fortuna
    generator is not.

    {1:limits Limits}

    - Two requests of one session at the same time: the last write of the data
      wins. A write never brings back a session that {!clear} or {!renew} ended:
      the store writes only to a session that exists, and a {!renew} after the
      end of its session acts as a {!clear}.
    - The idle limit: a session ends after [idle] with no request. The
      middleware moves the limit only when more than half of [idle] passed, thus
      a session can end between [idle / 2] and [idle] after the last request.
    - The limits use the wall clock: a step of the clock changes them.
    - The data of a session has at most 64 KiB.
    - Make no session for an anonymous user: a flood of new sessions removes the
      oldest sessions of a {!memory_store}. *)

(** Secret keys for HMAC-SHA256. *)
module Key : sig
  type t

  val of_string : string -> t
  (** [of_string s] is the key of the bytes [s]. A key is random bytes, not a
      password: use 32 random bytes, for example from {!generate}.

      @raise Invalid_argument if [s] has less than 32 bytes. *)

  val generate : unit -> t
  (** [generate ()] is 32 bytes of the default generator. A key of one process:
      more processes must share a key of the configuration. *)

  val to_base64url : t -> string

  val of_base64url : string -> t
  (** @raise Invalid_argument
        if the text is not base64url (no padding) or the key has less than 32
        bytes. *)
end

(** Values with a signature: [value.mac], the MAC is the HMAC-SHA256 of
    [purpose ^ "\000" ^ value], in base64url with no padding. *)
module Signed : sig
  val sign : purpose:string -> Key.t -> string -> string
  (** [sign ~purpose key value] is [value] with its signature. [purpose] stops
      the use of a signature for another purpose.

      @raise Invalid_argument
        if [purpose] has a NUL byte, or starts with ["caravan."]: the session
        cookies and the CSRF tokens use these purposes. *)

  val verify : purpose:string -> Key.t list -> string -> string option
  (** [verify ~purpose keys s] is the value of [s] if a key of [keys] signed it
      for [purpose]. The first key signs, the others are old keys of a rotation.
      The compare is constant time, and only the exact text of the MAC is
      accepted.

      @raise Invalid_argument
        if [purpose] has a NUL byte, or starts with ["caravan."]. *)
end

(** {1:stores Stores} *)

(** The type for the place of the data of the sessions. *)
type store

val memory_store : ?max_sessions:int -> ?max_bytes:int -> unit -> store
(** [memory_store ~max_sessions ~max_bytes ()] keeps the sessions in the memory
    of the process. Over [max_sessions] (default 100_000) sessions or
    [max_bytes] (default 256 MiB) of keys and values, a write removes the
    sessions with the oldest writes. A session larger than [max_bytes] is not
    kept: it ends. An expired session goes at its next read, or when it is the
    oldest write at a write. A write rebuilds the list of the writes when it has
    more stale entries than sessions: that write visits each session in the
    lock, one time for about each number of sessions of writes.

    @raise Invalid_argument if [max_sessions] or [max_bytes] is less than 1. *)

val store :
     find:(string -> (string * Weft.Clock.posix) option)
  -> create:(string -> string -> expires:Weft.Clock.posix -> unit)
  -> update:(string -> string -> expires:Weft.Clock.posix -> unit)
  -> touch:(string -> expires:Weft.Clock.posix -> unit)
  -> remove:(string -> bool)
  -> store
(** A store of the application, for example a database. The keys are the SHA-256
    (hex) of the session IDs: a copy of the store gives no ID. The values are
    the encoded data.
    - [find key] is the value and its expiry, if the key exists.
    - [create key value ~expires] adds a new key.
    - [update] and [touch] change only a key that exists: a write after a
      [remove] does nothing. [touch] changes only the expiry.
    - [remove key] removes the key, and is [true] if it existed. A {!renew}
      makes its new session only if the remove of the old one was [true].
    - The functions must be safe on many domains. Their exceptions go out of the
      middleware.

    The middleware checks the expiry itself: a store can keep a key after its
    expiry. *)

(** {1:middleware The middleware} *)

val middleware :
     ?cookie:string
  -> ?idle:Weft.Span.t
  -> ?absolute:Weft.Span.t
  -> ?secure:bool
  -> ?same_site:Caravan.Cookie.same_site
  -> keys:Key.t list
  -> store:store
  -> unit
  -> Caravan.Handler.middleware
(** [middleware ~keys ~store ()] gives each request its session, from a signed
    cookie [cookie] (default ["__Host-session"]).
    - A cookie with a bad signature, or with the ID of no session, gives no
      session. A cookie of another length than a signed ID costs no HMAC, and a
      bad signature costs no read of the store. The purpose of the signature of
      the cookie is ["caravan.session"]: {!Signed} refuses the purposes
      [caravan.*].
    - [idle] (default 30 min) and [absolute] (default 8 h, from the creation)
      end a session.
    - [secure] (default [true]) and [same_site] (default [`Lax]) are those of
      the cookie, with [HttpOnly] and [Path=/]. The [__Host-] prefix needs
      [secure]. A name with no prefix lets a host of a sibling domain plant a
      cookie.
    - After the handler, the middleware removes the ended sessions, writes the
      new or changed data, and sets the cookie for a new ID ({!set} with no
      session, {!renew}, {!id}), for a cookie that an old key signed, and an
      expired cookie after {!clear}. A response with a session cookie gets
      [Cache-Control: no-store] in place of its own field, and a response after
      {!Csrf.token} too.
    - A handler that raises: no write, and the exception goes on.

    @raise Invalid_argument
      if [keys] is empty, if [idle] or [absolute] is not in 0 (excluded) to 10
      years, if the cookie name or its attributes are not correct (see
      {!Caravan.Cookie.make}), or if no default generator is set. *)

(** {1:handler In a handler}

    Each function raises [Invalid_argument] if the request did not go through
    {!middleware}. A function that changes the session raises [Invalid_argument]
    after the handler gave its response, for example in the producer of a
    stream, in an upgrade or in a background fiber. {!find}, {!set} and
    {!remove} raise [Invalid_argument] for a name that starts with [caravan.]:
    these names are for the libraries of caravan. *)

val find : string -> Caravan.Request.t -> string option
(** [find name req] is the value of [name] in the session of [req], if any. *)

val set : string -> string -> Caravan.Request.t -> unit
(** [set name value req] sets [name] in the session. With no session, it makes a
    session with a new ID.

    @raise Invalid_argument if the data gets more than 64 KiB. *)

val remove : string -> Caravan.Request.t -> unit
(** [remove name req] removes [name] from the session, if it is there. *)

val renew : Caravan.Request.t -> unit
(** [renew req] gives the session a new ID, and ends the old ID. The data and
    the creation time stay. Call it after a login (no session fixation). With no
    session, it does nothing. If another request ended the session first, the
    response expires the cookie, as after {!clear}. *)

val clear : Caravan.Request.t -> unit
(** [clear req] ends the session: the store removes it, and the cookie expires.
    A later {!set} makes a new session with a new ID. *)

val id : Caravan.Request.t -> string
(** [id req] is the ID of the session. With no session, it makes an empty
    session with a new ID, for example for a CSRF token. *)

(** {1:csrf CSRF tokens}

    Tokens against CSRF, bound to the session. They add depth to
    [Caravan_middleware.Cross_origin], which stops the cross-origin requests of
    browsers since 2020 with no token. The tokens also stop the older browsers.

    {[
    let app =
      Caravan.Handler.compose
        [ Caravan_middleware.Cross_origin.middleware ()
        ; Caravan_session.middleware ~keys:[key] ~store ()
        ; Caravan_session.Csrf.middleware () ]
        (fun req ->
          match Caravan.Request.meth req with
          | GET ->
              Caravan.Response.string
                ~headers:
                  (Caravan.Headers.of_list [("Content-Type", "text/html")])
                ( "<form method=post><input type=hidden name=_csrf value=\""
                ^ Caravan_session.Csrf.token req
                ^ "\"><button>Send</button></form>" )
          | _ ->
              Caravan.Response.string "sent" )
    ]}

    Use the middleware only for the routes of the cookie sessions, for example
    in a group of the router: a JSON API with a bearer token needs no token
    against CSRF. Do not exempt a request because it has an [Authorization]
    field: a browser sends cached Basic credentials with a cross-site form.

    A session cookie with no [__Host-] prefix lets a host of a sibling domain
    plant its own session and its own token: a login CSRF (see {!middleware}).
*)
module Csrf : sig
  val token : Caravan.Request.t -> string
  (** [token req] is a token for the session of [req]: 86 characters of
      base64url. With no session, it makes one (as {!id}). Each call gives a new
      text of the same secret (a random mask: BREACH cannot find the secret in a
      compressed page). The response gets [Cache-Control: no-store].

      The secret is the HMAC-SHA256 of the session ID with the first key. Thus
      {!renew} (a login) and {!clear} end the tokens of the session: the pages
      of other tabs need a reload. In a request, call [token] after {!renew}. A
      page gives the token to its scripts, for example in a [<meta>] tag: the
      scripts send it in the header of {!middleware}.

      @raise Invalid_argument
        after the response, also for a session that exists, and if [req] did not
        go through {!Caravan_session.middleware}. *)

  val check : string -> Caravan.Request.t -> bool
  (** [check t req] is [true] if [t] is a token of the session of [req], made
      with a key of the middleware (the old keys of a rotation too). With no
      session, it is [false] and makes no session. The compare is constant time.

      @raise Invalid_argument
        if [req] did not go through {!Caravan_session.middleware}. *)

  val middleware :
       ?header:string
    -> ?field:string
    -> ?max_bytes:int
    -> ?exempt:(Caravan.Request.t -> bool)
    -> ?deny:Caravan.Handler.t
    -> unit
    -> Caravan.Handler.middleware
  (** [middleware ()] checks the token of each request with a method other than
      [GET], [HEAD] and [OPTIONS]. Put it after {!Caravan_session.middleware}.
      In this order:
      + [exempt req] is [true]: the next handler. Unlike [Cross_origin],
        [exempt] runs before the check.
      + The request has no session: [deny req], with no read of the body.
      + The first field [header] (default [X-CSRF-Token]) is there: {!check}
        decides. A bad value gives [deny req], also with a good form field.
      + The body is a form (the [Content-Type] rule of {!Caravan.Request.form})
        of at most [max_bytes] (default 1 MiB): the first pair [field] (default
        [_csrf]) decides ({!Caravan.Form.find_text}). A larger form gets
        [413 Content Too Large]. The next handler, or [deny], gets the same
        bytes, with [Content-Length], and with no [Transfer-Encoding] and no
        [Expect]. The request trailers do not stay. Its {!Caravan.Request.form}
        gives the pairs, also the pair [field], with its own limits.
      + Else (another body, for example JSON or [multipart/form-data]):
        [deny req]. Send the token in [header], or exempt the path and call
        {!check} in the handler before any change of state: the parts of a
        multipart body come in the order of the client.

      [deny] (default [403 Forbidden]) and [exempt] run only on a request with a
      method other than [GET], [HEAD] and [OPTIONS]. Their exceptions, and an
      exception of the body read, go out.

      A form for anonymous users, for example a login form, needs a session for
      its token. A {!memory_store} then keeps a session for each visitor. Exempt
      that form, and keep [Cross_origin] for it:
      [~exempt:(fun r -> Caravan.Request.path_only r = "/login")].

      @raise Invalid_argument
        if [header] is not a field name, [field] is empty, or [max_bytes] is
        negative. Each request raises it if it did not go through
        {!Caravan_session.middleware}. *)
end

(**/**)

(** Internal: for caravan.auth. The functions do not check the name. *)
module Private : sig
  val find : string -> Caravan.Request.t -> string option

  val find_loaded : string -> Caravan.Request.t -> string option
  (** [find_loaded name req] is the value of [name] at the start of the request,
      before each change of the handler. *)

  val set : string -> string -> Caravan.Request.t -> unit

  val remove : string -> Caravan.Request.t -> unit
end
