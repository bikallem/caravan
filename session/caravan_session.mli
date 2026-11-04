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

      @raise Invalid_argument if [purpose] has a NUL byte. *)

  val verify : purpose:string -> Key.t list -> string -> string option
  (** [verify ~purpose keys s] is the value of [s] if a key of [keys] signed it
      for [purpose]. The first key signs, the others are old keys of a rotation.
      The compare is constant time, and only the exact text of the MAC is
      accepted.

      @raise Invalid_argument if [purpose] has a NUL byte. *)
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
      the cookie is ["caravan.session"]: do not sign other values with it.
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
      [Cache-Control: no-store] in place of its own field.
    - A handler that raises: no write, and the exception goes on.

    @raise Invalid_argument
      if [keys] is empty, if [idle] or [absolute] is not in 0 (excluded) to 10
      years, if the cookie name or its attributes are not correct (see
      {!Caravan.Cookie.make}), or if no default generator is set. *)

(** {1:handler In a handler}

    Each function raises [Invalid_argument] if the request did not go through
    {!middleware}. A function that changes the session raises [Invalid_argument]
    after the handler gave its response, for example in the producer of a
    stream, in an upgrade or in a background fiber. *)

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
