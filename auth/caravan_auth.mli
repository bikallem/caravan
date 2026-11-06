(** Authentication.

    {!Jwt} verifies JSON Web Tokens (RFC 7519) in the compact form of JWS (RFC
    7515), for example the bearer tokens of an identity provider. *)

(** JSON Web Tokens: the check of the signature and of the claims.

    {[
    let keys =
      match Caravan_auth.Jwt.Key.of_jwks jwks_text with
      | Ok keys ->
          keys
      | Error e ->
          failwith e

    let user token =
      match
        Caravan_auth.Jwt.verify ~issuer:(One_of ["https://id.example"])
          ~audience:(One_of ["api://orders"]) ~keys token
      with
      | Ok claims ->
          Caravan_auth.Jwt.subject claims
      | Error _ ->
          None
    ]}

    This module only verifies tokens: an identity provider signs them. It never
    takes a key from a token ([jwk], [jku], [x5u] and [x5c] have no effect), and
    it never fetches a key.

    A check of an ES256 signature costs about 0.4 ms, ten times an EdDSA check
    and fifteen times an RS256 check: each forged token costs the server that
    time. Put a rate limit on the requests with a bad token. *)
module Jwt : sig
  (** The algorithms. [none] and the other names are always refused. *)
  type alg =
    | HS256  (** HMAC with SHA-256. *)
    | RS256  (** RSASSA-PKCS1-v1_5 with SHA-256. *)
    | ES256  (** ECDSA with P-256 and SHA-256. *)
    | EdDSA  (** Ed25519 (RFC 8037). A token can also name it [Ed25519]. *)

  (** The keys that verify the signatures. Each key has one algorithm: a token
      with the [alg] of another algorithm never uses it (no algorithm confusion,
      RFC 8725 §3.1). *)
  module Key : sig
    type t

    val of_secret : ?kid:string -> string -> t
    (** [of_secret ~kid secret] is an HS256 key. [secret] is random bytes, not a
        password.

        @raise Invalid_argument if [secret] has less than 32 bytes. *)

    val of_secret_base64url : ?kid:string -> string -> t
    (** [of_secret_base64url ~kid text] is {!of_secret} of the bytes of the
        base64url [text] (no padding), as the [k] member of a JWK.

        @raise Invalid_argument
          if [text] is not base64url, or if the secret has less than 32 bytes.
    *)

    val of_pem : ?kid:string -> string -> t
    (** [of_pem ~kid pem] is the key of a public key ([PUBLIC KEY]) or of a
        certificate ([CERTIFICATE]) in PEM: RSA (RS256), P-256 (ES256) or
        Ed25519 (EdDSA). The certificate is not validated: it only carries the
        key.

        @raise Invalid_argument
          if the text is not one of these, or if an RSA key has less than 2048
          bits, more than 8192 bits, or an exponent of more than 32 bits. *)

    val of_jwks : ?max_length:int -> string -> (t list, string) result
    (** [of_jwks text] is the keys of a JWK Set (RFC 7517 §5), for example the
        document of the [jwks_uri] of an identity provider. It keeps the keys
        that this module can use, and skips the others with no error: a [kty]
        other than [RSA], [EC] ([crv] [P-256]) and [OKP] ([crv] [Ed25519]), a
        [use] other than [sig], an [alg] of another algorithm, a [key_ops] with
        no [verify], a member of a bad type or value, a point that is not on the
        curve, and the RSA keys that {!of_pem} refuses. A token of a skipped key
        gives [No_key] or [Bad_signature].

        [Error] if [text] has more than [max_length] bytes (default 64 KiB), is
        not strict JSON (see {!verify}), or is not an object with an array
        [keys]. A large set takes milliseconds: load the keys at the start, in
        the background, or in a cache with one load at a time
        ({!Oidc.Provider.keys}).

        @raise Invalid_argument if [max_length] is less than 1. *)

    val alg : t -> alg

    val kid : t -> string option
  end

  (** The expected values of the [iss] or [aud] claim. *)
  type expect =
    | Any
        (** No check. Use it only if each token of the keys is for this
            application: an identity provider signs the tokens of all its
            applications with the same keys (RFC 8725 §3.9). *)
    | One_of of string list
        (** The claim must be there, and have one of these values (compared
            exactly). *)

  type error =
    | Malformed of string
        (** Not a token: the form, base64url, JSON, the type of a member, a size
            limit, or [nbf] after [exp]. *)
    | Unsupported of string
        (** An [alg] that is not one of {!alg}, a [crit] member, or a [typ]
            other than [JWT] and [at+jwt]. *)
    | Missing of string
        (** A claim that the check needs: [exp] (with [require_exp]), [iss] or
            [aud] (with [One_of]). *)
    | No_key
        (** No key to try: no key of the [alg] of the token, or, for a token
            with a [kid], none with this kid or with no kid (see {!verify}). *)
    | Bad_signature  (** No key to try verifies the signature. *)
    | Expired
    | Not_yet_valid
    | Bad_issuer
    | Bad_audience

  val pp_error : Format.formatter -> error -> unit

  (** The type for the claims of a verified token. *)
  type claims

  val verify :
       ?leeway:Weft.Span.t
    -> ?max_length:int
    -> ?require_exp:bool
    -> issuer:expect
    -> audience:expect
    -> keys:Key.t list
    -> string
    -> (claims, error) result
  (** [verify ~issuer ~audience ~keys token] checks [token] in this order:
      + The form: at most [max_length] bytes (default 16384: a token with many
        groups can be larger), three parts, and a header part of at most 8192
        bytes.
      + The header: canonical base64url with no padding of a strict JSON object.
        [alg] is one of {!alg}. No [crit]. [typ], if there, is [JWT] or
        [at+jwt], ASCII case-insensitive, with an optional [application/].
        [kid], if there, is a string.
      + The keys to try, of the [alg] of the token. With a [kid]: the keys with
        this kid, then the keys with no kid. With no [kid]: the keys with no
        kid, or else the first four keys of the [alg] (an identity provider that
        sends no kid can publish kids in its JWK Set). Thus one token costs at
        most the checks of these keys. No key: [No_key].
      + The signature part: canonical base64url, else [Malformed]. Then the
        signature, with each key in order. The first key that verifies it wins.
      + Only then the payload: base64url of a strict JSON object. [exp], [nbf]
        and [iat] are numbers. [iss] is a string. [aud] is a string or a
        non-empty list of strings.
      + The times, with the posix clock of weft ([Weft.Time.now Posix]):
        [Expired] if the time is not before [exp] plus [leeway] (default 60 s),
        [Not_yet_valid] if it is before [nbf] minus [leeway]. With no [exp] and
        [require_exp] (the default), [Missing "exp"].
      + [iss], then [aud], against [issuer] and [audience].

      Strict JSON is RFC 8259 with valid UTF-8, no lone surrogate in a [\u]
      escape, a depth of at most 32, finite numbers and distinct member names.
      Comments, [NaN] and names with no quotes are refused.

      ES256 accepts the two forms of a signature (s and n - s): a token text is
      not unique. To deny one token, use its [jti] claim, not its text.

      @raise Invalid_argument
        if [issuer] or [audience] is [One_of []], [leeway] is negative, or
        [max_length] is less than 1. *)

  (** {1:claims The claims}

      Each function gives [None] (or [[]]) for a claim that is not there, and
      also for a claim of another type. *)

  val subject : claims -> string option
  (** The [sub] claim. *)

  val issuer : claims -> string option
  (** The [iss] claim. *)

  val audience : claims -> string list
  (** The [aud] claim: a string gives a list of one. *)

  val expires : claims -> Weft.Clock.posix option
  (** The [exp] claim. A time that a [Weft.Clock.posix] cannot hold gives
      [None]. *)

  val not_before : claims -> Weft.Clock.posix option
  (** The [nbf] claim. *)

  val issued_at : claims -> Weft.Clock.posix option
  (** The [iat] claim. *)

  val string : string -> claims -> string option
  (** [string name c] is a claim that is a string. *)

  val strings : string -> claims -> string list option
  (** [strings name c] is a claim that is a list of strings, for example the
      roles. *)

  val int : string -> claims -> int option
  (** [int name c] is a claim that is an integer of the [int] range. *)

  val number : string -> claims -> float option
  (** [number name c] is a claim that is a number, as a float: an integer larger
      than 2{^ 53} loses precision. *)

  val bool : string -> claims -> bool option
  (** [bool name c] is a claim that is [true] or [false]. *)

  val payload : claims -> string
  (** [payload c] is the JSON text of the payload, for a parse of the other
      claims. *)
end

(** {1:users Users} *)

(** The user of a request. *)
module User : sig
  type t

  val make : ?roles:string list -> ?claims:(string * string) list -> string -> t
  (** [make ~roles ~claims id] is a user. [id] is the stable identifier of the
      user, for example the [sub] claim of a token or the key of a database row.
      Its {!scheme} is [""] until a scheme authenticates it. *)

  val id : t -> string

  val scheme : t -> string
  (** [scheme u] is the name of the scheme that authenticated [u] for this
      request, for example ["Bearer"] or ["Session"]. The scheme sets it: a
      session never keeps it. *)

  val roles : t -> string list

  val has_role : string -> t -> bool
  (** [has_role r u] compares [r] case-sensitively. *)

  val claim : string -> t -> string option

  val claims : t -> (string * string) list
end

(** {1:schemes Authentication schemes} *)

(** The result of a scheme for a request. *)
type outcome =
  | Authenticated of User.t
  | No_credentials
      (** The request has no credentials of this scheme: the next scheme. *)
  | Failed of string
      (** The request has credentials of this scheme that are bad: no user, and
          no other scheme. The text is for logs. *)

(** A way to find the user of a request. The functions that a scheme gets
    ([authenticate], [keys], [user], [validate], [lookup]):
    - raise only for a failure that the server must answer with [500], for
      example a failure of a store: their exception goes out of {!user},
      {!failure}, {!challenge} or {!require}, and a later call runs the schemes
      again;
    - can run two times for one request, when two fibers of the request ask for
      the user at the same time (the first result stays);
    - must not read the body of the request: they run at the first need, after a
      middleware or the handler can have read it. *)
module Scheme : sig
  type t

  val name : t -> string

  val make :
       name:string
    -> authenticate:(Caravan.Request.t -> outcome)
    -> challenge:
         (failure:string option -> Caravan.Request.t -> Caravan.Response.t)
    -> t
  (** [make ~name ~authenticate ~challenge] is a scheme of the application.
      [challenge] gives the response to a request with no user (see
      {!Caravan_auth.require}). [failure] is the text of [Failed], if the scheme
      failed.

      @raise Invalid_argument
        if [name] is empty, ["Bearer"] or ["Session"] (with no case): the names
        of the schemes of this library. *)

  val bearer :
       ?realm:string
    -> ?leeway:Weft.Span.t
    -> ?roles_claim:string
    -> ?user:(Jwt.claims -> (User.t, string) result)
    -> issuer:Jwt.expect
    -> audience:Jwt.expect
    -> keys:(unit -> Jwt.Key.t list)
    -> unit
    -> t
  (** [bearer ~issuer ~audience ~keys ()] is the scheme ["Bearer"] of RFC 6750:
      a JWT in the field [Authorization: Bearer <token>].
      - No [Authorization] field, or a field of another scheme:
        [No_credentials]. The name [Bearer] is ASCII case-insensitive. A token
        in the query or in the body is never read.
      - Two [Authorization] fields, or a [Bearer] value that is not one or more
        spaces and a token68: [Failed].
      - {!Jwt.verify} with [keys ()] (called for each check: the application can
        replace the keys, for example after a refresh of a JWK Set). An error:
        [Failed].
      - [user] gives the user of the claims. The default: the id is [sub] (no
        [sub], or an empty one: [Failed]), the roles are the claim [roles_claim]
        (default ["roles"]) if it is a list of strings (else no roles), and the
        claims are the string claims. For the [scope] claim, see
        {!Caravan_auth.scope}.
      - The challenge: [401] with [WWW-Authenticate: Bearer realm="<realm>"]
        (default ["api"]), and [error="invalid_request"] after a bad field or
        [error="invalid_token"] after a bad token. No [error_description]: it
        gives the reason to an attacker. RFC 6750 recommends [400] for
        [invalid_request]: this scheme gives [401]. A policy refusal is a plain
        [403], with no [insufficient_scope].

      For many issuers, use one scheme with [One_of] and all their keys: a
      [Failed] stops the other schemes.

      @raise Invalid_argument
        if [realm] has a quote, a backslash or a control character, [issuer] or
        [audience] is [One_of []], or [leeway] is negative. *)

  val session :
    ?login:string -> ?validate:(User.t -> User.t option) -> unit -> t
  (** [session ()] is the scheme ["Session"]: the user that
      {!Caravan_auth.sign_in} put in the session of [caravan.session]. Put the
      middleware of the session before {!Caravan_auth.middleware}.
      - [validate] runs for each check, for example to load the roles again or
        to end the access of a deleted user: [None] gives [Failed]. With no
        [validate], a session keeps the user that {!Caravan_auth.sign_in} gave
        until the session ends.
      - The challenge: with [login], a [303 See Other] to
        [<login>?return=<path>] for a [GET] or [HEAD] navigation of a browser:
        [Sec-Fetch-Mode: navigate], or, with no [Sec-Fetch-Mode], an [Accept]
        that contains [text/html]. [<path>] is the path and the query of the
        request, percent-encoded. Else [401 Unauthorized], with no
        [WWW-Authenticate]: no standard scheme names a cookie. Do not put the
        login route under {!Caravan_auth.require}. Check the return path with
        {!Caravan_auth.return_path}.

      A form of a session needs a protection against CSRF: see
      {!Caravan_auth.middleware}. A WebSocket upgrade is a [GET]: check its
      [Origin].

      @raise Invalid_argument
        if [login] does not start with one [/], or has a backslash, a [?] or a
        control character. *)
end

(** {1:api_keys API keys} *)

(** API keys for a web API: the generation of a key, the hash to store, and a
    scheme that finds the user of a key.

    {[
    let live = Caravan_auth.Api_key.format ~prefix:"myapp_"

    (* At creation: show [k.key] to the user one time, store [k.hash]. *)
    let k = Caravan_auth.Api_key.generate live

    let api_key =
      Caravan_auth.Api_key.scheme live ~lookup:(fun hash ->
          match Db.find_key hash with
          | Some row when not (Db.revoked row) ->
              Ok (Caravan_auth.User.make ~roles:row.roles row.owner)
          | Some _ ->
              Error "a revoked key"
          | None ->
              Error "an unknown key" )

    let api =
      Caravan.Handler.compose
        [ Caravan_auth.middleware ~schemes:[api_key; bearer] ()
        ; Caravan_auth.require ~challenge:"ApiKey" () ]
    ]}

    A key is the prefix of its format and 43 base62 characters (256 bits). A
    secret scanner finds a leaked key by its prefix. The client sends the key in
    [Authorization: Bearer <key>] (RFC 6750: a bearer token does not need to be
    a JWT), or in a field of the application, for example [X-API-Key].

    The application stores only the hash: the lowercase hex of SHA-256 of the
    whole key. A fast hash is correct for a key of 256 random bits: nobody can
    find the key from its hash. The lookup is by the hash, thus the time of the
    read of an index tells bits of the hash, not of the key. A hash is safe in a
    log, a key is not. An attacker who can write to the database can add the
    hash of a key that the attacker made. A hash with a secret of the server
    stops this, but a lost secret ends all keys: this module does not do it.

    The scheme never reads a key in the query of a URL: a query goes into logs
    and into [Referer]. A browser never sends a key by itself, thus a request
    with a key needs no CSRF token: exempt the scheme by its name (see
    {!Caravan_auth.middleware}).

    Each lookup is a read of the store, also for a random key. Put a rate limit
    of each client before {!Caravan_auth.middleware}: it limits the reads of the
    store. A rate limit of each user after it limits the work of each user (the
    keys of one user share it):
    {[
    Caravan.Handler.compose
      [ Caravan_middleware.Rate_limit.middleware ~burst:20 ~every:(Span.s 1) ()
      ; Caravan_auth.middleware ~schemes:[api_key] ()
      ; Caravan_middleware.Rate_limit.middleware ~burst:100 ~every:(Span.ms 100)
          ~key:(fun req ->
            Option.map Caravan_auth.User.id (Caravan_auth.user req) )
          () ]
    ]} *)
module Api_key : sig
  type format

  val format : prefix:string -> format
  (** [format ~prefix] is the form of the keys with [prefix], for example
      ["myapp_"] or ["myapp_test_"]. The owner of a key is its text up to and
      with its last [_] (base62 has no [_]): thus the formats ["myapp_"] and
      ["myapp_test_"] can share one field. A token with a [.] is never a key of
      a format: thus a JWT is never a key.

      @raise Invalid_argument
        if [prefix] is not 2 to 32 characters of [a-z], [0-9] and [_], with a
        last [_]. *)

  type created = {key: string; hash: string}

  val generate : format -> created
  (** [generate f] is a new key of [f] and its hash. Show the key to the user
      one time, and store only the hash. The random bytes come from
      [Mirage_crypto_rng]: initialize it at the start of the program. *)

  val hash : format -> string -> string option
  (** [hash f key] is the hash of [key], or [None] if [key] is not of the form
      of [f]. For example, to revoke a key that a user gives. *)

  (** The users of recent keys, in the memory of the process. *)
  type cache

  val cache : ?size:int -> Weft.Span.t -> cache
  (** [cache ~size ttl] keeps the user of a key for [ttl] after its lookup:
      during this time, the key does not cause a read of the store. Thus a
      revoked or expired key works until its entry expires: call {!forget} after
      the revocation is in the store (a lookup before it can put the key back),
      and keep [ttl] short. A [forget] has an effect only in its process. Give a
      cache to one scheme only: the cache does not call the [lookup] of a second
      scheme. The cache keeps only users: an [Error] of the lookup is never kept
      (random keys of the correct form never repeat, thus a negative cache does
      not stop a flood). Two requests with one key at the same time can do two
      lookups.

      The cache keeps at most [size] (default 10_000) entries. An insert into a
      full cache removes the expired entries, then the oldest, until at most
      half of [size] stays.

      @raise Invalid_argument
        if [size] is less than 1, or [ttl] is not from 1 s to 1 h. *)

  val forget : cache -> string -> unit
  (** [forget c hash] removes the entry of [hash]. A lookup that started before
      it does not put its result in [c]. *)

  val scheme :
       ?name:string
    -> ?field:string
    -> ?realm:string
    -> ?cache:cache
    -> lookup:(string -> (User.t, string) result)
    -> format
    -> Scheme.t
  (** [scheme ~lookup f] is the scheme [name] (default ["ApiKey"]) of the keys
      of [f].

      [lookup hash] gives the user of the key with [hash], for example its
      owner, with roles. It checks the expiry and the revocation of the key.
      [Error m] gives [Failed m]. A failure of the store must raise (the server
      sends [500]): with [Error], each client gets [401] and can discard a good
      key. [lookup] gets the hash, never the key.

      With no [field], the scheme reads [Authorization: Bearer <key>]:
      - No field, a field of another scheme, or a token that is not of [f] (its
        text up to and with its last [_] is not the prefix, or it has a [.], as
        a JWT): [No_credentials], with no failure for a log. Thus put this
        scheme before {!Scheme.bearer}: in the other order, the bearer scheme
        gives [Failed] for a key.
      - Two [Authorization] fields, or a malformed [Bearer] field: [Failed], the
        same as {!Scheme.bearer}.
      - A token of [f] that is not 43 base62 characters after the prefix:
        [Failed].
      - The challenge: the challenge of {!Scheme.bearer}, with [realm] (default
        ["api"]).

      With [field] (for example ["X-API-Key"]), the scheme reads only this
      field:
      - No field, or a value that is not of [f]: [No_credentials].
      - Two fields: [Failed].
      - The challenge: [401] with [WWW-Authenticate: <name> realm="<realm>"]
        (RFC 9110 §11.6.1), and [error="invalid_token"] after a bad key.

      With [cache], a key in the cache does not call [lookup].

      @raise Invalid_argument
        if [name] is empty, is not a token, or is ["Bearer"] or ["Session"]
        (with no case); if [field] is not a valid field name, or is
        [Authorization], [Proxy-Authorization] or [Cookie] (a browser sends a
        cookie by itself); if [realm] has a quote, a backslash or a control
        character. *)
end

val sign_in : Caravan.Request.t -> User.t -> unit
(** [sign_in req u] puts [u] in the session of [req] for the scheme
    {!Scheme.session}. If the session has a user (also one that does not
    decode), the session ends and a new session starts, with no data of the old
    one. Else the session gets a new ID ({!Caravan_session.renew}: no session
    fixation) and keeps its data and its creation time: the absolute limit of
    the session counts from the first request. The user of [req] ({!user}) does
    not change in this request.

    @raise Invalid_argument
      as {!Caravan_session.set}, for example after the response, or if the data
      of the session gets more than 64 KiB. *)

val sign_out : Caravan.Request.t -> unit
(** [sign_out req] ends the session ({!Caravan_session.clear}). *)

(** {1:middleware The middleware} *)

val middleware : schemes:Scheme.t list -> unit -> Caravan.Handler.middleware
(** [middleware ~schemes ()] gives each request a way to find its user. At the
    first call of {!user}, {!failure}, {!challenge} or {!require} for the
    request, it tries the schemes in order: the first [Authenticated] gives the
    user, a [Failed] stops with no user, and [No_credentials] goes to the next
    scheme. A request that never asks costs no check (the CSRF exemption below
    asks for each request with a method other than [GET], [HEAD] and [OPTIONS]).
    The middleware never refuses a request: {!require} does. Put the middleware
    of the session before this middleware: else the first need raises
    [Invalid_argument] of [caravan.session].

    The first scheme gives the challenge to a request with no credentials: put
    the session scheme first for a redirect to the login page, and use
    [require ~challenge:"Bearer"] (or ["ApiKey"]) on the routes of an API.

    With the session scheme, a cross-site form can send the session cookie.
    Protect the routes with [Caravan_middleware.Cross_origin] first, and with
    [Caravan_session.Csrf.middleware] after this middleware, with the users of
    the bearer and API key schemes exempt (a browser never sends their
    credentials by itself). Name the exempt schemes: a scheme of the application
    can read a cookie.
    {[
    Caravan.Handler.compose
      [ Caravan_middleware.Cross_origin.middleware ()
      ; Caravan_session.middleware ~keys ~store ()
      ; Caravan_auth.middleware ~schemes:[session; bearer] ()
      ; Caravan_session.Csrf.middleware
          ~exempt:(fun r ->
            match Caravan_auth.user r with
            | Some u ->
                List.mem (Caravan_auth.User.scheme u) ["Bearer"; "ApiKey"]
            | None ->
                false )
          () ]
    ]}

    @raise Invalid_argument
      if [schemes] is empty or has two schemes with one name. *)

val user : Caravan.Request.t -> User.t option
(** [user req] is the user of [req], if a scheme authenticated it.

    @raise Invalid_argument if [req] did not go through {!middleware}. *)

val failure : Caravan.Request.t -> (string * string) option
(** [failure req] is the name of the scheme that failed and its text, if a
    scheme failed: for a log or a rate limit.

    @raise Invalid_argument if [req] did not go through {!middleware}. *)

val challenge : Caravan.Request.t -> Caravan.Response.t
(** [challenge req] is the challenge of the scheme that failed, or else of the
    first scheme.

    @raise Invalid_argument if [req] did not go through {!middleware}. *)

val forbid : Caravan.Request.t -> Caravan.Response.t
(** [forbid req] is [403 Forbidden]: the response of {!require} to a user that
    the policy refuses. *)

val return_path : Caravan.Request.t -> string option
(** [return_path req] is the [return] parameter of the query of [req] if it is a
    path of this site: [/] and then not [/], with no backslash and no control
    character. Use it for the redirect after a login: another value can send the
    user to another site. *)

(** {1:authorization Authorization} *)

(** The type for a rule on a user. *)
type policy = User.t -> bool

val authenticated : policy
(** Each user. *)

val role : string -> policy
(** [role r] is the users with the role [r] (case-sensitive). *)

val scope : string -> policy
(** [scope s] is the users with [s] in the claim [scope], a list of names
    separated by spaces (RFC 9068). *)

val all : policy list -> policy
(** [all ps] is the users that each policy of [ps] accepts: [all []] is each
    user. *)

val any : policy list -> policy
(** [any ps] is the users that one policy of [ps] accepts: [any []] is no user.
*)

val require :
  ?policy:policy -> ?challenge:string -> unit -> Caravan.Handler.middleware
(** [require ~policy ~challenge ()] gives a request to the next handler only if
    it has a user that [policy] (default {!authenticated}) accepts. A user that
    the policy refuses gets {!forbid}. A request with no user gets the challenge
    of the scheme that failed, else of the scheme with the name [challenge],
    else of the first scheme. A response of the next handler with no
    [Cache-Control] gets [Cache-Control: private]: a shared cache must not keep
    a response for a user. A handler that sets [Cache-Control] decides. Put it
    on a group of the router:
    [Caravan_router.group ~middleware:[Caravan_auth.require ~policy:(role
     "admin") ()] "/admin" routes].

    @raise Invalid_argument
      for each request that did not go through {!middleware}, or if no scheme of
      the middleware has the name [challenge]. *)

(** {1:oidc OpenID Connect} *)

(** OpenID Connect 1.0: the provider of a relying party, and the check of an ID
    token.

    {[
    let google client =
      match
        Caravan_auth.Oidc.Provider.discover client "https://accounts.google.com"
      with
      | Ok p ->
          p
      | Error e ->
          failwith e

    let user provider token =
      match
        Caravan_auth.Oidc.verify_id_token provider ~client_id:"123.apps" token
      with
      | Ok claims ->
          Caravan_auth.Jwt.subject claims
      | Error _ ->
          None
    ]}

    The client must have HTTPS:
    [Caravan_client.run ~tls:(Caravan_tls.https config)]. Each URL of a provider
    is [https]. *)
module Oidc : sig
  (** An OpenID provider: its endpoints and a cache of its keys. *)
  module Provider : sig
    type t

    val discover :
         ?timeout:Weft.Span.t
      -> ?max_length:int
      -> Caravan_client.t
      -> string
      -> (t, string) result
    (** [discover client issuer] gets the discovery document of [issuer] (OIDC
        Discovery §4): a GET of [issuer], with no terminating ["/"], and then
        [/.well-known/openid-configuration].
        - [issuer] must be an [https] URL with a host, a port of 1 to 65535, no
          user information, no query and no fragment, else [Error] with no
          request.
        - The document has the limit [max_length] (default 64 KiB) and one
          deadline [timeout] (default 10 s) for all of the request. A status
          other than 200 is an [Error].
        - The document is strict JSON (see {!Jwt.verify}). Its [issuer] must be
          equal to [issuer], byte for byte (§4.3). [authorization_endpoint],
          [token_endpoint] and [jwks_uri] must be [https] URLs.
          [response_types_supported] must contain [code].
          [code_challenge_methods_supported], if there, must contain [S256].
        - The function does not load the keys: the first need does.

        The provider keeps [client] for the loads of its keys.

        @raise Invalid_argument
          if [timeout] is not positive or [max_length] is less than 1. *)

    val make :
         ?timeout:Weft.Span.t
      -> ?auth_methods:string list
      -> ?iss_parameter:bool
      -> issuer:string
      -> authorization_endpoint:string
      -> token_endpoint:string
      -> jwks_uri:string
      -> Caravan_client.t
      -> t
    (** [make ~issuer ~authorization_endpoint ~token_endpoint ~jwks_uri client]
        is a provider with no discovery. [auth_methods] and [iss_parameter] are
        the members [token_endpoint_auth_methods_supported] and
        [authorization_response_iss_parameter_supported] of a document.

        @raise Invalid_argument
          if a URL is not [https] (as for {!discover}), the issuer has a query,
          or [timeout] is not positive. *)

    val issuer : t -> string

    val authorization_endpoint : t -> string

    val token_endpoint : t -> string

    val auth_methods : t -> string list option
    (** The methods of client authentication at the token endpoint, if the
        provider tells them. *)

    val iss_parameter : t -> bool
    (** [true] if the provider sends the [iss] parameter to the redirect URI
        (RFC 9207). *)

    val keys : t -> Jwt.Key.t list
    (** [keys t] is the keys of the JWK Set of [t] ([jwks_uri]), from a cache:
        - The keys stay fresh for the [max-age] of the [Cache-Control] of the
          response, from 5 min to 24 h. With no [max-age], or with [no-cache] or
          [no-store]: 1 h.
        - After that time, the first need loads the keys again, in the request
          that needs them. One load at a time: the other callers wait for its
          result. A caller that is cancelled does not stop the load. The load
          has the deadline [timeout] of the provider.
        - A failed load keeps the old keys, and the next load comes after 1 min,
          or when the old keys stop being fresh, if that is later. A failure is
          a timeout, an error of the network, a status other than 200, more than
          64 KiB, a set that {!Jwt.Key.of_jwks} refuses, or a set with no key
          that caravan can use. *)
  end

  type error =
    | Jwt of Jwt.error
    | Missing_nonce  (** A nonce was expected, and the token has none. *)
    | Bad_nonce  (** The nonce of the token is not the expected nonce. *)
    | Bad_azp
        (** The [azp] claim is not the client ID, or a token for more than one
            audience has no [azp]. *)
    | Keys of string
        (** The provider has no key: the loads failed, with this cause. *)

  val pp_error : Format.formatter -> error -> unit

  val verify_id_token :
       ?leeway:Weft.Span.t
    -> ?nonce:string
    -> ?issuers:string list
    -> Provider.t
    -> client_id:string
    -> string
    -> (Jwt.claims, error) result
  (** [verify_id_token provider ~client_id token] checks an ID token (OIDC Core
      §3.1.3.7):
      - {!Jwt.verify} with the keys of the provider, the issuer of the provider
        (or one of [issuers]), and the audience [client_id]. [exp] is necessary.
        [leeway] is that of {!Jwt.verify}.
      - [No_key]: if the last load of the keys is 1 min old or more, one more
        load (a new key after a rotation), then one more check. Thus a token of
        an unknown key costs at most one load each minute.
      - A [typ] of [at+jwt] is refused: an access token is not an ID token (RFC
        8725 §3.11). A token with no [sub], or an empty one, gives
        [Jwt (Missing "sub")].
      - [azp], if there, must be [client_id]. A token with more than one
        audience must have [azp]. Other audiences are then permitted.
      - With [nonce], the [nonce] claim must be equal to it. With no [nonce],
        nothing stops a replay of a token that was taken: give a nonce if the
        flow has one.

      A token of an other client of the same project, for example a token that
      an Android app of Google got for the web client, can have [azp] set to
      that other client: it gives [Bad_azp].

      Google signs the tokens of its button with two issuer texts:
      [~issuers:["https://accounts.google.com"; "accounts.google.com"]]. The
      button "Sign in with Google" sends an ID token in a form, with no code
      flow. Use its popup mode: the JavaScript callback of the page sends the
      token in a POST of the same origin, with a CSRF token
      ({!Caravan_session.Csrf}). Then check the token with these issuers and a
      nonce that the page gave to the button. In the redirect mode, the browser
      sends a cross-site POST, with no session cookie: only the cookie
      [g_csrf_token] protects it, and a host of a sibling domain can set that
      cookie. Prefer the code flow ({!login}).

      @raise Invalid_argument as {!Jwt.verify}, for example for [issuers = []].
  *)

  (** {2:flow The login and the callback}

      The authorization code flow with PKCE (OIDC Core §3.1, RFC 7636): {!login}
      sends the browser to the provider, and the provider sends it back to
      {!callback} with a code. The callback gets the tokens, checks the ID
      token, and calls a function of the application. That function finds or
      makes the user, calls {!Caravan_auth.sign_in}, and gives the response.

      {[
      let google =
        Caravan_auth.Oidc.client ~secret ~provider ~id:"123.apps"
          ~redirect_uri:"https://app.example/auth/callback" ()

      (* The key of an account is the issuer and the subject. Never find or link
         an account by the email claim. *)
      let signed_in req (t : Caravan_auth.Oidc.tokens) ~return =
        let issuer = Option.get (Caravan_auth.Jwt.issuer t.claims) in
        let user = find_or_make_user ~issuer ~subject:t.subject in
        Caravan_auth.sign_in req user ;
        Caravan.Response.empty ~status:Caravan.Status.see_other
          ~headers:(Caravan.Headers.of_list [("Location", return)])
          ()

      let routes =
        Caravan_router.
          [ get "/auth/login" (Caravan_auth.Oidc.login google)
          ; get "/auth/callback" (Caravan_auth.Oidc.callback google signed_in)
          ]
      ]}

      The two handlers need the middleware of [caravan.session]. Each login
      makes a session if the request has none: put a rate limit on the login
      route ([Caravan_middleware.Rate_limit]). The session cookie must be
      [SameSite=Lax] (the default) or [None]: with [Strict], the browser does
      not send it with the redirect of the provider, and the callback gives
      [No_login].

      Give each client its own redirect URI: the callback finds only the logins
      of its client (RFC 9700 §4.4.2). Two requests of one session at the same
      time can lose a change of the other: the session middleware writes all the
      data of the session at the end of a request. Thus two logins at the same
      time can lose one login, and two callbacks of one login at the same time
      can both find it. The provider refuses the second use of a code (RFC 6749
      §4.1.2). *)

  (** The type for a client of a provider: its registration. *)
  type client

  val client :
       ?scope:string list
    -> ?secret:string
    -> ?auth:[`Basic | `Post]
    -> ?leeway:Weft.Span.t
    -> ?login_timeout:Weft.Span.t
    -> ?extra:(string * string) list
    -> provider:Provider.t
    -> id:string
    -> redirect_uri:string
    -> unit
    -> client
  (** [client ~provider ~id ~redirect_uri ()] is the client [id] of [provider].
      - [scope] (default [["openid"; "email"; "profile"]]) gets [openid] if it
        does not have it.
      - [secret] is the client secret. With no secret, the client is public:
        PKCE protects the code, and the token request has only [client_id].
      - [auth] is how the secret goes: [`Basic] (the [Authorization] field) or
        [`Post] (the form). The default: [`Basic], unless the provider lists
        [client_secret_post] and not [client_secret_basic] (see
        {!Provider.auth_methods}).
      - [leeway] is that of {!verify_id_token}.
      - [login_timeout] (default 10 min) is the time from {!login} to
        {!callback}.
      - [extra] are more parameters of the authorization request, for example
        [["prompt", "select_account"]].

      An [http] redirect URI is for a native app on the same host. RFC 8252 §8.3
      recommends [127.0.0.1] or [[::1]], not [localhost].

      @raise Invalid_argument
        if [id] is empty; [redirect_uri] is not [https], or [http] of
        [localhost], [127.0.0.1] or [[::1]] (RFC 8252 §7.3); [login_timeout] is
        less than 1 s; [leeway] is negative; a scope is empty or has a space or
        a control character; [extra] has a parameter that {!login} sets or that
        changes the checks ([response_type], [client_id], [redirect_uri],
        [scope], [state], [nonce], [code_challenge], [code_challenge_method],
        [max_age], [request], [request_uri], [response_mode]); or the provider
        lists neither [client_secret_basic] nor [client_secret_post] for a
        client with a secret and no [auth]. *)

  val login : ?return:string -> client -> Caravan.Handler.t
  (** [login c] is a handler that starts a login: a [303] to the authorization
      endpoint, with [response_type=code], [client_id], [redirect_uri], [scope],
      [state], [nonce], [code_challenge] (S256) and the [extra] parameters. The
      response has [Cache-Control: no-store] and [Referrer-Policy: no-referrer].
      - It keeps the login in the session: a random state, nonce and PKCE
        verifier (32 bytes each), the time, and the return path. A session keeps
        at most 4 logins, the newest. Logins in more tabs give [No_login] to the
        oldest. A callback removes only its login: the other logins stay after a
        sign-in that keeps the data of the session (see
        {!Caravan_auth.sign_in}).
      - The return path is [return], else {!Caravan_auth.return_path}, else
        ["/"]. A path that is not a path of this site, or that has more than 1
        KiB, becomes ["/"].

      @raise Invalid_argument
        as {!Caravan_session.set}, for example with no session middleware. *)

  (** The tokens of a callback. *)
  type tokens =
    { id_token: string  (** The text of the ID token. *)
    ; access_token: string option
    ; expires_in: int option  (** The life of the access token, in seconds. *)
    ; subject: string
          (** The [sub] claim of the ID token: the ID of the user at the
              provider. With the issuer, it is the key of an account. *)
    ; claims: Jwt.claims  (** The claims of the checked ID token. *) }

  (** The type for the failures of a callback. *)
  type failure =
    | No_login
        (** No login of the session has the [state] of the callback, for this
            client: for example a callback that comes two times, a login of
            another browser, or a login that 4 newer logins pushed out. *)
    | Expired  (** The login is older than [login_timeout]. *)
    | Denied of string * string option
        (** The provider sent [error] (for example [access_denied]) and
            [error_description]. *)
    | Bad_response of string
        (** The callback has no code, its [iss] is not the issuer (RFC 9207), or
            the token response is not correct. *)
    | Token of string
        (** The token request failed: the network, a timeout, or a status other
            than 200 (with the [error] of the response). *)
    | Id_token of error  (** {!verify_id_token} refused the ID token. *)

  val pp_failure : Format.formatter -> failure -> unit

  val callback :
       ?failed:(Caravan.Request.t -> failure -> Caravan.Response.t)
    -> client
    -> (Caravan.Request.t -> tokens -> return:string -> Caravan.Response.t)
    -> Caravan.Handler.t
  (** [callback c f] is the handler of the redirect URI of [c]:
      - A method other than [GET]: [405].
      - The [state] parameter comes first: with no login of this state and
        client, the result is [No_login], and no other parameter is read.
      - The login goes from the session at once, with the expired logins.
      - If the callback has [iss], it must be the issuer. If the provider tells
        that it sends [iss] ({!Provider.iss_parameter}), it must be there.
      - An [error] parameter gives [Denied].
      - The token request: a [POST] of the code, the redirect URI and the PKCE
        verifier to the token endpoint, with the secret as [auth] tells, and the
        deadline of the provider. The response must be [200] with a JSON object
        of at most 64 KiB, with [id_token] and a [token_type] of [Bearer].
      - {!verify_id_token} with the nonce of the login.
      - Then [f req tokens ~return], with the return path of the login.

      [failed] gives the response of a failure: by default a [400] with no
      detail. The texts of [Denied] come from the query: escape them in HTML.
      Each response gets [Referrer-Policy: no-referrer] if it has none: the URL
      of the callback has the code. *)
end

(** {1:code Codes by email} *)

(** Sign-in with a one-time code that the application sends to an address, for
    example by email. The user types the code in the same browser.

    {[
    let codes = Caravan_auth.Code.make ()

    (* POST /email, with a rate limit: each start can make a session. *)
    let email req =
      let address = normalize (form_field "email" req) in
      ( match Caravan_auth.Code.start codes req ~address with
      | Ok code ->
          (* A job of caravan.jobs: the response does not wait for the mail. *)
          ignore (Caravan_jobs.enqueue jobs send_code (address, code))
      | Error _wait ->
          () ) ;
      (* The same response for each case: no hint of the accounts. *)
      page "We sent a code if the address is correct."

    (* POST /code, with a rate limit. *)
    let code req =
      match Caravan_auth.Code.check codes req (form_field "code" req) with
      | Ok address ->
          Caravan_auth.sign_in req (find_or_make_user address) ;
          redirect "/"
      | Error _ ->
          page "The code is not correct."
    ]}

    Rules for the application:
    - Call {!start} for an address with no account too, give the same response
      for each result, and send the message in the background: the response and
      its time must not tell which addresses have an account. Make the session
      first (for example at the [GET] of the form): else only [Ok] sets the
      session cookie, and that tells a refusal of [sends].
    - Give a normalized address (for example the domain in lowercase). The
      limits use the address in lowercase, with no spaces at its ends: the
      account lookup must not be coarser than that.
    - Check with a [POST] form. A link in the message must open a form, not
      check the code with a [GET]: mail scanners open links, and a URL goes to
      logs. The code works only in the browser of {!start}.
    - Put [Caravan_middleware.Rate_limit] on the routes of {!start} and
      {!check}: each [start] can make a session, and the limits of [Code] are
      for each address, not for each client.
    - After [Ok], call {!Caravan_auth.sign_in} (it renews the session), or
      [Caravan_session.renew] in a flow with no sign-in: a request of the same
      session that runs at the same time can else write the code back.
    - Give one [checks] to the [t] values that sign in the same accounts (for
      example two purposes): each [make] has its own default limiters.

    Limits:
    - With the default [checks], an attacker who does not have the mailbox makes
      about 8760 guesses a year for one address: with 8 digits (the default), a
      chance of about 0.009 % a year; with 6 digits, about 0.9 %. Over many
      addresses, the chances add.
    - The default limiters are in the memory of the process. [N] processes give
      [N] times the guesses. A full table forgets the buckets nearest to full
      (see [Caravan_middleware.Rate_limit.Limiter]): a flood of new addresses
      can give the old addresses new tokens. For a deployment of more processes,
      or for a larger table, give [sends] and [checks]: for example
      [Limiter.take] of a limiter with a larger [max_keys], or a function of a
      shared store.
    - The other side of the limits: one wrong code each hour keeps the code
      sign-in of an address closed, and one {!start} each 10 min keeps its
      [sends] empty (and sends up to 147 messages a day). Give a second way to
      sign in, for example a passkey or an OpenID provider. Log the failures
      ({!failure} has the address).
    - NIST SP 800-63B rev. 3 §5.2.2 limits the failures in a row to 100 for each
      account: [checks] limits a rate, not a count. NIST does not accept email
      for out-of-band authentication (rev. 3 §5.1.3.1): a code by email has the
      strength of a mail to reset a password.
    - The session keeps a salted SHA-256 of the code, not the code. A reader of
      the store can find a code from its hash, but the code works only in its
      session.
    - Two checks of one session at the same time read the same attempts: then
      only [checks] limits them. Two right checks at the same time both give
      [Ok]. *)
module Code : sig
  type t

  val make :
       ?purpose:string
    -> ?digits:int
    -> ?ttl:Weft.Span.t
    -> ?attempts:int
    -> ?sends:(string -> Weft.Span.t option)
    -> ?checks:(string -> Weft.Span.t option)
    -> unit
    -> t
  (** [make ()] is a kind of code:
      - [purpose] (default ["sign-in"]) separates the codes of two flows (for
        example a sign-up and a sign-in): a session keeps one code for each
        purpose.
      - [digits] (default 8) is the length of a code.
      - [ttl] (default 10 min) is the life of a code.
      - [attempts] (default 5) is the number of wrong codes before the code
        goes.
      - [sends] (default: 3, then one each 10 min) limits the codes for each
        address. [checks] (default: 10, then one each hour) limits the checks
        for each address, also of right codes. Each is a function of a key:
        [None] lets the action go, [Some wait] refuses it, as
        [Caravan_middleware.Rate_limit.Limiter.take]. The keys have a prefix for
        each use ([send:] or [check:]) and a hash of the address: one function
        can serve the two, and more [t] values.

      @raise Invalid_argument
        if [purpose] is empty or has a character other than a letter, a digit
        and ['-']; [digits] is not 6 to 9; [ttl] is not 1 min to 1 h; or
        [attempts] is not 1 to 10. *)

  val start :
    t -> Caravan.Request.t -> address:string -> (string, Weft.Span.t) result
  (** [start t req ~address] makes a random code for [address], and keeps its
      hash, the address, the expiry and the attempts in the session of [req]. It
      replaces the code of the session for this purpose. [Ok code]: send [code]
      to [address]. [Error wait]: [sends] refused the address for [wait]; the
      session does not change. A cancellation point.

      @raise Invalid_argument
        if [address] is empty or has more than 320 bytes, or as
        {!Caravan_session.set}. *)

  (** The type for the failures of {!check}. *)
  type failure =
    | No_code  (** The session has no code for this purpose. *)
    | Expired  (** The code is older than [ttl]: {!check} removes it. *)
    | Wrong of {address: string; left: int}
        (** A wrong code: [left] attempts are left. At 0, {!check} removes the
            code from the session. *)
    | Too_many of {address: string; wait: Weft.Span.t}
        (** [checks] refused the address for [wait]. The attempts do not change.
        *)

  val pp_failure : Format.formatter -> failure -> unit

  val check : t -> Caravan.Request.t -> string -> (string, failure) result
  (** [check t req text] checks [text] against the code of the session. Spaces
      and ['-'] in [text] are removed; a text of more than 64 bytes is a wrong
      code. First the expiry, then a token of [checks] for the address, then the
      compare (constant time). [Ok address]: {!check} removes the code from the
      session (one use); sign the user in ({!Caravan_auth.sign_in}). A text that
      is not [digits] digits is a wrong code. A cancellation point.

      @raise Invalid_argument as {!Caravan_session.set}. *)
end

(** {1:passkey Passkeys}

    Registration and sign-in with passkeys (W3C WebAuthn Level 3, the server
    part). The server gives the options as JSON, the browser calls
    [navigator.credentials.create] or [.get], and the server checks the JSON of
    the response.

    {[
    // The browser. With a CSRF token on the POST (Caravan_session.Csrf).
    const options = await (await fetch("/passkey/register-options")).json();
    const credential = await navigator.credentials.create({
      publicKey: PublicKeyCredential.parseCreationOptionsFromJSON(options) });
    await fetch("/passkey/register", { method: "POST",
      body: JSON.stringify(credential.toJSON()) });
    ]}

    {[
    let rp =
      Caravan_auth.Passkey.rp ~id:"example.com" ~name:"Example"
        ~origins:["https://example.com"]

    (* GET /passkey/register-options, for a signed-in user. *)
    let register_options req user =
      Caravan_auth.Passkey.creation_options rp req ~user_id:user.handle
        ~user_name:user.email ~display_name:user.name
        ~exclude:(credential_ids user)

    (* POST /passkey/register, with a body of at most 64 KiB. *)
    let register req body =
      match
        Caravan_auth.Passkey.register rp req ~registered:credential_exists body
      with
      | Ok c ->
          store_credential c.id c.stored
      | Error e ->
          refuse e

    (* POST /passkey/sign-in, after request_options. *)
    let sign_in req body =
      match
        Caravan_auth.Passkey.authenticate rp req ~lookup:find_credential body
      with
      | Ok a when a.cloned ->
          refuse_or_warn a
      | Ok a ->
          update_sign_count a.credential_id a.sign_count ;
          Caravan_auth.sign_in req (user_of_handle a.user_handle)
      | Error e ->
          refuse e
    ]}

    - Each ceremony keeps a random challenge of 32 bytes in the session: at most
      4 at a time (an autofill sign-in and a click can both wait), for the time
      of [timeout]. When a response has the challenge of a ceremony, the
      ceremony goes from the session, also when a later check fails. A response
      with bad JSON, another type, or no challenge does not take a ceremony.
    - The user handle of a response is not signed. {!authenticate} compares it
      with the handle of the stored credential, and gives the stored handle:
      find the user with [assertion.user_handle].
    - Attestation: the options ask for none, and a statement of another format
      is not verified. [credential.aaguid] is not verified: do not base a
      decision on it.
    - Store the credential ID with a UNIQUE index: [registered] checks it, but
      two registrations at the same time can both pass.
    - The RP ID is not checked against the public suffix list.
    - Read the body of a response with a limit of 64 KiB: [register] and
      [authenticate] refuse a larger one, but [Caravan.Config.max_body] is 8 MiB
      by default.
    - Two requests of one session at the same time can lose a change of the
      other (the session middleware writes all the data of the session at the
      end of a request): a response sent two times in parallel can pass two
      times. Each options call can make a session for an anonymous visitor: put
      a rate limit on these routes. *)
module Passkey : sig
  (** The type for a relying party: its RP ID, name and origins. *)
  type rp

  val rp : id:string -> name:string -> origins:string list -> rp
  (** [rp ~id ~name ~origins] is a relying party.
      - [id] is a domain in lowercase ASCII (labels of letters, digits and
        ['-'], separated by ['.']), not an IP address, for example
        ["example.com"] or ["login.example.com"].
      - Each origin is [https://host] or [https://host:port] (not [:443]), with
        [host] the RP ID or a subdomain of it, in lowercase, with no path. For
        development, the RP ID [localhost] also accepts [http://localhost] and
        [http://localhost:port]. A response must have one of these origins
        exactly.

      @raise Invalid_argument
        if [id], [name] or an origin is not correct, or [origins] is empty. *)

  (** A requirement of WebAuthn: of the user verification (a PIN or a biometric
      check of the authenticator), or of a resident key. *)
  type requirement = [`Required | `Preferred | `Discouraged]

  val creation_options :
       ?timeout:Weft.Span.t
    -> ?verification:requirement
    -> ?resident_key:requirement
    -> ?exclude:string list
    -> rp
    -> Caravan.Request.t
    -> user_id:string
    -> user_name:string
    -> display_name:string
    -> string
  (** [creation_options rp req ~user_id ~user_name ~display_name] is the JSON of
      [PublicKeyCredentialCreationOptionsJSON], for the registration of a
      passkey of the user [user_id] (the user handle: 1 to 64 random bytes of
      the application, with no personal data). It keeps a new ceremony in the
      session.
      - The algorithms: EdDSA, ES256 and RS256. The attestation: none.
      - [resident_key] (default [`Required]: a passkey that the authenticator
        can find with no user name).
      - [verification] (default [`Preferred]): with [`Required], a response with
        no user verification is refused.
      - [exclude] are the IDs of the credentials of the user: an authenticator
        does not make a second one.
      - [timeout] (default 5 min, 30 s to 10 min) is the time of the browser and
        the life of the challenge.

      @raise Invalid_argument
        for a bad [timeout] or [user_id], or as {!Caravan_session.set}. *)

  val request_options :
       ?timeout:Weft.Span.t
    -> ?verification:requirement
    -> ?allow:string list
    -> ?user:string
    -> rp
    -> Caravan.Request.t
    -> string
  (** [request_options rp req] is the JSON of
      [PublicKeyCredentialRequestOptionsJSON], for a sign-in. It keeps a new
      ceremony in the session.
      - With no [user], any passkey of the RP can sign in (a discoverable
        credential, also for the autofill of the browser): the response must
        have a user handle.
      - With [user] (the user handle of a user that the application knows, for
        example after a password), only a credential of this user is accepted.
        [allow] gives the IDs of its credentials to the browser.

      @raise Invalid_argument
        for a bad [timeout], for [allow] with no [user] (the server checks the
        user, not the list), or as {!Caravan_session.set}. *)

  (** The part of a credential to store, for a sign-in. *)
  type stored =
    { public_key: string  (** The COSE key. *)
    ; sign_count: int
    ; user_handle: string
    ; backup_eligible: bool
          (** The credential can be in a backup (a synced passkey). *) }

  (** A new credential. *)
  type credential =
    { id: string  (** The credential ID: 1 to 1023 bytes. *)
    ; stored: stored  (** Store it with [id]. *)
    ; user_verified: bool
    ; backed_up: bool
    ; transports: string list
          (** The transports that the browser told, for the records of the
              application. *)
    ; aaguid: string  (** 16 bytes; not verified. *) }

  (** A sign-in. *)
  type assertion =
    { credential_id: string
    ; user_handle: string  (** The stored handle: the user. *)
    ; sign_count: int
          (** The larger of the new and the stored counter: store it. *)
    ; user_verified: bool
    ; backed_up: bool
    ; cloned: bool
          (** The counter did not grow, and one of the two counters is not 0:
              the authenticator can be a copy (§6.1.1). Passkeys that sync send
              0 each time: then [false]. The application decides. *) }

  type error =
    | No_ceremony
        (** No ceremony of the session has the challenge and the kind of the
            response. *)
    | Expired  (** The ceremony is older than its [timeout]. *)
    | Bad_response of string
        (** The JSON, the CBOR or a member is not correct. *)
    | Bad_origin of string
    | Bad_rp_id  (** The authenticator data is for another RP ID. *)
    | Not_present  (** The user was not present (the flag UP). *)
    | Not_verified  (** [`Required] verification, and none. *)
    | Registered  (** [registered] knows the credential ID. *)
    | Unknown_credential  (** [lookup] does not know the credential ID. *)
    | Wrong_user
        (** The credential is not of the [user] of the options, or the user
            handle of the response is not the stored one. *)
    | Bad_backup  (** The flag BE is not that of the registration. *)
    | Bad_signature
    | Unsupported of string  (** A key or an alg that caravan does not use. *)

  val pp_error : Format.formatter -> error -> unit

  val register :
       rp
    -> Caravan.Request.t
    -> registered:(string -> bool)
    -> string
    -> (credential, error) result
  (** [register rp req ~registered text] checks the JSON of
      [credential.toJSON()] of [create] (WebAuthn §7.1):
      - At most 64 KiB, [id] equal to [rawId].
      - The client data: the type [webauthn.create], the challenge of a ceremony
        of {!creation_options}, one of the origins, [crossOrigin] absent or
        false, and no [topOrigin].
      - The attestation object: [none] with no statement, or another format (not
        verified).
      - The authenticator data: the RP ID hash, the flag UP, the flag UV if the
        verification was required, the attested credential data with the ID of
        the response, a key of the asked algorithms.
      - [registered id = false]. *)

  val authenticate :
       rp
    -> Caravan.Request.t
    -> lookup:(string -> stored option)
    -> string
    -> (assertion, error) result
  (** [authenticate rp req ~lookup text] checks the JSON of
      [credential.toJSON()] of [get] (WebAuthn §7.2):
      - At most 64 KiB, [id] equal to [rawId].
      - The client data: the type [webauthn.get], the challenge of a ceremony of
        {!request_options}, one of the origins, [crossOrigin] absent or false,
        and no [topOrigin].
      - The credential [lookup id], of the [user] of the options if there is
        one. The user handle of the response, if there, must be its stored
        handle; it must be there with no [user].
      - The authenticator data: the RP ID hash, the flags UP, UV (if required)
        and BE.
      - The signature of the authenticator data and of the hash of the client
        data, with the stored key. *)
end

(**/**)

(** Internal: for the tests. *)
module Private : sig
  module Cbor = Cbor
  module Webauthn = Webauthn

  val api_key_char : int -> char option

  val api_key_cache_counts : Api_key.cache -> int * int
  (** The count and the cardinal of the map. *)

  val api_key_insert : Api_key.cache -> string -> Weft.Clock.mono -> unit
  (** An insert of the user "x" at a time, with no effect. *)

  val key_of_cose : string -> (Jwt.Key.t * int, string) result

  val verify : Jwt.Key.t -> data:string -> signature:string -> bool

  val passkey_ceremony :
       Caravan.Request.t
    -> create:bool
    -> challenge:string
    -> uv:bool
    -> ?user:string
    -> unit
    -> unit
end
