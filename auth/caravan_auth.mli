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
        [keys]. Load the keys at the start, or in the background, not in a
        request: a large set takes milliseconds.

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
    ([authenticate], [keys], [user], [validate]):
    - must not raise: their exception goes out of {!user}, {!failure},
      {!challenge} or {!require}, and a later call runs the schemes again;
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
        if [name] is empty, ["Bearer"] or ["Session"]: the names of the schemes
        of this library. *)

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
    [require ~challenge:"Bearer"] on the routes of an API.

    With the session scheme, a cross-site form can send the session cookie.
    Protect the routes with [Caravan_middleware.Cross_origin] first, and with
    [Caravan_session.Csrf.middleware] after this middleware, with the bearer
    users exempt (a browser never sends a bearer token by itself):
    {[
    Caravan.Handler.compose
      [ Caravan_middleware.Cross_origin.middleware ()
      ; Caravan_session.middleware ~keys ~store ()
      ; Caravan_auth.middleware ~schemes:[session; bearer] ()
      ; Caravan_session.Csrf.middleware
          ~exempt:(fun r ->
            match Caravan_auth.user r with
            | Some u ->
                Caravan_auth.User.scheme u = "Bearer"
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
