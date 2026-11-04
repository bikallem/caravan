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

      let signed_in req (t : Caravan_auth.Oidc.tokens) ~return =
        let user =
          find_or_make_user t.subject (Caravan_auth.Jwt.string "email" t.claims)
        in
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
          send_in_background address code
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
