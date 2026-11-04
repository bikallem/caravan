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
