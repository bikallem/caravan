(** An ES256 key pair in memory, and the JSON Web Tokens (JWTs) that it signs,
    for development and tests. Never use it in production: there, an identity
    provider signs the tokens, and the application gets only its public keys.

    {!make} takes its random bytes from [Mirage_crypto_rng.default_generator]:
    set it first. *)

(** The type for a P-256 key pair and its key ID. *)
type t

val make : ?kid:string -> unit -> t
(** [make ~kid ()] is a new P-256 key pair with the key ID [kid] (default
    ["dev"]). Each call makes a new key pair.

    @raise Invalid_argument
      if [kid] is empty, or has a character other than [A-Z], [a-z], [0-9], [-],
      [_] and [.]. *)

val kid : t -> string

val jwks : t -> string
(** [jwks t] is the JWK Set of the public key of [t], as an identity provider
    publishes it: one key with [kty] [EC], [crv] [P-256], the key ID, [alg]
    [ES256] and [use] [sig]. [Caravan_auth.Jwt.Key.of_jwks] reads it. *)

val sign : t -> string -> string
(** [sign t claims] is a JWT in the compact form of JWS, with the header
    [{"alg":"ES256","typ":"JWT","kid":<kid>}] and the payload [claims]. [claims]
    is the JSON text of an object, for example [{"sub":"ada","exp":1750003600}].
    [sign] does not check it. *)
