(* Internal. The binary parts of WebAuthn (W3C WebAuthn Level 3): COSE keys, the
   DER form of an ECDSA signature, and the authenticator data. *)

(** A public key of a COSE_Key map (RFC 9052 §7, RFC 9053), with its algorithm.
    The bytes are not yet checked as a point or as an RSA key. *)
type cose =
  | Ec2 of {alg: int; x: string; y: string}
      (** kty 2, crv 1 (P-256), alg -7 (ES256) or -9 (ESP256); x and y have 32
          bytes. *)
  | Okp of {alg: int; x: string}
      (** kty 1, crv 6 (Ed25519), alg -8 (EdDSA) or -19 (Ed25519); x has 32
          bytes. *)
  | Rsa of {alg: int; n: string; e: string}  (** kty 3, alg -257 (RS256). *)

val cose_key : Cbor.t -> (cose, string) result
(* [cose_key c] is the key of the map [c]. A map with a private part (the label
   -4) is an error. Each other kty, alg, curve or form (for example a boolean y,
   a compressed point) is an error. An alg that does not belong to the kty is an
   error: no algorithm confusion. *)

val der_signature : string -> (string * string) option
(* [der_signature s] is r and s of a DER ECDSA signature (RFC 3279 §2.2.3):
   SEQUENCE { INTEGER r, INTEGER s } with short lengths, each INTEGER positive,
   not 0, in its shortest form, of at most 32 bytes of value; nothing after it.
   r and s have 32 bytes (with zeros on the left). *)

(** The attested credential data (§6.5.1). *)
type attested =
  { aaguid: string  (** 16 bytes; not verified. *)
  ; credential_id: string  (** 1 to 1023 bytes. *)
  ; public_key: string  (** The bytes of the COSE key. *)
  ; key: Cbor.t  (** The COSE key. *) }

(** The authenticator data (§6.1). *)
type auth_data =
  { rp_id_hash: string  (** 32 bytes. *)
  ; flags: int
  ; sign_count: int
  ; attested: attested option
  ; extensions: Cbor.t option }

val up : auth_data -> bool
(* User present (bit 0). *)

val uv : auth_data -> bool
(* User verified (bit 2). *)

val be : auth_data -> bool
(* Backup eligible (bit 3). *)

val bs : auth_data -> bool
(* Backed up (bit 4). *)

val parse_auth_data : string -> (auth_data, string) result
(* [parse_auth_data s]: 37 bytes or more; with the flag AT, the attested
   credential data; with the flag ED, a CBOR map of extensions; nothing after.
   The flag BS with no BE is an error (§6.1). *)
