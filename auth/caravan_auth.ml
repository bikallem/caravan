module Base64 = Caravan.Private.Base64

let b64_decode = Base64.decode ~url:true ~pad:false

(* [string_list l] is the strings of [l], if each element is a string. *)
let string_list l =
  let s = List.filter_map (function `String s -> Some s | _ -> None) l in
  if List.length s = List.length l then Some s else None

(* [number_of v] is the value of a JSON number. [Json.parse] keeps only finite
   numbers. *)
let number_of = function
  | `Int i ->
      Some (float_of_int i)
  | `Float f ->
      Some f
  | `Intlit s ->
      Some (float_of_string s)
  | _ ->
      None

module Jwt = struct
  type alg = HS256 | RS256 | ES256 | EdDSA

  (* The public part of a key, or the secret of HS256. *)
  type material =
    | Hmac of string
    | Rsa of Mirage_crypto_pk.Rsa.pub
    | P256 of Mirage_crypto_ec.P256.Dsa.pub
    | Ed25519 of Mirage_crypto_ec.Ed25519.pub

  module Key = struct
    type t = {kid: string option; material: material}

    let alg k =
      match k.material with
      | Hmac _ ->
          HS256
      | Rsa _ ->
          RS256
      | P256 _ ->
          ES256
      | Ed25519 _ ->
          EdDSA

    let kid k = k.kid

    let of_secret ?kid secret =
      if String.length secret < 32 then
        invalid_arg
          "Caravan_auth.Jwt.Key.of_secret: a secret has 32 bytes or more" ;
      {kid; material= Hmac secret}

    let of_secret_base64url ?kid text =
      match b64_decode text with
      | Some secret when String.length secret >= 32 ->
          {kid; material= Hmac secret}
      | _ ->
          invalid_arg
            "Caravan_auth.Jwt.Key.of_secret_base64url: not base64url of 32 \
             bytes or more"

    (* RSA keys of 2048 to 8192 bits (RFC 7518 §3.3), with an exponent of at
       most 32 bits: a larger key makes each check slow. *)
    let rsa_ok (p : Mirage_crypto_pk.Rsa.pub) =
      let bits = Z.numbits p.n in
      bits >= 2048 && bits <= 8192 && Z.numbits p.e <= 32

    let of_public kid = function
      | `RSA p when rsa_ok p ->
          Some {kid; material= Rsa p}
      | `P256 p ->
          Some {kid; material= P256 p}
      | `ED25519 p ->
          Some {kid; material= Ed25519 p}
      | `RSA _ | `P384 _ | `P521 _ ->
          None

    let of_pem ?kid pem =
      let key =
        match X509.Public_key.decode_pem pem with
        | Ok k ->
            Ok k
        | Error _ ->
            Result.map X509.Certificate.public_key
              (X509.Certificate.decode_pem pem)
      in
      match key with
      | Error _ ->
          invalid_arg
            "Caravan_auth.Jwt.Key.of_pem: not a public key or a certificate in \
             PEM"
      | Ok k -> (
        match of_public kid k with
        | Some k ->
            k
        | None ->
            invalid_arg
              "Caravan_auth.Jwt.Key.of_pem: not an RSA key of 2048 to 8192 \
               bits, a P-256 key or an Ed25519 key" )

    let ( let* ) = Option.bind

    let guard b = if b then Some () else None

    let member name = function `Assoc l -> List.assoc_opt name l | _ -> None

    let string name j =
      match member name j with Some (`String s) -> Some s | _ -> None

    (* [optional name j] is the value of the member [name] of [j] if it is a
       string, and [None] if [j] has no member [name]. A member of another type
       gives [Error ()]: the key is skipped. *)
    let optional name j =
      match member name j with
      | None ->
          Ok None
      | Some (`String s) ->
          Ok (Some s)
      | Some _ ->
          Error ()

    let bytes name j = Option.bind (string name j) b64_decode

    (* The number of the big-endian bytes [s]. *)
    let z_of_bytes s =
      let n = String.length s in
      Z.of_bits (String.init n (fun i -> s.[n - 1 - i]))

    let exactly n s = if String.length s = n then Some s else None

    (* [of_jwk j] is the key of the JWK [j], or [None] if this library cannot
       use it. *)
    let of_jwk j =
      let* kid = Result.to_option (optional "kid" j) in
      let* use = Result.to_option (optional "use" j) in
      let* alg = Result.to_option (optional "alg" j) in
      let* () = guard (use = None || use = Some "sig") in
      let* () =
        guard
          ( match member "key_ops" j with
          | None ->
              true
          | Some (`List l) ->
              List.mem (`String "verify") l
          | Some _ ->
              false )
      in
      let alg_is names =
        guard (match alg with None -> true | Some a -> List.mem a names)
      in
      let* material =
        match string "kty" j with
        | Some "RSA" ->
            let* () = alg_is ["RS256"] in
            let* n = bytes "n" j in
            let* e = bytes "e" j in
            let* p =
              Result.to_option
                (Mirage_crypto_pk.Rsa.pub ~e:(z_of_bytes e) ~n:(z_of_bytes n))
            in
            let* () = guard (rsa_ok p) in
            Some (Rsa p)
        | Some "EC" ->
            let* () = alg_is ["ES256"] in
            let* () = guard (string "crv" j = Some "P-256") in
            let* x = Option.bind (bytes "x" j) (exactly 32) in
            let* y = Option.bind (bytes "y" j) (exactly 32) in
            Option.map
              (fun p -> P256 p)
              (Result.to_option
                 (Mirage_crypto_ec.P256.Dsa.pub_of_octets ("\x04" ^ x ^ y)) )
        | Some "OKP" ->
            let* () = alg_is ["EdDSA"; "Ed25519"] in
            let* () = guard (string "crv" j = Some "Ed25519") in
            let* x = Option.bind (bytes "x" j) (exactly 32) in
            Option.map
              (fun p -> Ed25519 p)
              (Result.to_option (Mirage_crypto_ec.Ed25519.pub_of_octets x))
        | _ ->
            None
      in
      Some {kid; material}

    let of_jwks ?(max_length = 65536) text =
      if max_length < 1 then
        invalid_arg "Caravan_auth.Jwt.Key.of_jwks: max_length is less than 1" ;
      if String.length text > max_length then Error "the text is too large"
      else
        match Json.parse text with
        | Error e ->
            Error e
        | Ok j -> (
          match member "keys" j with
          | Some (`List l) ->
              Ok (List.filter_map of_jwk l)
          | _ ->
              Error "no array keys" )
  end

  type expect = Any | One_of of string list

  type error =
    | Malformed of string
    | Unsupported of string
    | Missing of string
    | No_key
    | Bad_signature
    | Expired
    | Not_yet_valid
    | Bad_issuer
    | Bad_audience

  let pp_error ppf = function
    | Malformed m ->
        Format.fprintf ppf "Malformed %S" m
    | Unsupported m ->
        Format.fprintf ppf "Unsupported %S" m
    | Missing m ->
        Format.fprintf ppf "Missing %S" m
    | No_key ->
        Format.pp_print_string ppf "No_key"
    | Bad_signature ->
        Format.pp_print_string ppf "Bad_signature"
    | Expired ->
        Format.pp_print_string ppf "Expired"
    | Not_yet_valid ->
        Format.pp_print_string ppf "Not_yet_valid"
    | Bad_issuer ->
        Format.pp_print_string ppf "Bad_issuer"
    | Bad_audience ->
        Format.pp_print_string ppf "Bad_audience"

  type claims = {payload: string; members: (string * Yojson.Safe.t) list}

  let alg_of_name = function
    | "HS256" ->
        Some HS256
    | "RS256" ->
        Some RS256
    | "ES256" ->
        Some ES256
    | "EdDSA" | "Ed25519" ->
        Some EdDSA
    | _ ->
        None

  (* [jwt] or [at+jwt] (RFC 9068), ASCII case-insensitive, with an optional
     [application/] (RFC 7515 §4.1.9). *)
  let good_typ t =
    let t = String.lowercase_ascii t in
    let prefix = "application/" in
    let t =
      if String.starts_with ~prefix t then
        String.sub t (String.length prefix)
          (String.length t - String.length prefix)
      else t
    in
    t = "jwt" || t = "at+jwt"

  (* [signs key input digest signature] is [true] if [signature] is a signature
     of [input] with [key]. [digest] is the SHA-256 of [input], for ES256,
     computed once. *)
  let signs (key : Key.t) input digest signature =
    match key.material with
    | Hmac secret ->
        Eqaf.equal
          Digestif.SHA256.(to_raw_string (hmac_string ~key:secret input))
          signature
    | Rsa key ->
        Mirage_crypto_pk.Rsa.PKCS1.verify
          ~hashp:(function `SHA256 -> true | _ -> false)
          ~key ~signature (`Message input)
    | P256 key ->
        (* [r || s] of 32 bytes each (RFC 7518 §3.4), not DER. *)
        String.length signature = 64
        && Mirage_crypto_ec.P256.Dsa.verify ~key
             (String.sub signature 0 32, String.sub signature 32 32)
             (Lazy.force digest)
    | Ed25519 key ->
        Mirage_crypto_ec.Ed25519.verify ~key signature ~msg:input

  let ( let* ) = Result.bind

  let seconds span = Int64.to_float (Weft.Span.to_ns span) /. 1e9

  (* [object_of what text] is the members of the JSON object [text]. *)
  let object_of what text =
    match Json.parse text with
    | Ok (`Assoc l) ->
        Ok l
    | Ok _ ->
        Error (Malformed (what ^ " is not an object"))
    | Error e ->
        Error (Malformed (what ^ ": " ^ e))

  (* [optional name members] is the string member [name], if any. *)
  let optional name members =
    match List.assoc_opt name members with
    | None ->
        Ok None
    | Some (`String s) ->
        Ok (Some s)
    | Some _ ->
        Error (Malformed (name ^ " is not a string"))

  (* [time name members] is the number of the member [name], if any. *)
  let time name members =
    match List.assoc_opt name members with
    | None ->
        Ok None
    | Some v -> (
      match number_of v with
      | Some f ->
          Ok (Some f)
      | None ->
          Error (Malformed (name ^ " is not a number")) )

  (* [check_expect expect values ~missing ~bad] checks the values of a claim
     against [expect]. *)
  let check_expect expect values ~missing ~bad =
    match (expect, values) with
    | Any, _ ->
        Ok ()
    | One_of _, None ->
        Error (Missing missing)
    | One_of l, Some vs ->
        if List.exists (fun v -> List.mem v l) vs then Ok () else Error bad

  let check_claims ~leeway ~require_exp ~issuer ~audience members =
    let* exp = time "exp" members in
    let* nbf = time "nbf" members in
    let* _ = time "iat" members in
    let* iss = optional "iss" members in
    let* aud =
      match List.assoc_opt "aud" members with
      | None ->
          Ok None
      | Some (`String s) ->
          Ok (Some [s])
      | Some (`List (_ :: _ as l)) -> (
        match string_list l with
        | Some s ->
            Ok (Some s)
        | None ->
            Error (Malformed "aud has an element that is not a string") )
      | Some _ ->
          Error (Malformed "aud is not a string or a list of strings")
    in
    let* () =
      match (nbf, exp) with
      | Some nbf, Some exp when nbf > exp ->
          Error (Malformed "nbf is after exp")
      | _ ->
          Ok ()
    in
    let now = Weft.Time.posix_to_unix_s (Weft.Time.now Posix) in
    let* () =
      match exp with
      | None ->
          if require_exp then Error (Missing "exp") else Ok ()
      | Some exp ->
          if now < exp +. leeway then Ok () else Error Expired
    in
    let* () =
      match nbf with
      | Some nbf when now < nbf -. leeway ->
          Error Not_yet_valid
      | _ ->
          Ok ()
    in
    let* () =
      check_expect issuer
        (Option.map (fun s -> [s]) iss)
        ~missing:"iss" ~bad:Bad_issuer
    in
    check_expect audience aud ~missing:"aud" ~bad:Bad_audience

  (* [header text] is the alg and the kid of the header [text]. *)
  let header text =
    let* members = object_of "the header" text in
    let* alg =
      match List.assoc_opt "alg" members with
      | Some (`String a) -> (
        match alg_of_name a with
        | Some alg ->
            Ok alg
        | None ->
            Error (Unsupported ("alg " ^ a)) )
      | _ ->
          Error (Malformed "no alg string")
    in
    let* () =
      if List.mem_assoc "crit" members then Error (Unsupported "crit")
      else Ok ()
    in
    let* typ = optional "typ" members in
    let* () =
      match typ with
      | Some t when not (good_typ t) ->
          Error (Unsupported ("typ " ^ t))
      | _ ->
          Ok ()
    in
    let* kid = optional "kid" members in
    Ok (alg, kid)

  (* The most keys of the alg that a token with no kid tries, when no key has no
     kid: an identity provider that sends no kid has one or two keys in a
     rotation. *)
  let max_tries = 4

  (* [candidates alg kid keys] is the keys to try, in order. With a kid: the
     keys with this kid, then the keys with no kid. With no kid: the keys with
     no kid, or else the first [max_tries] keys of the alg. Thus a token with no
     kid costs at most [max_tries] checks when each key has a kid. *)
  let candidates alg kid keys =
    let of_alg = List.filter (fun k -> Key.alg k = alg) keys in
    let no_kid = List.filter (fun (k : Key.t) -> k.kid = None) of_alg in
    match kid with
    | Some id ->
        List.filter (fun (k : Key.t) -> k.kid = Some id) of_alg @ no_kid
    | None -> (
      match no_kid with
      | [] ->
          List.filteri (fun i _ -> i < max_tries) of_alg
      | _ ->
          no_kid )

  (* The largest header part: a header has a few members, and an [x5c]
     certificate chain can have some kilobytes. *)
  let max_header = 8192

  let check_expect_arg fn = function
    | One_of [] ->
        invalid_arg ("Caravan_auth.Jwt.verify: an empty list for " ^ fn)
    | Any | One_of _ ->
        ()

  let verify ?(leeway = Weft.Span.s 60) ?(max_length = 16384)
      ?(require_exp = true) ~issuer ~audience ~keys token =
    check_expect_arg "issuer" issuer ;
    check_expect_arg "audience" audience ;
    if Weft.Span.compare leeway Weft.Span.zero < 0 then
      invalid_arg "Caravan_auth.Jwt.verify: a negative leeway" ;
    if max_length < 1 then
      invalid_arg "Caravan_auth.Jwt.verify: max_length is less than 1" ;
    let leeway = seconds leeway in
    let n = String.length token in
    if n > max_length then Error (Malformed "the token is too large")
    else
      match String.index_opt token '.' with
      | None ->
          Error (Malformed "not three parts")
      | Some d1 -> (
        match String.index_from_opt token (d1 + 1) '.' with
        | Some d2 when not (String.contains_from token (d2 + 1) '.') -> (
            if d1 > max_header then Error (Malformed "the header is too large")
            else
              (* The header and the keys first: no other work for a token that
                 no key can verify. *)
              match b64_decode (String.sub token 0 d1) with
              | None ->
                  Error (Malformed "a part is not base64url")
              | Some header_text -> (
                  let* alg, kid = header header_text in
                  match candidates alg kid keys with
                  | [] ->
                      Error No_key
                  | keys -> (
                    match
                      b64_decode (String.sub token (d2 + 1) (n - d2 - 1))
                    with
                    | None ->
                        Error (Malformed "a part is not base64url")
                    | Some signature -> (
                        let input = String.sub token 0 d2 in
                        let digest =
                          lazy
                            Digestif.SHA256.(to_raw_string (digest_string input))
                        in
                        if
                          not
                            (List.exists
                               (fun k -> signs k input digest signature)
                               keys )
                        then Error Bad_signature
                        else
                          (* The payload only after the signature. *)
                          match
                            b64_decode (String.sub token (d1 + 1) (d2 - d1 - 1))
                          with
                          | None ->
                              Error (Malformed "a part is not base64url")
                          | Some payload ->
                              let* members = object_of "the payload" payload in
                              let* () =
                                check_claims ~leeway ~require_exp ~issuer
                                  ~audience members
                              in
                              Ok {payload; members} ) ) ) )
        | _ ->
            Error (Malformed "not three parts") )

  let find name c = List.assoc_opt name c.members

  let string name c =
    match find name c with Some (`String s) -> Some s | _ -> None

  let int name c = match find name c with Some (`Int i) -> Some i | _ -> None

  let number name c = Option.bind (find name c) number_of

  let bool name c =
    match find name c with Some (`Bool b) -> Some b | _ -> None

  let strings name c =
    match find name c with Some (`List l) -> string_list l | _ -> None

  let subject = string "sub"

  let issuer = string "iss"

  let audience c =
    match find "aud" c with
    | Some (`String a) ->
        [a]
    | _ ->
        Option.value ~default:[] (strings "aud" c)

  (* A time outside the range of [Weft.Clock.posix] gives [None]. *)
  let time_claim name c =
    Option.bind (number name c) (fun f ->
        match Weft.Time.posix_of_unix_s f with
        | t ->
            Some t
        | exception Invalid_argument _ ->
            None )

  let expires = time_claim "exp"

  let not_before = time_claim "nbf"

  let issued_at = time_claim "iat"

  let payload c = c.payload
end
