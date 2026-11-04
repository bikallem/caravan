module Base64 = Caravan.Private.Base64

let b64_decode = Base64.decode ~url:true ~pad:false

let b64_encode = Base64.encode ~url:true ~pad:false

let sha256 s = Digestif.SHA256.(to_raw_string (digest_string s))

(* The posix time in seconds: a session can outlive a process. *)
let now_s () = int_of_float (Weft.Time.posix_to_unix_s (Weft.Time.now Posix))

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

  (* [access_token]: the [typ] of the header is [at+jwt]. [Oidc] refuses an
     access token. *)
  type claims =
    {payload: string; members: (string * Yojson.Safe.t) list; access_token: bool}

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
  let typ_name t =
    let t = String.lowercase_ascii t in
    let prefix = "application/" in
    if String.starts_with ~prefix t then
      String.sub t (String.length prefix)
        (String.length t - String.length prefix)
    else t

  let good_typ t =
    let t = typ_name t in
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
    Ok (alg, kid, Option.map typ_name typ = Some "at+jwt")

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
                  let* alg, kid, access_token = header header_text in
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
                        let digest = lazy (sha256 input) in
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
                              Ok {payload; members; access_token} ) ) ) )
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

(* {1 Users} *)

let is_ctl c = Char.code c < 0x20 || c = '\x7f'

(* [/] and then not [/], with no [\] and no control character: a path of this
   site. A [\] at 1 is a [\] anywhere. *)
let is_local_path p =
  String.length p >= 1
  && p.[0] = '/'
  && (String.length p = 1 || p.[1] <> '/')
  && String.for_all (fun c -> c <> '\\' && not (is_ctl c)) p

module User = struct
  type t =
    { id: string
    ; scheme: string
    ; roles: string list
    ; claims: (string * string) list }

  let make ?(roles = []) ?(claims = []) id = {id; scheme= ""; roles; claims}

  let id u = u.id

  let scheme u = u.scheme

  let roles u = u.roles

  let has_role r u = List.mem r u.roles

  let claim name u = List.assoc_opt name u.claims

  let claims u = u.claims

  (* The text of a user in a session: version 1, then [len:bytes] fields: the
     id, the count of the roles (decimal), the roles, then the names and values
     of the claims. The scheme is not kept: the scheme that reads it sets it. *)
  let field s = string_of_int (String.length s) ^ ":" ^ s

  let encode u =
    String.concat ""
      ( "1" :: field u.id
        :: field (string_of_int (List.length u.roles))
        :: List.map field u.roles
      @ List.concat_map (fun (n, v) -> [field n; field v]) u.claims )

  (* [number s] is the value of the decimal digits [s], with no sign and no
     leading zero. *)
  let number s =
    let n = String.length s in
    if
      n = 0
      || (n > 1 && s.[0] = '0')
      || not (String.for_all (fun c -> c >= '0' && c <= '9') s)
    then None
    else int_of_string_opt s

  (* [fields s i] is the [len:bytes] fields of [s] from [i] to its end. *)
  let rec fields s i =
    if i = String.length s then Some []
    else
      match String.index_from_opt s i ':' with
      | None ->
          None
      | Some colon -> (
        match number (String.sub s i (colon - i)) with
        | Some l when l <= String.length s - colon - 1 ->
            Option.map
              (fun rest -> String.sub s (colon + 1) l :: rest)
              (fields s (colon + 1 + l))
        | _ ->
            None )

  let rec take n l =
    match (n, l) with
    | 0, l ->
        Some ([], l)
    | n, x :: l ->
        Option.map (fun (a, b) -> (x :: a, b)) (take (n - 1) l)
    | _, [] ->
        None

  let rec pairs = function
    | [] ->
        Some []
    | n :: v :: rest ->
        Option.map (fun r -> (n, v) :: r) (pairs rest)
    | [_] ->
        None

  let ( let* ) = Option.bind

  let decode s =
    let* () = if String.starts_with ~prefix:"1" s then Some () else None in
    let* l = fields s 1 in
    match l with
    | id :: count :: rest ->
        let* count = number count in
        let* roles, rest = take count rest in
        let* claims = pairs rest in
        Some {id; scheme= ""; roles; claims}
    | _ ->
        None
end

type outcome = Authenticated of User.t | No_credentials | Failed of string

(* {1 Schemes} *)

module Scheme = struct
  type t =
    { name: string
    ; authenticate: Caravan.Request.t -> outcome
    ; challenge:
        failure:string option -> Caravan.Request.t -> Caravan.Response.t }

  let name s = s.name

  let make ~name ~authenticate ~challenge =
    if name = "" || name = "Bearer" || name = "Session" then
      invalid_arg
        "Caravan_auth.Scheme.make: an empty name, or the name of a scheme of \
         this library" ;
    {name; authenticate; challenge}

  let is_token68 = function
    | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '.' | '_' | '~' | '+' | '/'
      ->
        true
    | _ ->
        false

  (* The failures of the form of the field: [invalid_request] (RFC 6750 §3.1).
     The challenge compares them physically, thus a text of the application with
     the same letters is not one of them. *)
  let two_fields = "two Authorization fields"

  let malformed = "a malformed Authorization field"

  (* [starts_ci v w] is [true] if [v] starts with [w] (lower case), with no
     case. *)
  let starts_ci v w =
    let n = String.length w in
    let rec at i =
      i = n || (Char.lowercase_ascii v.[i] = w.[i] && at (i + 1))
    in
    String.length v >= n && at 0

  (* [parse v] is the token of the field value [v] for the scheme Bearer. The
     scheme is the run of tchar at the start. *)
  let parse v =
    let n = String.length v in
    if
      (not (starts_ci v "bearer"))
      || (n > 6 && Caravan.Private.Token.is_tchar v.[6])
    then `Other
    else
      let rec skip p i = if i < n && p v.[i] then skip p (i + 1) else i in
      let t = skip (fun c -> c = ' ') 6 in
      let c = skip is_token68 t in
      if t = 6 || c = t || skip (fun c -> c = '=') c <> n then `Malformed
      else `Token (String.sub v t (n - t))

  let default_user roles_claim (claims : Jwt.claims) =
    match Jwt.subject claims with
    | None | Some "" ->
        Error "no sub"
    | Some id ->
        let roles = Option.value ~default:[] (Jwt.strings roles_claim claims) in
        let strings =
          List.filter_map
            (function n, `String v -> Some (n, v) | _ -> None)
            claims.Jwt.members
        in
        Ok (User.make ~roles ~claims:strings id)

  let bearer ?(realm = "api") ?leeway ?(roles_claim = "roles") ?user ~issuer
      ~audience ~keys () =
    let bad m = invalid_arg ("Caravan_auth.Scheme.bearer: " ^ m) in
    if String.exists (fun c -> c = '"' || c = '\\' || is_ctl c) realm then
      bad "the realm has a quote, a backslash or a control character" ;
    ( match (issuer, audience) with
    | Jwt.One_of [], _ | _, Jwt.One_of [] ->
        bad "an empty list for issuer or audience"
    | _ ->
        () ) ;
    ( match leeway with
    | Some l when Weft.Span.compare l Weft.Span.zero < 0 ->
        bad "a negative leeway"
    | _ ->
        () ) ;
    let user_of =
      match user with Some f -> f | None -> default_user roles_claim
    in
    let authenticate req =
      match
        Caravan.Headers.find_all "authorization" (Caravan.Request.headers req)
      with
      | [] ->
          No_credentials
      | _ :: _ :: _ ->
          Failed two_fields
      | [v] -> (
        match parse v with
        | `Other ->
            No_credentials
        | `Malformed ->
            Failed malformed
        | `Token token -> (
          match Jwt.verify ?leeway ~issuer ~audience ~keys:(keys ()) token with
          | Error e ->
              Failed (Format.asprintf "%a" Jwt.pp_error e)
          | Ok claims -> (
            match user_of claims with
            | Ok u ->
                Authenticated u
            | Error m ->
                Failed m ) ) )
    in
    let challenge ~failure _ =
      let error =
        match failure with
        | None ->
            ""
        | Some m when m == two_fields || m == malformed ->
            {|, error="invalid_request"|}
        | Some _ ->
            {|, error="invalid_token"|}
      in
      Caravan.Response.string ~status:Caravan.Status.unauthorized
        ~headers:
          (Caravan.Headers.of_list
             [("WWW-Authenticate", {|Bearer realm="|} ^ realm ^ {|"|} ^ error)] )
        "Unauthorized\n"
    in
    {name= "Bearer"; authenticate; challenge}

  let member = "caravan.auth.user"

  (* [contains_ci a w] is [true] if [a] contains [w] (lower case), with no case
     and no copy. *)
  let contains_ci a w =
    let n = String.length a and m = String.length w in
    let rec at i j =
      j = m || (Char.lowercase_ascii a.[i + j] = w.[j] && at i (j + 1))
    in
    let rec from i = i + m <= n && (at i 0 || from (i + 1)) in
    from 0

  (* A navigation of a browser: a GET or HEAD for a page. *)
  let navigation req =
    let h = Caravan.Request.headers req in
    ( match Caravan.Request.meth req with
      | Caravan.Method.GET | Caravan.Method.HEAD ->
          true
      | _ ->
          false )
    &&
    match Caravan.Headers.find "sec-fetch-mode" h with
    | Some m ->
        m = "navigate"
    | None -> (
      match Caravan.Headers.find "accept" h with
      | Some a ->
          contains_ci a "text/html"
      | None ->
          false )

  (* The bytes of a path in a query value: the unreserved characters and [/]
     stay. *)
  let encode_path p =
    let b = Buffer.create (String.length p) in
    String.iter
      (fun c ->
        match c with
        | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '.' | '_' | '~' | '/' ->
            Buffer.add_char b c
        | c ->
            Buffer.add_string b (Printf.sprintf "%%%02X" (Char.code c)) )
      p ;
    Buffer.contents b

  let session ?login ?(validate = Option.some) () =
    ( match login with
    | Some l when (not (is_local_path l)) || String.contains l '?' ->
        invalid_arg
          "Caravan_auth.Scheme.session: login is not a path that starts with \
           one /"
    | _ ->
        () ) ;
    let authenticate req =
      (* The session at the start of the request: a sign in or a sign out of the
         handler does not change the user of this request. *)
      match Caravan_session.Private.find_loaded member req with
      | None ->
          No_credentials
      | Some text -> (
        match Option.map validate (User.decode text) with
        | None ->
            Failed "bad session data"
        | Some None ->
            Failed "the user is not valid"
        | Some (Some u) ->
            Authenticated u )
    in
    let challenge ~failure:_ req =
      match login with
      | Some l when navigation req ->
          Caravan.Response.empty ~status:Caravan.Status.see_other
            ~headers:
              (Caravan.Headers.of_list
                 [ ( "Location"
                   , l ^ "?return=" ^ encode_path (Caravan.Request.path req) )
                 ] )
            ()
      | _ ->
          Caravan.Response.string ~status:Caravan.Status.unauthorized
            "Unauthorized\n"
    in
    {name= "Session"; authenticate; challenge}
end

let sign_in req user =
  ( match Caravan_session.Private.find Scheme.member req with
  | Some _ ->
      (* Another user: a new session, with no data of the old one. *)
      Caravan_session.clear req
  | None ->
      Caravan_session.renew req ) ;
  Caravan_session.Private.set Scheme.member (User.encode user) req

let sign_out req = Caravan_session.clear req

(* {1 The middleware} *)

(* The result of the schemes: the user, or the scheme that failed. *)
type result = {user: User.t option; failure: (Scheme.t * string) option}

(* The schemes run at the first need of their result, not for each request: a
   public route pays nothing. Two fibers of one request can compute it at the
   same time: the first result stays. *)
type context =
  {schemes: Scheme.t list; run: unit -> result; result: result option Atomic.t}

let context_key : context Caravan.Attrs.key =
  Caravan.Attrs.key ~name:"caravan.auth" ()

let middleware ~schemes () =
  let names = List.map Scheme.name schemes in
  ( match schemes with
  | [] ->
      invalid_arg "Caravan_auth.middleware: no scheme"
  | _ ->
      () ) ;
  if List.length (List.sort_uniq String.compare names) <> List.length names then
    invalid_arg "Caravan_auth.middleware: two schemes with one name" ;
  fun next req ->
    let run () =
      let rec go = function
        | [] ->
            {user= None; failure= None}
        | (s : Scheme.t) :: rest -> (
          match s.authenticate req with
          | Authenticated u ->
              {user= Some {u with User.scheme= s.name}; failure= None}
          | No_credentials ->
              go rest
          | Failed m ->
              {user= None; failure= Some (s, m)} )
      in
      go schemes
    in
    next
      (Caravan.Request.with_attr context_key
         {schemes; run; result= Atomic.make None}
         req )

let context fn req =
  match Caravan.Request.find_attr context_key req with
  | Some c ->
      c
  | None ->
      invalid_arg
        ( "Caravan_auth." ^ fn
        ^ ": the request did not go through Caravan_auth.middleware" )

let result c =
  match Atomic.get c.result with
  | Some r ->
      r
  | None ->
      (* Another fiber can set it first: its result stays. *)
      ignore (Atomic.compare_and_set c.result None (Some (c.run ()))) ;
      Option.get (Atomic.get c.result)

let user req = (result (context "user" req)).user

let failure req =
  Option.map
    (fun ((s : Scheme.t), m) -> (s.name, m))
    (result (context "failure" req)).failure

(* [challenge_of ?scheme c req] is the challenge of the scheme that failed, else
   of [scheme], else of the first scheme. *)
let challenge_of ?scheme c req =
  match ((result c).failure, scheme) with
  | Some ((s : Scheme.t), m), _ ->
      s.challenge ~failure:(Some m) req
  | None, Some (s : Scheme.t) ->
      s.challenge ~failure:None req
  | None, None ->
      (List.hd c.schemes).challenge ~failure:None req

let challenge req = challenge_of (context "challenge" req) req

let forbid _ =
  Caravan.Response.string ~status:Caravan.Status.forbidden "Forbidden\n"

let return_path req =
  match Caravan.Form.find "return" (Caravan.Request.query_params req) with
  | Some p when is_local_path p ->
      Some p
  | _ ->
      None

(* {1 Authorization} *)

type policy = User.t -> bool

let authenticated _ = true

let role = User.has_role

let scope s u =
  match User.claim "scope" u with
  | Some v ->
      List.mem s (String.split_on_char ' ' v)
  | None ->
      false

let all ps u = List.for_all (fun p -> p u) ps

let any ps u = List.exists (fun p -> p u) ps

let require ?(policy = authenticated) ?challenge () next req =
  let c = context "require" req in
  let scheme =
    Option.map
      (fun name ->
        match List.find_opt (fun (s : Scheme.t) -> s.name = name) c.schemes with
        | Some s ->
            s
        | None ->
            invalid_arg
              ("Caravan_auth.require: no scheme " ^ name ^ " in the middleware") )
      challenge
  in
  match (result c).user with
  | None ->
      challenge_of ?scheme c req
  | Some u ->
      if policy u then
        let r = next req in
        let h = Caravan.Response.headers r in
        if Caravan.Headers.mem "cache-control" h then r
        else
          Caravan.Response.with_headers
            (Caravan.Headers.add "Cache-Control" "private" h)
            r
      else forbid req

(* {1 OpenID Connect} *)

module Oidc = struct
  open Weft
  module Response = Caravan.Response

  let ( let* ) = Result.bind

  let minute = Span.s 60

  (* [parse u] is the scheme (in lowercase) and the authority of a URL that the
     client accepts: [http] or [https], a host, a port of 1 to 65535 and no user
     information. Also no fragment, and no space or control character. *)
  let parse u =
    if String.for_all (fun c -> c > ' ' && c < '\x7f' && c <> '#') u then
      match Caravan_client.Private.parse_url u with
      | scheme, authority, _ ->
          Some (scheme, authority)
      | exception (Invalid_argument _ | Caravan_client.Error _) ->
          None
    else None

  let is_https u = match parse u with Some ("https", _) -> true | _ -> false

  (* [json_string name members] is the string member [name], if any. *)
  let json_string name members =
    match List.assoc_opt name members with
    | Some (`String s) ->
        Some s
    | _ ->
        None

  (* An issuer: an https URL with no query (OIDC Discovery §3). *)
  let good_issuer u = is_https u && not (String.contains u '?')

  (* The state of the key cache. [fresh_until = None]: a load at the next need.
     It is [None] before the first load, and also for an overflow of [Time.add],
     which real times of the mono clock cannot give. *)
  type state =
    { keys: Jwt.Key.t list
    ; fresh_until: Clock.mono option
    ; last_load: Clock.mono option
    ; last_error: string option }

  let no_state = {keys= []; fresh_until= None; last_load= None; last_error= None}

  (* The size limit of a document of the provider. *)
  let max_document = 65536

  (* [fetch ~timeout ~max http ?headers ?body meth url] is the response, its
     status and its body, with one deadline for all of it. *)
  let fetch ~timeout ~max http ?(headers = []) ?body meth url =
    match
      Fiber.with_timeout Mono timeout (fun () ->
          let scheme, authority, path = Caravan_client.Private.parse_url url in
          let resp =
            Caravan_client.request http
              (Caravan.Request.make ~scheme ~authority
                 ~headers:
                   (Caravan.Headers.of_list
                      (("Accept", "application/json") :: headers) )
                 ?body meth path )
          in
          let b = Caravan_client.body resp in
          Fun.protect
            ~finally:(fun () -> Caravan.Body.close b)
            (fun () ->
              let code = Caravan.Status.code (Response.status resp) in
              Ok (resp, code, Caravan.Body.to_string ~max b) ) )
    with
    | r ->
        r
    | exception Fiber.Timeout ->
        Error (url ^ ": timeout")
    | exception (Caravan_client.Error _ as e) ->
        Error (url ^ ": " ^ Printexc.to_string e)
    | exception Caravan.Body.Too_large ->
        Error (url ^ ": the body is too large")

  (* [get_text ~timeout ~max http url] is the response and the body of a GET of
     [url] with the status 200. *)
  let get_text ~timeout ~max http url =
    match fetch ~timeout ~max http Caravan.Method.GET url with
    | Ok (resp, 200, text) ->
        Ok (resp, text)
    | Ok (_, code, _) ->
        Error (Printf.sprintf "%s: status %d" url code)
    | Error e ->
        Error e

  (* The time that a JWK Set stays fresh: the [max-age] of [Cache-Control], 5
     min to 24 h. With no [max-age], or with [no-cache] or [no-store]: 1 h. *)
  let fresh_for resp =
    let default = Span.s 3600 in
    match Caravan.Headers.find "cache-control" (Response.headers resp) with
    | None ->
        default
    | Some v -> (
        let ds =
          List.map
            (fun d -> String.lowercase_ascii (String.trim d))
            (String.split_on_char ',' v)
        in
        let prefix = "max-age=" in
        let p = String.length prefix in
        (* The value saturates at 24 h: no overflow. *)
        let seconds n =
          String.fold_left
            (fun a c -> Int.min 86400 ((a * 10) + Char.code c - 48))
            0 n
        in
        let age d =
          if String.starts_with ~prefix d then
            let n = String.sub d p (String.length d - p) in
            if n <> "" && String.for_all (fun c -> c >= '0' && c <= '9') n then
              Some (seconds n)
            else None
          else None
        in
        if List.mem "no-cache" ds || List.mem "no-store" ds then default
        else
          match List.find_map age ds with
          | Some n ->
              Span.s (Int.max 300 n)
          | None ->
              default )

  module Provider = struct
    type t =
      { issuer: string
      ; authorization_endpoint: string
      ; token_endpoint: string
      ; jwks_uri: string
      ; auth_methods: string list option
      ; iss_parameter: bool
      ; http: Caravan_client.t
      ; timeout: Span.t
      ; state: state Atomic.t
      ; loading: state Ivar.t option Atomic.t }

    let bad fn m = invalid_arg (Printf.sprintf "Caravan_auth.Oidc.%s: %s" fn m)

    let check_timeout fn timeout =
      if Span.compare timeout Span.zero <= 0 then
        bad fn "the timeout is not positive"

    let make_unchecked ~timeout ~auth_methods ~iss_parameter ~issuer
        ~authorization_endpoint ~token_endpoint ~jwks_uri http =
      { issuer
      ; authorization_endpoint
      ; token_endpoint
      ; jwks_uri
      ; auth_methods
      ; iss_parameter
      ; http
      ; timeout
      ; state= Atomic.make no_state
      ; loading= Atomic.make None }

    let make ?(timeout = Span.s 10) ?auth_methods ?(iss_parameter = false)
        ~issuer ~authorization_endpoint ~token_endpoint ~jwks_uri http =
      check_timeout "Provider.make" timeout ;
      if not (good_issuer issuer) then
        bad "Provider.make" "the issuer is not an https URL with no query" ;
      List.iter
        (fun (name, u) ->
          if not (is_https u) then
            bad "Provider.make" (name ^ " is not an https URL") )
        [ ("authorization_endpoint", authorization_endpoint)
        ; ("token_endpoint", token_endpoint)
        ; ("jwks_uri", jwks_uri) ] ;
      make_unchecked ~timeout ~auth_methods ~iss_parameter ~issuer
        ~authorization_endpoint ~token_endpoint ~jwks_uri http

    (* The members of a discovery document (OIDC Discovery §3). *)
    let of_document ~timeout ~issuer http m =
      let str name =
        match List.assoc_opt name m with
        | Some (`String s) ->
            Ok s
        | Some _ ->
            Error (name ^ " is not a string")
        | None ->
            Error ("no " ^ name)
      in
      let url name =
        let* u = str name in
        if is_https u then Ok u else Error (name ^ " is not an https URL")
      in
      let strs name =
        match List.assoc_opt name m with
        | None ->
            Ok None
        | Some (`List l) -> (
          match string_list l with
          | Some l ->
              Ok (Some l)
          | None ->
              Error (name ^ " is not a list of strings") )
        | Some _ ->
            Error (name ^ " is not a list of strings")
      in
      let* iss = str "issuer" in
      (* §4.3: the same text, byte for byte. *)
      let* () =
        if iss = issuer then Ok ()
        else Error ("the issuer of the document is " ^ iss)
      in
      let* authorization_endpoint = url "authorization_endpoint" in
      let* token_endpoint = url "token_endpoint" in
      let* jwks_uri = url "jwks_uri" in
      let* types = strs "response_types_supported" in
      let* () =
        match types with
        | Some l when List.mem "code" l ->
            Ok ()
        | _ ->
            Error "the provider does not give the response type code"
      in
      let* methods = strs "code_challenge_methods_supported" in
      let* () =
        match methods with
        | Some l when not (List.mem "S256" l) ->
            Error "the provider does not give the PKCE method S256"
        | _ ->
            Ok ()
      in
      let* auth_methods = strs "token_endpoint_auth_methods_supported" in
      let* iss_parameter =
        match
          List.assoc_opt "authorization_response_iss_parameter_supported" m
        with
        | None ->
            Ok false
        | Some (`Bool b) ->
            Ok b
        | Some _ ->
            Error
              "authorization_response_iss_parameter_supported is not a boolean"
      in
      Ok
        (make_unchecked ~timeout ~auth_methods ~iss_parameter ~issuer
           ~authorization_endpoint ~token_endpoint ~jwks_uri http )

    let discover ?(timeout = Span.s 10) ?(max_length = max_document) http issuer
        =
      check_timeout "Provider.discover" timeout ;
      if max_length < 1 then bad "Provider.discover" "max_length is less than 1" ;
      if not (good_issuer issuer) then
        Error "the issuer is not an https URL with no query"
      else
        (* §4.1: one terminating "/" goes before the path. *)
        let base =
          if String.ends_with ~suffix:"/" issuer then
            String.sub issuer 0 (String.length issuer - 1)
          else issuer
        in
        match
          get_text ~timeout ~max:max_length http
            (base ^ "/.well-known/openid-configuration")
        with
        | Error e ->
            Error e
        | Ok (_, text) -> (
          match Json.parse text with
          | Error e ->
              Error ("the document: " ^ e)
          | Ok (`Assoc m) ->
              of_document ~timeout ~issuer http m
          | Ok _ ->
              Error "the document is not an object" )

    let issuer t = t.issuer

    let authorization_endpoint t = t.authorization_endpoint

    let token_endpoint t = t.token_endpoint

    let auth_methods t = t.auth_methods

    let iss_parameter t = t.iss_parameter

    (* [load t] is the state after a load of the JWK Set. A failure keeps the
       keys, and the next load comes after 1 min. *)
    let load t =
      let r =
        match
          get_text ~timeout:t.timeout ~max:max_document t.http t.jwks_uri
        with
        | Error e ->
            Error e
        | Ok (resp, text) -> (
          match Jwt.Key.of_jwks text with
          | Error e ->
              Error ("the JWK Set: " ^ e)
          | Ok [] ->
              Error "the JWK Set has no key that caravan can use"
          | Ok keys ->
              Ok (keys, fresh_for resp) )
        | exception e ->
            (* [get_text] catches the errors of the network: this is for an
               other exception of the client or of TLS. *)
            Error (t.jwks_uri ^ ": " ^ Printexc.to_string e)
      in
      let now = Time.now Mono in
      match r with
      | Ok (keys, span) ->
          { keys
          ; fresh_until= Time.add Mono now span
          ; last_load= Some now
          ; last_error= None }
      | Error e ->
          (* The next load after 1 min, but keys that are fresh for longer stay
             fresh. *)
          let old = Atomic.get t.state in
          let retry = Time.add Mono now minute in
          let fresh_until =
            match (old.fresh_until, retry) with
            | Some u, Some r
              when ( Span.compare (Time.diff Mono u r) Span.zero
                   > 0 )
                   [@mutate off "u = r: the two branches give the same time"] ->
                Some u
            | _ ->
                retry
          in
          {old with fresh_until; last_load= Some now; last_error= Some e}

    (* [refresh t ~seen] loads the keys, one load at a time: a caller during a
       load waits for its result. [seen] is the state that made the caller load:
       if a load of an other caller ended after it, that state is the result,
       with no new load. The load is in a shield: a caller that leaves does not
       stop it for the others. The shield also sets the state and fills the
       Ivar, thus a cancel of the loader cannot leave the waiters with no
       result. *)
    let rec refresh t ~seen =
      match Atomic.get t.loading with
      | Some ivar ->
          sync (Ivar.read ivar)
      | None ->
          let ivar = Ivar.create () in
          if not (Atomic.compare_and_set t.loading None (Some ivar)) then
            refresh t ~seen
          else
            Fiber.shield (fun () ->
                let now = Atomic.get t.state in
                let st = if now != seen then now else load t in
                Atomic.set t.state st ;
                Atomic.set t.loading None ;
                Ivar.fill ivar st ;
                st )

    let is_fresh st now =
      match st.fresh_until with
      | Some u ->
          Span.compare (Time.diff Mono u now) Span.zero > 0
      | None ->
          false

    let current t =
      let st = Atomic.get t.state in
      if is_fresh st (Time.now Mono) then st else refresh t ~seen:st

    (* A new key after a rotation: one load each minute at most. *)
    let can_reload st =
      match st.last_load with
      | Some l ->
          Span.compare (Time.diff Mono (Time.now Mono) l) minute >= 0
      | None ->
          true

    let keys t = (current t).keys
  end

  type error =
    | Jwt of Jwt.error
    | Missing_nonce
    | Bad_nonce
    | Bad_azp
    | Keys of string

  let pp_error ppf = function
    | Jwt e ->
        Format.fprintf ppf "Jwt (%a)" Jwt.pp_error e
    | Missing_nonce ->
        Format.pp_print_string ppf "Missing_nonce"
    | Bad_nonce ->
        Format.pp_print_string ppf "Bad_nonce"
    | Bad_azp ->
        Format.pp_print_string ppf "Bad_azp"
    | Keys m ->
        Format.fprintf ppf "Keys %S" m

  (* OIDC Core §2 and §3.1.3.7, after [Jwt.verify]. *)
  let check_id_token ?nonce ~client_id (c : Jwt.claims) =
    let* () =
      (* An access token is not an ID token (RFC 8725 §3.11). *)
      if c.access_token then Error (Jwt (Unsupported "typ at+jwt")) else Ok ()
    in
    let* () =
      match Jwt.subject c with
      | None | Some "" ->
          Error (Jwt (Missing "sub"))
      | Some _ ->
          Ok ()
    in
    let* () =
      match Jwt.string "azp" c with
      | Some a when a <> client_id ->
          Error Bad_azp
      | None when List.length (Jwt.audience c) > 1 ->
          Error Bad_azp
      | _ ->
          Ok ()
    in
    match nonce with
    | None ->
        Ok c
    | Some n -> (
      match Jwt.string "nonce" c with
      | None ->
          Error Missing_nonce
      | Some m ->
          if Eqaf.equal n m then Ok c else Error Bad_nonce )

  let verify_id_token ?leeway ?nonce ?issuers (p : Provider.t) ~client_id token
      =
    let issuers = Option.value issuers ~default:[p.issuer] in
    let verify (st : state) =
      Jwt.verify ?leeway ~issuer:(One_of issuers) ~audience:(One_of [client_id])
        ~keys:st.keys token
    in
    let st = Provider.current p in
    let st, r =
      match verify st with
      | Error No_key when Provider.can_reload st ->
          let st = Provider.refresh p ~seen:st in
          (st, verify st)
      | r ->
          (st, r)
    in
    match r with
    | Error No_key when st.keys = [] ->
        Error (Keys (Option.value st.last_error ~default:"no key"))
    | Error e ->
        Error (Jwt e)
    | Ok c ->
        check_id_token ?nonce ~client_id c
  (* {2 The login and the callback} *)

  type auth = No_secret | Basic of string | Post of string

  type client =
    { provider: Provider.t
    ; id: string
    ; auth: auth
    ; redirect_uri: string
    ; scope: string
    ; leeway: Span.t option
    ; login_timeout: int  (** Seconds. *)
    ; extra: (string * string) list }

  (* The parameters that [login] sets, or that change the checks. *)
  let reserved =
    [ "response_type"
    ; "client_id"
    ; "redirect_uri"
    ; "scope"
    ; "state"
    ; "nonce"
    ; "code_challenge"
    ; "code_challenge_method"
    ; "max_age"
    ; "request"
    ; "request_uri"
    ; "response_mode" ]

  (* An https URL, or an http URL of a loopback host (RFC 8252 §7.3). *)
  let good_redirect u =
    match parse u with
    | Some ("https", _) ->
        true
    | Some ("http", authority) ->
        let a = String.lowercase_ascii authority in
        List.exists
          (fun h -> a = h || String.starts_with ~prefix:(h ^ ":") a)
          ["localhost"; "127.0.0.1"; "[::1]"]
    | _ ->
        false

  (* The default scope is openid, email and profile: [client] puts openid
     first. *)
  let client ?(scope = ["email"; "profile"]) ?secret ?auth ?leeway
      ?(login_timeout = Span.s 600) ?(extra = []) ~provider ~id ~redirect_uri ()
      =
    let bad m = invalid_arg ("Caravan_auth.Oidc.client: " ^ m) in
    if id = "" then bad "the client ID is empty" ;
    if not (good_redirect redirect_uri) then
      bad "the redirect URI is not https, or http of a loopback host" ;
    if Span.compare login_timeout (Span.s 1) < 0 then
      bad "the login timeout is less than 1 s" ;
    Option.iter
      (fun l -> if Span.compare l Span.zero < 0 then bad "a negative leeway")
      leeway ;
    List.iter
      (fun (n, _) ->
        if List.mem n reserved then bad ("the parameter " ^ n ^ " is reserved") )
      extra ;
    List.iter
      (fun s ->
        if s = "" || not (String.for_all (fun c -> c > ' ' && c < '\x7f') s)
        then bad "a scope is empty, or has a space or a control character" )
      scope ;
    let scope =
      String.concat " "
        (if List.mem "openid" scope then scope else "openid" :: scope)
    in
    let auth =
      match (secret, auth) with
      | None, _ ->
          No_secret
      | Some s, Some `Basic ->
          Basic s
      | Some s, Some `Post ->
          Post s
      | Some s, None -> (
        (* RFC 8414 §2: client_secret_basic if the provider does not tell. *)
        match Provider.auth_methods provider with
        | None ->
            Basic s
        | Some l when List.mem "client_secret_basic" l ->
            Basic s
        | Some l when List.mem "client_secret_post" l ->
            Post s
        | Some _ ->
            bad
              "the provider gives neither client_secret_basic nor \
               client_secret_post" )
    in
    { provider
    ; id
    ; auth
    ; redirect_uri
    ; scope
    ; leeway
    ; login_timeout=
        Int64.to_int (Int64.div (Span.to_ns login_timeout) 1_000_000_000L)
    ; extra }

  (* A login that waits for its callback. *)
  type pending =
    { issuer: string
    ; client_id: string
    ; expires: int  (** Posix seconds: the login timeout of its client. *)
    ; state: string
    ; nonce: string
    ; verifier: string
    ; return: string }

  (* The pending logins of a session, the newest first, in one value: the fields
     of a login with '|' between them, and ',' between the logins. Each field is
     base64url or digits: no '|' or ','. *)
  let key = "caravan.oidc"

  let encode_pending p =
    String.concat "|"
      [ b64_encode p.issuer
      ; b64_encode p.client_id
      ; string_of_int p.expires
      ; p.state
      ; p.nonce
      ; p.verifier
      ; b64_encode p.return ]

  let decode_pending s =
    match String.split_on_char '|' s with
    | [i; c; t; state; nonce; verifier; r] -> (
      match (b64_decode i, b64_decode c, int_of_string_opt t, b64_decode r) with
      | Some issuer, Some client_id, Some expires, Some return ->
          Some {issuer; client_id; expires; state; nonce; verifier; return}
      | _ ->
          None )
    | _ ->
        None

  (* The pending logins of the session that did not expire. *)
  let pendings req ~now =
    match Caravan_session.Private.find key req with
    | Some v ->
        List.filter
          (fun p -> now < p.expires)
          (List.filter_map decode_pending (String.split_on_char ',' v))
    | None ->
        []

  let save req = function
    | [] ->
        Caravan_session.Private.remove key req
    | l ->
        Caravan_session.Private.set key
          (String.concat "," (List.map encode_pending l))
          req

  (* The most pending logins of a session, and the longest return path. *)
  let max_pending = 4

  let max_return = 1024

  let random () = b64_encode (Mirage_crypto_rng.generate 32)

  let no_cache =
    [("Cache-Control", "no-store"); ("Referrer-Policy", "no-referrer")]

  let login ?return c req =
    let return =
      match return with Some r -> Some r | None -> return_path req
    in
    let return =
      match return with
      | Some r when is_local_path r && String.length r <= max_return ->
          r
      | _ ->
          "/"
    in
    let now = now_s () in
    let p =
      { issuer= Provider.issuer c.provider
      ; client_id= c.id
      ; expires= now + c.login_timeout
      ; state= random ()
      ; nonce= random ()
      ; verifier= random ()
      ; return }
    in
    save req (List.take max_pending (p :: pendings req ~now)) ;
    let challenge = b64_encode (sha256 p.verifier) in
    let params =
      [ ("response_type", "code")
      ; ("client_id", c.id)
      ; ("redirect_uri", c.redirect_uri)
      ; ("scope", c.scope)
      ; ("state", p.state)
      ; ("nonce", p.nonce)
      ; ("code_challenge", challenge)
      ; ("code_challenge_method", "S256") ]
      @ c.extra
    in
    let endpoint = Provider.authorization_endpoint c.provider in
    let sep = if String.contains endpoint '?' then "&" else "?" in
    Response.empty ~status:Caravan.Status.see_other
      ~headers:
        (Caravan.Headers.of_list
           ( ("Location", endpoint ^ sep ^ Caravan.Form.encode params)
           :: no_cache ) )
      ()

  type tokens =
    { id_token: string
    ; access_token: string option
    ; expires_in: int option
    ; subject: string
    ; claims: Jwt.claims }

  type failure =
    | No_login
    | Expired
    | Denied of string * string option
    | Bad_response of string
    | Token of string
    | Id_token of error

  let pp_failure ppf = function
    | No_login ->
        Format.pp_print_string ppf "No_login"
    | Expired ->
        Format.pp_print_string ppf "Expired"
    | Denied (e, d) ->
        Format.fprintf ppf "Denied (%S, %a)" e
          (Format.pp_print_option
             ~none:(fun ppf () -> Format.pp_print_string ppf "None")
             (fun ppf -> Format.fprintf ppf "Some %S") )
          d
    | Bad_response m ->
        Format.fprintf ppf "Bad_response %S" m
    | Token m ->
        Format.fprintf ppf "Token %S" m
    | Id_token e ->
        Format.fprintf ppf "Id_token (%a)" pp_error e

  (* [form_name s] is [s] encoded as a name of a form: the text of the pair
     ([s], "") with no "=" at its end. *)
  let form_name s =
    let e = Caravan.Form.encode [(s, "")] in
    String.sub e 0 (String.length e - 1)

  (* The JSON of a token response (RFC 6749 §5.1 and §5.2). *)
  let token_response status text =
    match (status, Json.parse text) with
    | 200, Ok (`Assoc m) -> (
      match (json_string "id_token" m, json_string "token_type" m) with
      | None, _ ->
          Error (Bad_response "the token response has no id_token")
      | Some id_token, Some t when String.lowercase_ascii t = "bearer" ->
          let expires_in =
            match List.assoc_opt "expires_in" m with
            | Some (`Int n) ->
                Some n
            | _ ->
                None
          in
          Ok (id_token, json_string "access_token" m, expires_in)
      | Some _, _ ->
          Error (Bad_response "the token type is not Bearer") )
    | 200, _ ->
        Error (Bad_response "the token response is not a JSON object")
    | status, json ->
        let code =
          match json with
          | Ok (`Assoc m) ->
              Option.fold ~none:"" ~some:(( ^ ) ": ") (json_string "error" m)
          | _ ->
              ""
        in
        Error (Token (Printf.sprintf "status %d%s" status code))

  (* The token request of the code flow (RFC 6749 §4.1.3, RFC 7636 §4.5). *)
  let token_request c ~code ~verifier =
    let base =
      [ ("grant_type", "authorization_code")
      ; ("code", code)
      ; ("redirect_uri", c.redirect_uri)
      ; ("code_verifier", verifier) ]
    in
    let form, headers =
      match c.auth with
      | No_secret ->
          (base @ [("client_id", c.id)], [])
      | Post s ->
          (base @ [("client_id", c.id); ("client_secret", s)], [])
      | Basic s ->
          (* RFC 6749 §2.3.1: the two parts are form-urlencoded first. *)
          let pair = form_name c.id ^ ":" ^ form_name s in
          let url =
            (false
            [@mutate
              off
                "a form-encoded text has no > ? ~ or DEL: its base64 has no + \
                 or /"] )
          in
          ( base
          , [("Authorization", "Basic " ^ Base64.encode ~url ~pad:true pair)] )
    in
    let p = c.provider in
    match
      fetch ~timeout:p.timeout ~max:max_document p.http
        ~headers:
          (("Content-Type", "application/x-www-form-urlencoded") :: headers)
        ~body:(Caravan.Body.of_string (Caravan.Form.encode form))
        Caravan.Method.POST p.token_endpoint
    with
    | Error e ->
        Error (Token e)
    | Ok (_, status, text) ->
        token_response status text

  let default_failed _ _ =
    Response.string ~status:Caravan.Status.bad_request
      ~headers:(Caravan.Headers.of_list no_cache)
      "Sign-in failed\n"

  (* [Referrer-Policy: no-referrer] on a response of the callback: its URL has
     the code. *)
  let no_referrer resp =
    let h = Response.headers resp in
    if Caravan.Headers.mem "referrer-policy" h then resp
    else
      Response.with_headers
        (Caravan.Headers.add "Referrer-Policy" "no-referrer" h)
        resp

  (* [take c req q] is the pending login of the [state] of [q], for [c]. It goes
     from the session at once (one use), with the expired logins. The state
     comes first: no other parameter is read for a request with no login. *)
  let take c req q =
    match Caravan.Form.find "state" q with
    | None ->
        Error No_login
    | Some state -> (
        let issuer = Provider.issuer c.provider in
        let mine p =
          p.issuer = issuer && p.client_id = c.id && Eqaf.equal p.state state
        in
        let now = now_s () in
        (* Read all the logins, also the expired ones: an expired login of the
           state gives [Expired]. *)
        let all =
          match Caravan_session.Private.find key req with
          | Some v ->
              List.filter_map decode_pending (String.split_on_char ',' v)
          | None ->
              []
        in
        match List.partition mine all with
        | [], _ ->
            Error No_login
        | p :: _, rest ->
            save req (List.filter (fun q -> now < q.expires) rest) ;
            if now < p.expires then Ok p else Error Expired )

  (* The checks of a callback, after the login: RFC 9207 §2.4 (iss), the error
     of the provider, the code, the token request, the ID token. *)
  let complete c q p =
    let issuer = Provider.issuer c.provider in
    let* () =
      match Caravan.Form.find "iss" q with
      | Some i when i <> issuer ->
          Error (Bad_response "the iss parameter is not the issuer")
      | None when Provider.iss_parameter c.provider ->
          Error (Bad_response "no iss parameter")
      | _ ->
          Ok ()
    in
    let* () =
      match Caravan.Form.find "error" q with
      | Some e ->
          Error (Denied (e, Caravan.Form.find "error_description" q))
      | None ->
          Ok ()
    in
    let* code =
      match Caravan.Form.find "code" q with
      | None | Some "" ->
          Error (Bad_response "no code")
      | Some code ->
          Ok code
    in
    let* id_token, access_token, expires_in =
      token_request c ~code ~verifier:p.verifier
    in
    let* claims =
      Result.map_error
        (fun e -> Id_token e)
        (verify_id_token ?leeway:c.leeway ~nonce:p.nonce c.provider
           ~client_id:c.id id_token )
    in
    (* [verify_id_token] refuses a token with no [sub]. *)
    let subject = Option.value ~default:"" (Jwt.subject claims) in
    Ok {id_token; access_token; expires_in; subject; claims}

  let callback ?(failed = default_failed) c f req =
    if not (Caravan.Method.equal (Caravan.Request.meth req) Caravan.Method.GET)
    then
      Response.string ~status:Caravan.Status.method_not_allowed
        ~headers:(Caravan.Headers.of_list [("Allow", "GET")])
        ""
    else
      let q = Caravan.Request.query_params req in
      match
        let* p = take c req q in
        Result.map (fun t -> (p, t)) (complete c q p)
      with
      | Ok (p, tokens) ->
          no_referrer (f req tokens ~return:p.return)
      | Error e ->
          no_referrer (failed req e)
end

(* {1 Codes by email} *)

module Code = struct
  module Limiter = Caravan_middleware.Rate_limit.Limiter
  open Weft

  type t =
    { key: string  (** The name of the value in the session. *)
    ; digits: int
    ; ttl: int  (** Seconds. *)
    ; attempts: int
    ; sends: string -> Span.t option
    ; checks: string -> Span.t option }

  let b64 = b64_encode

  let seconds span = Int64.to_int (Int64.div (Span.to_ns span) 1_000_000_000L)

  let make ?(purpose = "sign-in") ?(digits = 8) ?(ttl = Span.s 600)
      ?(attempts = 5) ?sends ?checks () =
    let bad m = invalid_arg ("Caravan_auth.Code.make: " ^ m) in
    let good c =
      (c >= 'a' && c <= 'z')
      || (c >= 'A' && c <= 'Z')
      || (c >= '0' && c <= '9')
      || c = '-'
    in
    if purpose = "" || not (String.for_all good purpose) then
      bad "the purpose is not letters, digits and '-'" ;
    if digits < 6 || digits > 9 then bad "digits is not 6 to 9" ;
    if Span.compare ttl (Span.s 60) < 0 || Span.compare ttl (Span.s 3600) > 0
    then bad "ttl is not 1 min to 1 h" ;
    if attempts < 1 || attempts > 10 then bad "attempts is not 1 to 10" ;
    let sends =
      match sends with
      | Some f ->
          f
      | None ->
          Limiter.take (Limiter.make ~burst:3 ~every:(Span.s 600) ())
    in
    let checks =
      match checks with
      | Some f ->
          f
      | None ->
          Limiter.take (Limiter.make ~burst:10 ~every:(Span.s 3600) ())
    in
    { key= "caravan.code." ^ purpose
    ; digits
    ; ttl= seconds ttl
    ; attempts
    ; sends
    ; checks }

  (* The key of an address in a limiter: [prefix] and a hash of the address in
     lowercase, with no spaces at its ends. Thus the variants of the case of an
     address share their buckets, and a key is short. *)
  let limit_key prefix address =
    prefix ^ b64 (sha256 (String.lowercase_ascii (String.trim address)))

  (* A code of [digits] decimal digits, uniform: 30 random bits, and a new draw
     over the largest multiple of 10^digits (no modulo bias). *)
  let random_code digits =
    let n = int_of_float (10. ** float_of_int digits) in
    let limit = (1 lsl 30) / n * n in
    let rec draw () =
      let v =
        Int32.to_int
          (String.get_int32_be
             (Mirage_crypto_rng.generate
                (4 [@mutate off "get_int32_be reads 4 bytes"]) )
             0 )
        land 0x3FFF_FFFF
      in
      if
        ( v < limit )
        [@mutate
          off
            "<= changes only the value v = limit: a probability of 2^-30 for \
             each draw"]
      then Printf.sprintf "%0*d" digits (v mod n)
      else draw ()
    in
    draw ()

  (* The code of a session. *)
  type entry =
    { address: string
    ; expires: int  (** Posix seconds. *)
    ; left: int  (** The attempts left. *)
    ; salt: string
    ; hash: string  (** SHA-256 of the salt and the code. *) }

  (* The fields with '|' between them: each one is base64url or digits. *)
  let encode e =
    String.concat "|"
      [ b64 e.address
      ; string_of_int e.expires
      ; string_of_int e.left
      ; b64 e.salt
      ; b64 e.hash ]

  let decode s =
    match String.split_on_char '|' s with
    | [a; x; l; salt; hash] -> (
      match
        ( b64_decode a
        , int_of_string_opt x
        , int_of_string_opt l
        , b64_decode salt
        , b64_decode hash )
      with
      | Some address, Some expires, Some left, Some salt, Some hash ->
          Some {address; expires; left; salt; hash}
      | _ ->
          None )
    | _ ->
        None

  let start t req ~address =
    if address = "" || String.length address > 320 then
      invalid_arg "Caravan_auth.Code.start: the address is empty or too long" ;
    match t.sends (limit_key "send:" address) with
    | Some wait ->
        Error wait
    | None ->
        let code = random_code t.digits in
        let salt =
          Mirage_crypto_rng.generate
            (16 [@mutate off "each length of a salt works the same"])
        in
        Caravan_session.Private.set t.key
          (encode
             { address
             ; expires= now_s () + t.ttl
             ; left= t.attempts
             ; salt
             ; hash= sha256 (salt ^ code) } )
          req ;
        Ok code

  type failure =
    | No_code
    | Expired
    | Wrong of {address: string; left: int}
    | Too_many of {address: string; wait: Span.t}

  let pp_failure ppf = function
    | No_code ->
        Format.pp_print_string ppf "No_code"
    | Expired ->
        Format.pp_print_string ppf "Expired"
    | Wrong {address; left} ->
        Format.fprintf ppf "Wrong {address= %S; left= %d}" address left
    | Too_many {address; wait} ->
        Format.fprintf ppf "Too_many {address= %S; wait= %Ld ns}" address
          (Span.to_ns wait)

  let check t req text =
    match Option.bind (Caravan_session.Private.find t.key req) decode with
    | None ->
        Error No_code
    | Some e -> (
        let address = e.address in
        if now_s () >= e.expires then (
          Caravan_session.Private.remove t.key req ;
          Error Expired )
        else
          (* A token before the compare: a right code costs one too. Else an
             attacker over the limit would learn the right code. *)
          match t.checks (limit_key "check:" address) with
          | Some wait ->
              Error (Too_many {address; wait})
          | None ->
              (* A long text is not a code: no copy of it. *)
              let code =
                if String.length text > 64 then ""
                else
                  String.of_seq
                    (Seq.filter
                       (fun c -> c <> ' ' && c <> '-')
                       (String.to_seq text) )
              in
              (* The length first: no hash of a text of another length. A text
                 with a non-digit cannot have the hash of a code. *)
              let right =
                String.length code = t.digits
                && Eqaf.equal (sha256 (e.salt ^ code)) e.hash
              in
              if right then (
                Caravan_session.Private.remove t.key req ;
                Ok address )
              else
                (* [start] sets 1 or more, and the code goes at 0. *)
                let left = e.left - 1 in
                if left = 0 then Caravan_session.Private.remove t.key req
                else
                  Caravan_session.Private.set t.key (encode {e with left}) req ;
                Error (Wrong {address; left}) )
end
