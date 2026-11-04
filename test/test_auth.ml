(* The tests of caravan.auth. The known answers are the examples of RFC 7515
   Appendix A and RFC 8037 Appendix A. The tests run in the simulator: its posix
   clock moves the time checks. *)

open Windtrap
open Weft
module J = Caravan_auth.Jwt
module B64 = Caravan.Private.Base64

let b64 = B64.encode ~url:true ~pad:false

let unb64 s = Option.get (B64.decode ~url:true ~pad:false s)

let sha256 s = Digestif.SHA256.(to_raw_string (digest_string s))

let z_of_b64 s =
  let b = unb64 s in
  let n = String.length b in
  Z.of_bits (String.init n (fun i -> b.[n - 1 - i]))

(* The exp of the RFC examples: 2011-03-22T18:43:00Z. *)
let rfc_exp = 1300819380.

(* [at t f] runs [f] in the simulator, with the posix clock at [t] seconds. *)
let at t f =
  Sim_support.run (fun s ->
      Weft_sim.set_posix s (Time.posix_of_unix_s t) ;
      f s )

let set s t = Weft_sim.set_posix s (Time.posix_of_unix_s t)

(* {1 The keys of the RFC examples} *)

let rfc_hs_secret =
  unb64
    "AyM1SysPpbyDfgZld3umj1qzKObwVMkoqQ-EstJQLr_T-1qS0gZH75aKtMN3Yj0iPS4hcgUuTwjAzZr1Z9CAow"

let rfc_rsa_n =
  "ofgWCuLjybRlzo0tZWJjNiuSfb4p4fAkd_wWJcyQoTbji9k0l8W26mPddxHmfHQp-Vaw-4qPCJrcS2mJPMEzP1Pt0Bm4d4QlL-yRT-SFd2lZS-pCgNMsD1W_YpRPEwOWvG6b32690r2jZ47soMZo9wGzjb_7OMg0LOL-bSf63kpaSHSXndS5z5rexMdbBYUsLA9e-KXBdQOS-UTo7WTBEMa2R2CapHg665xsmtdVMTBQY4uDZlxvb3qCo5ZwKh9kG4LT6_I5IhlJH7aGhyxXFvUK-DWNmoudF8NAco9_h9iaGNj8q2ethFkMLs91kzk2PAcDTW9gb54h4FRWyuXpoQ"

let rfc_rsa_p =
  "4BzEEOtIpmVdVEZNCqS7baC4crd0pqnRH_5IB3jw3bcxGn6QLvnEtfdUdiYrqBdss1l58BQ3KhooKeQTa9AB0Hw_Py5PJdTJNPY8cQn7ouZ2KKDcmnPGBY5t7yLc1QlQ5xHdwW1VhvKn-nXqhJTBgIPgtldC-KDV5z-y2XDwGUc"

let rfc_rsa_q =
  "uQPEfgmVtjL0Uyyx88GZFF1fOunH3-7cepKmtH4pxhtCoHqpWmT8YAmZxaewHgHAjLYsp1ZSe7zFYHj7C6ul7TjeLQeZD_YwD66t62wDmpe_HlB-TnBA-njbglfIsRLtXlnDzQkv5dTltRJ11BKBBypeeF6689rjcJIDEz9RWdc"

let rsa_priv =
  Result.get_ok
    (Mirage_crypto_pk.Rsa.priv_of_primes ~e:(Z.of_int 65537)
       ~p:(z_of_b64 rfc_rsa_p) ~q:(z_of_b64 rfc_rsa_q) )

let rsa_pub = Mirage_crypto_pk.Rsa.pub_of_priv rsa_priv

let rfc_ec_x = "f83OJ3D2xF1Bg8vub9tLe1gHMzV76e8Tus9uPHvRVEU"

let rfc_ec_y = "x_FEzRu9m36HLN_tue659LNpXW6pCyStikYjKIWI5a0"

let ec_priv =
  Result.get_ok
    (Mirage_crypto_ec.P256.Dsa.priv_of_octets
       (unb64 "jpsQnnGQmL-YBIffH1136cspYG6-0iY7X1fCE9-E9LI") )

let rfc_ed_x = "11qYAYKxCrfVS_7TyWQHOg7hcvPapiMlrwIaaPcHURo"

let ed_priv =
  Result.get_ok
    (Mirage_crypto_ec.Ed25519.priv_of_octets
       (unb64 "nWGxne_9WmC6hEr0kuwsxERJxWl7MmkZcDusAxyuf2A") )

let jwk_rsa = Printf.sprintf {|{"kty":"RSA","n":"%s","e":"AQAB"}|} rfc_rsa_n

let jwk_ec =
  Printf.sprintf {|{"kty":"EC","crv":"P-256","x":"%s","y":"%s"}|} rfc_ec_x
    rfc_ec_y

let jwk_ed = Printf.sprintf {|{"kty":"OKP","crv":"Ed25519","x":"%s"}|} rfc_ed_x

let jwks l = {|{"keys":[|} ^ String.concat "," l ^ "]}"

let keys_of text = Result.get_ok (J.Key.of_jwks text)

let key_of jwk =
  match keys_of (jwks [jwk]) with [k] -> k | _ -> fail "one key"

let hs_key = J.Key.of_secret rfc_hs_secret

let rsa_key = key_of jwk_rsa

let ec_key = key_of jwk_ec

let ed_key = key_of jwk_ed

(* {1 The RFC tokens} *)

let rfc_payload =
  "eyJpc3MiOiJqb2UiLA0KICJleHAiOjEzMDA4MTkzODAsDQogImh0dHA6Ly9leGFtcGxlLmNvbS9pc19yb290Ijp0cnVlfQ"

let rfc_hs =
  "eyJ0eXAiOiJKV1QiLA0KICJhbGciOiJIUzI1NiJ9." ^ rfc_payload
  ^ ".dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk"

let rfc_rs =
  "eyJhbGciOiJSUzI1NiJ9." ^ rfc_payload
  ^ ".cC4hiUPoj9Eetdgtv3hF80EGrhuB__dzERat0XF9g2VtQgr9PJbu3XOiZj5RZmh7AAuHIm4Bh-0Qc_lF5YKt_O8W2Fp5jujGbds9uJdbF9CUAr7t1dnZcAcQjbKBYNX4BAynRFdiuB--f_nZLgrnbyTyWzO75vRK5h6xBArLIARNPvkSjtQBMHlb1L07Qe7K0GarZRmB_eSN9383LcOLn6_dO--xi12jzDwusC-eOkHWEsqtFZESc6BfI7noOPqvhJ1phCnvWh6IeYI2w9QOYEUipUTI8np6LbgGY9Fs98rqVt5AXLIhWkWywlVmtVrBp0igcN_IoypGlUPQGe77Rw"

let rfc_es =
  "eyJhbGciOiJFUzI1NiJ9." ^ rfc_payload
  ^ ".DtEhU3ljbEg8L38VWAfUAqOyKAM6-Xx-F4GawxaepmXFCgfTjDxw5djxLa8ISlSApmWQxfKTUJqPP3-Kg6NU1Q"

let rfc_ed =
  "eyJhbGciOiJFZERTQSJ9.RXhhbXBsZSBvZiBFZDI1NTE5IHNpZ25pbmc.hgyY0il_MGCjP0JzlnLWG1PPOt7-09PGcvMg3AIbQR6dWbhijcNR4ki4iylGjg5BhVsPt9g7sVvpAr_MuM0KAg"

(* {1 Tokens of the tests} *)

type signer = string -> string

let hs ?(secret = rfc_hs_secret) () : signer =
 fun input -> Digestif.SHA256.(to_raw_string (hmac_string ~key:secret input))

let rs ?(hash = `SHA256) () : signer =
 fun input ->
  Mirage_crypto_pk.Rsa.PKCS1.sign ~mask:`No ~hash ~key:rsa_priv (`Message input)

let es : signer =
 fun input ->
  let r, s =
    Mirage_crypto_ec.P256.Dsa.sign ~mask:`No ~key:ec_priv (sha256 input)
  in
  r ^ s

let ed : signer = fun input -> Mirage_crypto_ec.Ed25519.sign ~key:ed_priv input

(* [token ~header sign payload] is a token of the JSON texts [header] and
   [payload], signed with [sign]. *)
let token ~header (sign : signer) payload =
  let input = b64 header ^ "." ^ b64 payload in
  input ^ "." ^ b64 (sign input)

let alg_header a = Printf.sprintf {|{"alg":"%s"}|} a

(* A payload with [exp] at [rfc_exp], [iss] "joe" and [aud] "app". *)
let claims_text =
  Printf.sprintf {|{"iss":"joe","aud":"app","exp":%.0f,"sub":"ada"}|} rfc_exp

let hs_token ?(header = alg_header "HS256") ?(payload = claims_text) () =
  token ~header (hs ()) payload

(* The time of most tests: one hour before [rfc_exp]. *)
let before = rfc_exp -. 3600.

let result =
  Testable.make
    ~pp:(fun ppf -> function
      | Ok _ -> Format.pp_print_string ppf "Ok" | Error e -> J.pp_error ppf e )
    ~equal:(fun a b ->
      match (a, b) with
      | Ok _, Ok _ ->
          true
      | Error a, Error b ->
          a = b
      | _ ->
          false )

let ok = Ok ()

let error e = Error e

(* [check ?leeway ?require_exp ?issuer ?audience ~keys t] is the result of
   [J.verify] with no claims, for a compare. *)
let check ?leeway ?max_length ?require_exp ?(issuer = J.Any) ?(audience = J.Any)
    ~keys t =
  Result.map ignore
    (J.verify ?leeway ?max_length ?require_exp ~issuer ~audience ~keys t)

let rfc_tests =
  group "The RFC examples"
    [ test "RFC 7515 A.1, A.2 and A.3 verify" (fun () ->
          at before (fun _ ->
              List.iter
                (fun (label, keys, t) ->
                  equal ~msg:label result ok (check ~keys t) )
                [ ("HS256", [hs_key], rfc_hs)
                ; ("RS256", [rsa_key], rfc_rs)
                ; ("ES256", [ec_key], rfc_es) ] ) )
    ; test "the claims of the example" (fun () ->
          at before (fun _ ->
              let c =
                Result.get_ok
                  (J.verify ~issuer:(One_of ["joe"]) ~audience:Any
                     ~keys:[hs_key] rfc_hs )
              in
              equal (option string) (Some "joe") (J.issuer c) ;
              equal (option bool) (Some true)
                (J.bool "http://example.com/is_root" c) ;
              equal (option int) (Some 1300819380) (J.int "exp" c) ;
              equal (option float_exact) (Some rfc_exp)
                (Option.map Time.posix_to_unix_s (J.expires c)) ;
              equal (option string) None (J.subject c) ;
              equal (list string) [] (J.audience c) ;
              equal string
                "{\"iss\":\"joe\",\r\n\
                \ \"exp\":1300819380,\r\n\
                \ \"http://example.com/is_root\":true}"
                (J.payload c) ) )
    ; test "RFC 8037 A.4: the signature is good, the payload is not JSON"
        (fun () ->
          at before (fun _ ->
              equal result
                (error (J.Malformed "the payload: not JSON"))
                (check ~keys:[ed_key] rfc_ed) ;
              (* The token ends with "Ag": "Bg" changes one bit. *)
              let bad = String.sub rfc_ed 0 (String.length rfc_ed - 2) ^ "Bg" in
              equal result (error J.Bad_signature) (check ~keys:[ed_key] bad) ) )
    ; test "an EdDSA token of the RFC key" (fun () ->
          at before (fun _ ->
              equal result ok
                (check ~keys:[ed_key]
                   (token ~header:(alg_header "EdDSA") ed claims_text) ) ;
              equal result ok
                (check ~keys:[ed_key]
                   (token ~header:(alg_header "Ed25519") ed claims_text) ) ) )
    ]

let time_tests =
  group "Times"
    [ test "exp and the leeway" (fun () ->
          at before (fun s ->
              let t = hs_token () in
              let zero = Span.zero in
              set s (rfc_exp -. 0.001) ;
              equal result ok (check ~leeway:zero ~keys:[hs_key] t) ;
              set s rfc_exp ;
              equal result (error J.Expired)
                (check ~leeway:zero ~keys:[hs_key] t) ;
              (* The default leeway is 60 s. *)
              equal result ok (check ~keys:[hs_key] t) ;
              set s (rfc_exp +. 59.999) ;
              equal result ok (check ~keys:[hs_key] t) ;
              set s (rfc_exp +. 60.) ;
              equal result (error J.Expired) (check ~keys:[hs_key] t) ;
              equal result ok (check ~leeway:(Span.s 120) ~keys:[hs_key] t) ) )
    ; test "nbf and the leeway" (fun () ->
          let nbf = rfc_exp -. 1000. in
          let t =
            hs_token
              ~payload:(Printf.sprintf {|{"exp":%.0f,"nbf":%.0f}|} rfc_exp nbf)
              ()
          in
          at (nbf -. 60.) (fun s ->
              equal result ok (check ~keys:[hs_key] t) ;
              set s (nbf -. 60.001) ;
              equal result (error J.Not_yet_valid) (check ~keys:[hs_key] t) ;
              set s (nbf -. 0.001) ;
              equal result (error J.Not_yet_valid)
                (check ~leeway:Span.zero ~keys:[hs_key] t) ;
              set s nbf ;
              equal result ok (check ~leeway:Span.zero ~keys:[hs_key] t) ) )
    ; test "nbf after exp is malformed" (fun () ->
          at (rfc_exp -. 10.) (fun _ ->
              let t =
                hs_token ~payload:{|{"exp":1300819380,"nbf":1300819381}|} ()
              in
              equal result
                (error (J.Malformed "nbf is after exp"))
                (check ~keys:[hs_key] t) ;
              let t =
                hs_token ~payload:{|{"exp":1300819380,"nbf":1300819380}|} ()
              in
              equal result ok (check ~keys:[hs_key] t) ) )
    ; test "exp is required by default" (fun () ->
          at before (fun _ ->
              let t = hs_token ~payload:{|{"iss":"joe"}|} () in
              equal result (error (J.Missing "exp")) (check ~keys:[hs_key] t) ;
              equal result ok (check ~require_exp:false ~keys:[hs_key] t) ) )
    ; test "the types and sizes of the time claims" (fun () ->
          at before (fun _ ->
              List.iter
                (fun (payload, expected) ->
                  equal ~msg:payload result expected
                    (check ~keys:[hs_key] (hs_token ~payload ())) )
                [ ( {|{"exp":"1300819380"}|}
                  , error (J.Malformed "exp is not a number") )
                ; ( {|{"exp":1300819380,"nbf":null}|}
                  , error (J.Malformed "nbf is not a number") )
                ; ( {|{"exp":1300819380,"iat":true}|}
                  , error (J.Malformed "iat is not a number") )
                ; ({|{"exp":1300819380.5}|}, ok)
                ; ({|{"exp":1e300}|}, ok)
                ; ({|{"exp":99999999999999999999}|}, ok)
                ; ( "{\"exp\":1" ^ String.make 400 '0' ^ "}"
                  , error
                      (J.Malformed "the payload: a number that is not finite")
                  )
                ; ( {|{"exp":1e400}|}
                  , error
                      (J.Malformed "the payload: a number that is not finite")
                  ) ] ;
              (* A time that a posix time cannot hold: no value. *)
              let c =
                Result.get_ok
                  (J.verify ~issuer:Any ~audience:Any ~keys:[hs_key]
                     (hs_token ~payload:{|{"exp":1e300,"iat":-1e300,"nbf":1}|}
                        () ) )
              in
              equal (option float_exact) None
                (Option.map Time.posix_to_unix_s (J.expires c)) ;
              equal (option float_exact) None
                (Option.map Time.posix_to_unix_s (J.issued_at c)) ;
              equal (option float_exact) (Some 1.)
                (Option.map Time.posix_to_unix_s (J.not_before c)) ;
              let c =
                Result.get_ok
                  (J.verify ~issuer:Any ~audience:Any ~keys:[hs_key]
                     (hs_token ~payload:{|{"exp":99999999999999999999}|} ()) )
              in
              equal (option float_exact) (Some 1e20) (J.number "exp" c) ;
              equal (option int) None (J.int "exp" c) ) ) ]

let issuer_tests =
  group "Issuer and audience"
    [ test "the issuer" (fun () ->
          at before (fun _ ->
              let t = hs_token () in
              let check issuer t = check ~issuer ~keys:[hs_key] t in
              equal result ok (check (One_of ["x"; "joe"]) t) ;
              equal result (error J.Bad_issuer) (check (One_of ["Joe"]) t) ;
              let no_iss = hs_token ~payload:{|{"exp":1300819380}|} () in
              equal result (error (J.Missing "iss"))
                (check (One_of ["joe"]) no_iss) ;
              equal result ok (check Any no_iss) ;
              let bad = hs_token ~payload:{|{"exp":1300819380,"iss":1}|} () in
              equal result
                (error (J.Malformed "iss is not a string"))
                (check Any bad) ) )
    ; test "the audience" (fun () ->
          at before (fun _ ->
              let check audience payload =
                check ~audience ~keys:[hs_key] (hs_token ~payload ())
              in
              let p aud = Printf.sprintf {|{"exp":1300819380,"aud":%s}|} aud in
              equal result ok (check (One_of ["app"]) (p {|"app"|})) ;
              equal result ok (check (One_of ["b"]) (p {|["a","b"]|})) ;
              equal result (error J.Bad_audience)
                (check (One_of ["c"]) (p {|["a","b"]|})) ;
              equal result (error J.Bad_audience)
                (check (One_of ["App"]) (p {|"app"|})) ;
              equal result (error (J.Missing "aud"))
                (check (One_of ["app"]) {|{"exp":1300819380}|}) ;
              List.iter
                (fun (aud, m) ->
                  equal ~msg:aud result (error (J.Malformed m))
                    (check Any (p aud)) )
                [ ("[]", "aud is not a string or a list of strings")
                ; ("1", "aud is not a string or a list of strings")
                ; ({|["a",1]|}, "aud has an element that is not a string") ] ;
              let c =
                Result.get_ok
                  (J.verify ~issuer:Any ~audience:Any ~keys:[hs_key]
                     (hs_token ~payload:(p {|["a","b"]|}) ()) )
              in
              equal (list string) ["a"; "b"] (J.audience c) ;
              let one =
                Result.get_ok
                  (J.verify ~issuer:Any ~audience:Any ~keys:[hs_key]
                     (hs_token ()) )
              in
              equal (list string) ["app"] (J.audience one) ;
              equal (option (list string)) (Some ["a"; "b"]) (J.strings "aud" c) ) )
    ; test "One_of [] and other bad arguments raise" (fun () ->
          let t = hs_token () in
          let raises f =
            raises_match (function Invalid_argument _ -> true | _ -> false) f
          in
          raises (fun () ->
              ignore
                (J.verify ~issuer:(One_of []) ~audience:Any ~keys:[hs_key] t) ) ;
          raises (fun () ->
              ignore
                (J.verify ~issuer:Any ~audience:(One_of []) ~keys:[hs_key] t) ) ;
          raises (fun () ->
              ignore
                (J.verify ~leeway:(Span.ns (-1)) ~issuer:Any ~audience:Any
                   ~keys:[hs_key] t ) ) ;
          raises (fun () ->
              ignore
                (J.verify ~max_length:0 ~issuer:Any ~audience:Any ~keys:[hs_key]
                   t ) ) ) ]

let signature_tests =
  group "Signatures and keys"
    [ test "another payload or an empty signature gives Bad_signature" (fun () ->
          at before (fun _ ->
              List.iter
                (fun (label, keys, sign, alg) ->
                  let t = token ~header:(alg_header alg) sign claims_text in
                  equal ~msg:label result ok (check ~keys t) ;
                  (* Another payload with the same signature. *)
                  let other =
                    match String.split_on_char '.' t with
                    | [h; _; s] ->
                        h ^ "."
                        ^ b64 {|{"exp":1300819380,"iss":"eve"}|}
                        ^ "." ^ s
                    | _ ->
                        fail "three parts"
                  in
                  equal ~msg:label result (error J.Bad_signature)
                    (check ~keys other) ;
                  (* An empty signature. *)
                  let empty = String.sub t 0 (String.rindex t '.' + 1) in
                  equal ~msg:label result (error J.Bad_signature)
                    (check ~keys empty) )
                [ ("HS256", [hs_key], hs (), "HS256")
                ; ("RS256", [rsa_key], rs (), "RS256")
                ; ("ES256", [ec_key], es, "ES256")
                ; ("EdDSA", [ed_key], ed, "EdDSA") ] ) )
    ; test "RS256 needs SHA-256" (fun () ->
          at before (fun _ ->
              List.iter
                (fun hash ->
                  equal result (error J.Bad_signature)
                    (check ~keys:[rsa_key]
                       (token ~header:(alg_header "RS256") (rs ~hash ())
                          claims_text ) ) )
                [`SHA1; `SHA512] ) )
    ; test "ES256 needs r and s of 32 bytes" (fun () ->
          at before (fun _ ->
              let input = b64 (alg_header "ES256") ^ "." ^ b64 claims_text in
              let sig_ = es input in
              let with_sig s = input ^ "." ^ b64 s in
              equal result ok (check ~keys:[ec_key] (with_sig sig_)) ;
              List.iter
                (fun s ->
                  equal result (error J.Bad_signature)
                    (check ~keys:[ec_key] (with_sig s)) )
                [ String.sub sig_ 0 63
                ; sig_ ^ "\000"
                ; "\000" ^ String.sub sig_ 0 63 ] ) )
    ; test "ES256 accepts the high form of s" (fun () ->
          at before (fun _ ->
              let input = b64 (alg_header "ES256") ^ "." ^ b64 claims_text in
              let sig_ = es input in
              let with_sig s = input ^ "." ^ b64 s in
              (* The high form of s also verifies (no unique text). *)
              let n =
                Z.of_string
                  "115792089210356248762697446949407573529996955224135760342422259061068512044369"
              in
              let z_of s = Z.of_bits (String.init 32 (fun i -> s.[31 - i])) in
              let bytes_of z =
                let b = Z.to_bits z in
                String.init 32 (fun i ->
                    if 31 - i < String.length b then b.[31 - i] else '\000' )
              in
              let s' = bytes_of (Z.sub n (z_of (String.sub sig_ 32 32))) in
              equal result ok
                (check ~keys:[ec_key] (with_sig (String.sub sig_ 0 32 ^ s'))) ) )
    ; test "alg none and other names are refused" (fun () ->
          at before (fun _ ->
              List.iter
                (fun a ->
                  equal ~msg:a result
                    (error (J.Unsupported ("alg " ^ a)))
                    (check
                       ~keys:[hs_key; rsa_key; ec_key; ed_key]
                       (token ~header:(alg_header a) (hs ()) claims_text) ) )
                ["none"; "nOne"; "HS512"; "RS384"; "PS256"; "hs256"; ""] ) )
    ; test "no algorithm confusion" (fun () ->
          at before (fun _ ->
              (* An HS256 token with the public key as the secret. *)
              let pem = X509.Public_key.encode_pem (`RSA rsa_pub) in
              let t =
                token ~header:(alg_header "HS256") (hs ~secret:pem ())
                  claims_text
              in
              equal result (error J.No_key) (check ~keys:[rsa_key] t) ;
              equal result (error J.No_key)
                (check ~keys:[ec_key; ed_key] (hs_token ())) ) )
    ; test "keys from the token are ignored" (fun () ->
          at before (fun _ ->
              (* The header carries the public key of the attacker (jwk) and a
                 certificate of another key (x5c); the attacker key signs. *)
              let attacker =
                Result.get_ok
                  (Mirage_crypto_ec.P256.Dsa.priv_of_octets
                     (String.make 32 '\007') )
              in
              let point =
                Mirage_crypto_ec.P256.Dsa.(pub_to_octets (pub_of_priv attacker))
              in
              let _, leaf, leaf_priv = Tls_support.pki () in
              let header =
                Printf.sprintf
                  {|{"alg":"ES256","jwk":{"kty":"EC","crv":"P-256","x":"%s","y":"%s"},"jku":"https://evil.example/k","x5c":["%s"]}|}
                  (b64 (String.sub point 1 32))
                  (b64 (String.sub point 33 32))
                  (B64.encode ~url:false ~pad:true
                     (X509.Certificate.encode_der leaf) )
              in
              let sign input =
                let r, s =
                  Mirage_crypto_ec.P256.Dsa.sign ~mask:`No ~key:attacker
                    (sha256 input)
                in
                r ^ s
              in
              equal result (error J.Bad_signature)
                (check ~keys:[ec_key] (token ~header sign claims_text)) ;
              (* The key of the certificate in x5c signs. *)
              let leaf_key =
                match leaf_priv with `P256 k -> k | _ -> fail "a P-256 key"
              in
              let sign input =
                let r, s =
                  Mirage_crypto_ec.P256.Dsa.sign ~mask:`No ~key:leaf_key
                    (sha256 input)
                in
                r ^ s
              in
              equal result (error J.Bad_signature)
                (check ~keys:[ec_key] (token ~header sign claims_text)) ) )
    ; test "the kid" (fun () ->
          at before (fun _ ->
              let k id s = J.Key.of_secret ~kid:id s in
              let a = String.make 32 'a' and b = String.make 32 'b' in
              let t kid secret =
                token
                  ~header:(Printf.sprintf {|{"alg":"HS256","kid":"%s"}|} kid)
                  (hs ~secret ()) claims_text
              in
              equal result ok (check ~keys:[k "b" b; k "a" a] (t "a" a)) ;
              (* A key with another kid is not tried. *)
              equal result (error J.No_key) (check ~keys:[k "b" a] (t "a" a)) ;
              (* A key with no kid is tried after the keys of the kid. *)
              equal result ok
                (check ~keys:[k "a" b; J.Key.of_secret a] (t "a" a)) ;
              (* No kid in the token: the keys with no kid, or else the first
                 four keys of the alg. *)
              let no_kid secret =
                token ~header:(alg_header "HS256") (hs ~secret ()) claims_text
              in
              equal result ok (check ~keys:[k "x" b; k "y" a] (no_kid a)) ;
              let c = String.make 32 'c' in
              equal result ok
                (check ~keys:[k "1" b; k "2" b; k "3" b; k "4" a] (no_kid a)) ;
              equal result (error J.Bad_signature)
                (check
                   ~keys:[k "1" b; k "2" b; k "3" b; k "4" c; k "5" a]
                   (no_kid a) ) ;
              (* Keys of other algs do not count. *)
              equal result ok
                (check
                   ~keys:[rsa_key; ec_key; k "1" b; k "2" b; k "3" b; k "4" a]
                   (no_kid a) ) ;
              equal result (error J.No_key)
                (check ~keys:[rsa_key; ec_key] (no_kid a)) ;
              equal result ok (check ~keys:[k "y" a] (no_kid a)) ;
              equal result ok
                (check ~keys:[k "y" b; J.Key.of_secret a] (no_kid a)) ;
              equal result (error J.Bad_signature)
                (check ~keys:[J.Key.of_secret b; k "y" a] (no_kid a)) ;
              equal (option string) (Some "x") (J.Key.kid (k "x" a)) ;
              equal (option string) None (J.Key.kid hs_key) ;
              let kid_type =
                token ~header:{|{"alg":"HS256","kid":1}|} (hs ()) claims_text
              in
              equal result
                (error (J.Malformed "kid is not a string"))
                (check ~keys:[hs_key] kid_type) ) )
    ; test "a secret has 32 bytes or more" (fun () ->
          let raises f =
            raises_match (function Invalid_argument _ -> true | _ -> false) f
          in
          raises (fun () -> ignore (J.Key.of_secret (String.make 31 'a'))) ;
          ignore (J.Key.of_secret (String.make 32 'a')) ;
          raises (fun () ->
              ignore (J.Key.of_secret_base64url (b64 (String.make 31 'a'))) ) ;
          raises (fun () -> ignore (J.Key.of_secret_base64url "a=")) ;
          let k =
            J.Key.of_secret_base64url ~kid:"s" (b64 (String.make 32 'a'))
          in
          equal (option string) (Some "s") (J.Key.kid k) ;
          at before (fun _ ->
              equal result ok
                (check ~keys:[k]
                   (token ~header:(alg_header "HS256")
                      (hs ~secret:(String.make 32 'a') ())
                      claims_text ) ) ;
              equal result ok
                (check
                   ~keys:
                     [ J.Key.of_secret_base64url
                         "AyM1SysPpbyDfgZld3umj1qzKObwVMkoqQ-EstJQLr_T-1qS0gZH75aKtMN3Yj0iPS4hcgUuTwjAzZr1Z9CAow"
                     ]
                   rfc_hs ) ) )
    ; test "of_pem" (fun () ->
          at before (fun _ ->
              let pem k = X509.Public_key.encode_pem k in
              let rsa = J.Key.of_pem (pem (`RSA rsa_pub)) in
              equal result ok
                (check ~keys:[rsa]
                   (token ~header:(alg_header "RS256") (rs ()) claims_text) ) ;
              let ec =
                J.Key.of_pem ~kid:"e"
                  (pem (`P256 (Mirage_crypto_ec.P256.Dsa.pub_of_priv ec_priv)))
              in
              equal (option string) (Some "e") (J.Key.kid ec) ;
              equal result ok
                (check ~keys:[ec]
                   (token ~header:(alg_header "ES256") es claims_text) ) ;
              let edk =
                J.Key.of_pem
                  (pem
                     (`ED25519 (Mirage_crypto_ec.Ed25519.pub_of_priv ed_priv)) )
              in
              equal result ok
                (check ~keys:[edk]
                   (token ~header:(alg_header "EdDSA") ed claims_text) ) ;
              List.iter
                (fun (k, a) -> is_true ~msg:"alg" (J.Key.alg k = a))
                [ (rsa, J.RS256)
                ; (ec, J.ES256)
                ; (edk, J.EdDSA)
                ; (hs_key, J.HS256) ] ;
              let bad text =
                raises_match
                  (function Invalid_argument _ -> true | _ -> false)
                  (fun () -> ignore (J.Key.of_pem text))
              in
              bad "not a PEM" ;
              let p384 = fst (Mirage_crypto_ec.P384.Dsa.generate ()) in
              bad (pem (`P384 (Mirage_crypto_ec.P384.Dsa.pub_of_priv p384))) ;
              let small = Mirage_crypto_pk.Rsa.generate ~bits:1024 () in
              bad (pem (`RSA (Mirage_crypto_pk.Rsa.pub_of_priv small))) ) )
    ; test "of_pem takes a certificate" (fun () ->
          at before (fun _ ->
              let _, leaf, _ = Tls_support.pki () in
              let k = J.Key.of_pem (X509.Certificate.encode_pem leaf) in
              is_true (J.Key.alg k = J.ES256) ) ) ]

let jwks_tests =
  group "JWK Sets"
    [ test "the keys that are kept and skipped" (fun () ->
          let count l = List.length (keys_of (jwks l)) in
          equal int 3 (count [jwk_rsa; jwk_ec; jwk_ed]) ;
          let with_member jwk m =
            String.sub jwk 0 (String.length jwk - 1) ^ "," ^ m ^ "}"
          in
          List.iter
            (fun (label, jwk, n) -> equal ~msg:label int n (count [jwk]))
            [ ("use sig", with_member jwk_ec {|"use":"sig"|}, 1)
            ; ("use enc", with_member jwk_ec {|"use":"enc"|}, 0)
            ; ("use 1", with_member jwk_ec {|"use":1|}, 0)
            ; ("alg ES256", with_member jwk_ec {|"alg":"ES256"|}, 1)
            ; ("alg ES384", with_member jwk_ec {|"alg":"ES384"|}, 0)
            ; ("alg RS256", with_member jwk_rsa {|"alg":"RS256"|}, 1)
            ; ("alg PS256", with_member jwk_rsa {|"alg":"PS256"|}, 0)
            ; ("alg EdDSA", with_member jwk_ed {|"alg":"EdDSA"|}, 1)
            ; ("alg Ed25519", with_member jwk_ed {|"alg":"Ed25519"|}, 1)
            ; ("alg Ed448", with_member jwk_ed {|"alg":"Ed448"|}, 0)
            ; ("key_ops verify", with_member jwk_ec {|"key_ops":["verify"]|}, 1)
            ; ("key_ops sign", with_member jwk_ec {|"key_ops":["sign"]|}, 0)
            ; ("key_ops a string", with_member jwk_ec {|"key_ops":"verify"|}, 0)
            ; ("kid", with_member jwk_ec {|"kid":"k1"|}, 1)
            ; ("kid 1", with_member jwk_ec {|"kid":1|}, 0)
            ; ( "oct"
              , {|{"kty":"oct","k":"AyM1SysPpbyDfgZld3umj1qzKObwVMkoqQ"}|}
              , 0 )
            ; ("no kty", {|{"crv":"P-256"}|}, 0)
            ; ( "P-384"
              , Printf.sprintf {|{"kty":"EC","crv":"P-384","x":"%s","y":"%s"}|}
                  rfc_ec_x rfc_ec_y
              , 0 )
            ; ( "x of 31 bytes"
              , Printf.sprintf {|{"kty":"EC","crv":"P-256","x":"%s","y":"%s"}|}
                  (b64 (String.sub (unb64 rfc_ec_x) 1 31))
                  rfc_ec_y
              , 0 )
            ; ( "y of 33 bytes"
              , Printf.sprintf {|{"kty":"EC","crv":"P-256","x":"%s","y":"%s"}|}
                  rfc_ec_x
                  (b64 ("\000" ^ unb64 rfc_ec_y))
              , 0 )
            ; ( "not on the curve"
              , Printf.sprintf {|{"kty":"EC","crv":"P-256","x":"%s","y":"%s"}|}
                  rfc_ec_x rfc_ec_x
              , 0 )
            ; ( "no crv"
              , Printf.sprintf {|{"kty":"EC","x":"%s","y":"%s"}|} rfc_ec_x
                  rfc_ec_y
              , 0 )
            ; ( "X25519"
              , Printf.sprintf {|{"kty":"OKP","crv":"X25519","x":"%s"}|}
                  rfc_ed_x
              , 0 )
            ; ( "Ed25519 x of 31 bytes"
              , Printf.sprintf {|{"kty":"OKP","crv":"Ed25519","x":"%s"}|}
                  (b64 (String.sub (unb64 rfc_ed_x) 0 31))
              , 0 )
            ; ( "RSA e 3"
              , Printf.sprintf {|{"kty":"RSA","n":"%s","e":"Aw"}|} rfc_rsa_n
              , 1 )
            ; ( "RSA e of 32 bits"
              , Printf.sprintf {|{"kty":"RSA","n":"%s","e":"%s"}|} rfc_rsa_n
                  (b64 "\x80\000\000\001")
              , 1 )
            ; ( "RSA e of 33 bits"
              , Printf.sprintf {|{"kty":"RSA","n":"%s","e":"%s"}|} rfc_rsa_n
                  (b64 "\001\000\000\000\001")
              , 0 )
            ; ( "RSA no e"
              , Printf.sprintf {|{"kty":"RSA","n":"%s"}|} rfc_rsa_n
              , 0 )
            ; ("RSA no n", {|{"kty":"RSA","e":"AQAB"}|}, 0)
            ; ( "RSA n bad base64url"
              , Printf.sprintf {|{"kty":"RSA","n":"%s=","e":"AQAB"}|} rfc_rsa_n
              , 0 ) ] ;
          (* A key of 1024 bits, and one of 8193 bits. *)
          let n_of bits =
            b64
              ( String.make 1 (Char.chr (1 lsl ((bits - 1) mod 8)))
              ^ String.make ((bits - 1) / 8) '\001' )
          in
          List.iter
            (fun (bits, expected) ->
              equal ~msg:(string_of_int bits) int expected
                (count
                   [ Printf.sprintf {|{"kty":"RSA","n":"%s","e":"AQAB"}|}
                       (n_of bits) ] ) )
            [(1024, 0); (2047, 0); (2048, 1); (8192, 1); (8193, 0)] )
    ; test "a key of a set verifies its token" (fun () ->
          at before (fun _ ->
              let keys =
                keys_of
                  (jwks
                     [ String.sub jwk_ec 0 (String.length jwk_ec - 1)
                       ^ {|,"kid":"e1"}|}
                     ; jwk_rsa ] )
              in
              equal result ok
                (check ~keys
                   (token ~header:{|{"alg":"ES256","kid":"e1"}|} es claims_text) ) ;
              equal result ok (check ~keys rfc_rs) ) )
    ; test "bad sets" (fun () ->
          let err text =
            match J.Key.of_jwks text with Error e -> e | Ok _ -> "ok"
          in
          equal string "no array keys" (err "{}") ;
          equal string "no array keys" (err {|{"keys":{}}|}) ;
          equal string "no array keys" (err "[]") ;
          equal string "not JSON" (err "{keys:[]}") ;
          equal string "a name two times" (err {|{"keys":[],"keys":[]}|}) ;
          equal string "the text is too large"
            ( match J.Key.of_jwks ~max_length:10 {|{"keys":[]}|} with
            | Error e ->
                e
            | Ok _ ->
                "ok" ) ;
          equal int 0
            (List.length
               (Result.get_ok (J.Key.of_jwks ~max_length:11 {|{"keys":[]}|})) ) ;
          raises_match
            (function Invalid_argument _ -> true | _ -> false)
            (fun () -> ignore (J.Key.of_jwks ~max_length:0 "{}")) ;
          equal string "the text is too large"
            ( match J.Key.of_jwks ~max_length:1 "{}" with
            | Error e ->
                e
            | Ok _ ->
                "ok" ) ;
          (* The default limit is 64 KiB. *)
          let padded n =
            {|{"keys":[]}|}
            ^ String.make (n - String.length {|{"keys":[]}|}) ' '
          in
          is_true ~msg:"64 KiB" (Result.is_ok (J.Key.of_jwks (padded 65536))) ;
          equal string "the text is too large"
            ( match J.Key.of_jwks (padded 65537) with
            | Error e ->
                e
            | Ok _ ->
                "ok" ) ) ]

let form_tests =
  group "The form of a token"
    [ test "malformed tokens" (fun () ->
          at before (fun _ ->
              let t = hs_token () in
              let h, p, s =
                match String.split_on_char '.' t with
                | [h; p; s] ->
                    (h, p, s)
                | _ ->
                    fail "three parts"
              in
              List.iter
                (fun (label, v, m) ->
                  equal ~msg:label result (error (J.Malformed m))
                    (check ~keys:[hs_key] v) )
                [ ("two parts", h ^ "." ^ p, "not three parts")
                ; ("four parts", t ^ ".x", "not three parts")
                ; ("padding", h ^ "=." ^ p ^ "." ^ s, "a part is not base64url")
                ; ( "plus in the header"
                  , h ^ "+." ^ p ^ "." ^ s
                  , "a part is not base64url" )
                ; ("empty header", "." ^ p ^ "." ^ s, "the header: not JSON")
                ; ( "an array"
                  , b64 "[1]" ^ "." ^ p ^ "." ^ s
                  , "the header is not an object" )
                ; ("no alg", b64 "{}" ^ "." ^ p ^ "." ^ s, "no alg string")
                ; ( "alg null"
                  , b64 {|{"alg":null}|} ^ "." ^ p ^ "." ^ s
                  , "no alg string" )
                ; ( "alg 1"
                  , b64 {|{"alg":1}|} ^ "." ^ p ^ "." ^ s
                  , "no alg string" )
                ; ( "two alg"
                  , b64 {|{"alg":"HS256","alg":"none"}|} ^ "." ^ p ^ "." ^ s
                  , "the header: a name two times" )
                ; ( "an escaped duplicate"
                  , b64 {|{"alg":"HS256","\u0061lg":"none"}|}
                    ^ "." ^ p ^ "." ^ s
                  , "the header: a name two times" )
                ; ( "a duplicate that is not next to its twin"
                  , b64 {|{"alg":"HS256","typ":"JWT","alg":"none"}|}
                    ^ "." ^ p ^ "." ^ s
                  , "the header: a name two times" )
                ; ( "a duplicate in an array"
                  , b64 {|{"alg":"HS256","x":[{"a":1,"a":2}]}|}
                    ^ "." ^ p ^ "." ^ s
                  , "the header: a name two times" )
                ; ( "a number that is not finite in an array"
                  , b64 {|{"alg":"HS256","x":[1e400]}|} ^ "." ^ p ^ "." ^ s
                  , "the header: a number that is not finite" )
                ; ( "the header part is too large"
                  , String.make 8193 'e' ^ "." ^ p ^ "." ^ s
                  , "the header is too large" )
                ; ("no dot", h, "not three parts")
                ; ( "a nested duplicate"
                  , b64 {|{"alg":"HS256","x":{"a":1,"a":2}}|}
                    ^ "." ^ p ^ "." ^ s
                  , "the header: a name two times" ) ] ;
              (* The default limit is 16384 bytes. *)
              equal result
                (error (J.Malformed "not three parts"))
                (check ~keys:[hs_key] (String.make 16384 'a')) ;
              equal result
                (error (J.Malformed "the token is too large"))
                (check ~keys:[hs_key] (String.make 16385 'a')) ;
              equal result
                (error (J.Malformed "the token is too large"))
                (check ~max_length:1 ~keys:[hs_key] t) ;
              (* A header part of 8192 bytes is not too large. *)
              let big =
                b64 ({|{"alg":"HS256","x":"|} ^ String.make 6122 'x' ^ {|"}|})
              in
              equal int 8192 (String.length big) ;
              equal result (error J.Bad_signature)
                (check ~keys:[hs_key] (big ^ "." ^ p ^ "." ^ s)) ;
              (* The length limit. *)
              let n = String.length t in
              equal result ok (check ~max_length:n ~keys:[hs_key] t) ;
              equal result
                (error (J.Malformed "the token is too large"))
                (check ~max_length:(n - 1) ~keys:[hs_key] t) ) )
    ; test "crit and typ" (fun () ->
          at before (fun _ ->
              let t header = token ~header (hs ()) claims_text in
              equal result
                (error (J.Unsupported "crit"))
                (check ~keys:[hs_key] (t {|{"alg":"HS256","crit":[]}|})) ;
              List.iter
                (fun typ ->
                  equal ~msg:typ result ok
                    (check ~keys:[hs_key]
                       (t (Printf.sprintf {|{"alg":"HS256","typ":"%s"}|} typ)) ) )
                [ "JWT"
                ; "jwt"
                ; "at+jwt"
                ; "AT+JWT"
                ; "application/at+jwt"
                ; "Application/JWT" ] ;
              List.iter
                (fun typ ->
                  equal ~msg:typ result
                    (error (J.Unsupported ("typ " ^ typ)))
                    (check ~keys:[hs_key]
                       (t (Printf.sprintf {|{"alg":"HS256","typ":"%s"}|} typ)) ) )
                [ "JWS"
                ; "dpop+jwt"
                ; "logout+jwt"
                ; "application/"
                ; "xapplication/jwt" ] ;
              equal result
                (error (J.Malformed "typ is not a string"))
                (check ~keys:[hs_key] (t {|{"alg":"HS256","typ":1}|})) ) )
    ; test "no key: the signature and the payload are not read" (fun () ->
          at before (fun _ ->
              let h = b64 (alg_header "ES256") in
              equal result (error J.No_key)
                (check ~keys:[hs_key] (h ^ ".!!!.!!!")) ;
              (* With a key, the bad signature part is found. *)
              equal result
                (error (J.Malformed "a part is not base64url"))
                (check ~keys:[ec_key] (h ^ ".!!!.!!!")) ;
              (* With a good signature, the bad payload part is found. *)
              let input = h ^ ".!!!" in
              equal result
                (error (J.Malformed "a part is not base64url"))
                (check ~keys:[ec_key] (input ^ "." ^ b64 (es input))) ;
              equal result
                (error (J.Malformed "a part is not base64url"))
                (check ~keys:[ec_key] ("!!!.e30." ^ b64 "x")) ) )
    ; test "the payload is parsed after the signature" (fun () ->
          at before (fun _ ->
              let input = b64 (alg_header "HS256") ^ "." ^ b64 "not json" in
              let good = input ^ "." ^ b64 ((hs ()) input) in
              equal result
                (error (J.Malformed "the payload: not JSON"))
                (check ~keys:[hs_key] good) ;
              let bad = input ^ "." ^ b64 (String.make 32 'x') in
              equal result (error J.Bad_signature) (check ~keys:[hs_key] bad) ;
              equal result
                (error (J.Malformed "the payload is not an object"))
                (check ~keys:[hs_key] (hs_token ~payload:"[]" ())) ) ) ]

(* The strict JSON check, through the header of a token. *)
let json_tests =
  group "Strict JSON"
    [ test "texts that RFC 8259 refuses" (fun () ->
          at before (fun _ ->
              let check_header h =
                check ~keys:[hs_key] (token ~header:h (hs ()) claims_text)
              in
              List.iter
                (fun h ->
                  equal ~msg:(String.escaped h) result
                    (error (J.Malformed "the header: not JSON"))
                    (check_header h) )
                [ {|{"alg":"HS256" /* */}|}
                ; {|{"alg":"HS256" // x
}|}
                ; {|{alg:"HS256"}|}
                ; {|{"alg":"HS256","x":NaN}|}
                ; {|{"alg":"HS256","x":Infinity}|}
                ; {|{"alg":"HS256","x":-Infinity}|}
                ; "{\"alg\":\"HS256\",\"x\":\"a\001\"}"
                ; {|{"alg":"HS256","x":"\x"}|}
                ; {|{"alg":"HS256","x":"\ud800"}|}
                ; {|{"alg":"HS256","x":"\udc00"}|}
                ; {|{"alg":"HS256","x":"\ud800\u0041"}|}
                ; {|{"alg":"HS256","x":"\ud800x"}|}
                ; {|{"alg":"HS256","x":"\uDC00"}|}
                ; {|{"alg":"HS256","x":"\uD7FF\uDC00"}|}
                ; {|{"alg":"HS256","x":"\uDC00\uDC00"}|}
                ; {|{"alg":"HS256","x":"\u00g0"}|}
                ; {|{"alg":"HS256","x":"\u000g"}|}
                ; {|{"alg":"HS256","x":"\u12"}|}
                ; {|{"alg":"HS256","x":"\u12g4"}|}
                ; {|{"alg":"HS256","x":01}|}
                ; {|{"alg":"HS256","x":1.}|}
                ; {|{"alg":"HS256","x":.5}|}
                ; {|{"alg":"HS256","x":1e}|}
                ; {|{"alg":"HS256","x":+1}|}
                ; {|{"alg":"HS256","x":-}|}
                ; {|{"alg":"HS256","x":[1,]}|}
                ; {|{"alg":"HS256",}|}
                ; {|{"alg":"HS256"} x|}
                ; {|{"alg":"HS256"|}
                ; {|{"alg" "HS256"}|}
                ; {|{"alg":"HS256","x":tru}|}
                ; {|{"alg":"HS256","x":[1 2]}|}
                ; {|{"alg":"HS256","x":"|}
                ; {|{"alg":"HS256","x":"\|}
                ; {|{"alg":"HS256","x":'a'}|}
                ; {|{"alg":"HS256",1:2}|}
                ; {|{"alg":"HS256","x":"\u12|}
                ; "{"
                ; "["
                ; "[1"
                ; "{\"a\":1"
                ; "tru"
                ; {|{"alg":"HS256","x":trux}|}
                ; "-"
                ; "1."
                ; "1e"
                ; "1e+" ] ;
              (* Top-level values that are not objects. *)
              List.iter
                (fun h ->
                  equal ~msg:h result
                    (error (J.Malformed "the header is not an object"))
                    (check_header h) )
                [ "0"
                ; "-1"
                ; "12"
                ; "1.5"
                ; "1e5"
                ; "1E+5"
                ; "true"
                ; "null"
                ; "[]"
                ; "[1]"
                ; "\"a\"" ] ;
              equal result
                (error (J.Malformed "the header: not UTF-8"))
                (check_header "{\"alg\":\"HS256\",\"x\":\"\xff\"}") ;
              (* Texts that RFC 8259 permits. *)
              List.iter
                (fun h ->
                  equal ~msg:(String.escaped h) result ok (check_header h) )
                [ " {\t\"alg\" :\r\n\"HS256\" } "
                ; {|{"alg":"HS256","x":[],"y":{},"z":[1,-0.5e+3,2E-2,0,null,true,false]}|}
                ; {|{"alg":"HS256","x":"\"\\\/\b\f\n\r\t\u00e9\ud83d\ude00"}|}
                ; {|{"alg":"HS256","x":"\u00E9\uD83D\uDE00\uDBFF\uDFFF\uE000"}|}
                ; {|{"alg":"HS256","x":"é"}|}
                ; {|{"alg":"HS256","x":"a b ~"}|}
                ; {|{"alg":"HS256","x":"\uD7FF\uE000"}|}
                ; {|{"alg":"HS256","x":[ 1 , { } ]}|} ] ) )
    ; test "the depth limit is 32" (fun () ->
          at before (fun _ ->
              let nest n = String.make n '[' ^ String.make n ']' in
              let h n = Printf.sprintf {|{"alg":"HS256","x":%s}|} (nest n) in
              (* The object is at depth 1. *)
              equal result ok
                (check ~keys:[hs_key]
                   (token ~header:(h 31) (hs ()) claims_text) ) ;
              equal result
                (error (J.Malformed "the header: not JSON"))
                (check ~keys:[hs_key]
                   (token ~header:(h 32) (hs ()) claims_text) ) ) ) ]

let claims_tests =
  group "The claims"
    [ test "the accessors" (fun () ->
          at before (fun _ ->
              let payload =
                {|{"exp":1300819380,"sub":"ada","roles":["a","b"],"mixed":["a",1],"n":2.5,"i":7,"big":12345678901234567890,"f":false,"o":{}}|}
              in
              let c =
                Result.get_ok
                  (J.verify ~issuer:Any ~audience:Any ~keys:[hs_key]
                     (hs_token ~payload ()) )
              in
              equal (option string) (Some "ada") (J.subject c) ;
              equal (option string) (Some "ada") (J.string "sub" c) ;
              equal (option string) None (J.string "n" c) ;
              equal
                (option (list string))
                (Some ["a"; "b"])
                (J.strings "roles" c) ;
              equal (option (list string)) None (J.strings "mixed" c) ;
              equal (option (list string)) None (J.strings "sub" c) ;
              equal (option float_exact) (Some 2.5) (J.number "n" c) ;
              equal (option float_exact) (Some 7.) (J.number "i" c) ;
              equal (option float_exact) (Some 12345678901234567890.)
                (J.number "big" c) ;
              equal (option float_exact) None (J.number "sub" c) ;
              equal (option int) (Some 7) (J.int "i" c) ;
              equal (option int) None (J.int "n" c) ;
              equal (option bool) (Some false) (J.bool "f" c) ;
              equal (option bool) None (J.bool "sub" c) ;
              equal (option string) None (J.issuer c) ;
              equal (list string) [] (J.audience c) ;
              equal string payload (J.payload c) ;
              equal (option float_exact) None
                (Option.map Time.posix_to_unix_s (J.not_before c)) ) )
    ; test "pp_error" (fun () ->
          List.iter
            (fun (e, s) -> equal string s (Format.asprintf "%a" J.pp_error e))
            [ (J.Malformed "m", {|Malformed "m"|})
            ; (J.Unsupported "u", {|Unsupported "u"|})
            ; (J.Missing "exp", {|Missing "exp"|})
            ; (J.No_key, "No_key")
            ; (J.Bad_signature, "Bad_signature")
            ; (J.Expired, "Expired")
            ; (J.Not_yet_valid, "Not_yet_valid")
            ; (J.Bad_issuer, "Bad_issuer")
            ; (J.Bad_audience, "Bad_audience") ] ) ]

(* {1 Authentication and authorization} *)

module A = Caravan_auth
module S = Caravan_session
open Caravan

let user_payload =
  Printf.sprintf
    {|{"exp":%.0f,"sub":"ada","roles":["admin","dev"],"scope":"read write","n":1}|}
    rfc_exp

let user_token () = hs_token ~payload:user_payload ()

let bearer ?realm ?user ?(keys = fun () -> [hs_key]) () =
  A.Scheme.bearer ?realm ?user ~issuer:Any ~audience:Any ~keys ()

(* A handler that tells the user: "id scheme roles", or "-". *)
let who req =
  Response.string
    ( match A.user req with
    | Some u ->
        String.concat " "
          [A.User.id u; A.User.scheme u; String.concat "," (A.User.roles u)]
    | None ->
        "-" )

let body r =
  match Response.kind r with
  | Response.Body b ->
      Body.to_string b
  | Response.Upgrade _ ->
      fail "an upgrade"

let code r = Status.code (Response.status r)

let field name r = Headers.find name (Response.headers r)

let get ?(fields = []) ?(meth = Method.GET) path =
  Request.make ~headers:(Headers.of_list fields) meth path

let auth v = ("Authorization", v)

(* [with_mw ?schemes h req] is the response of [h] behind the middleware
   (default: the bearer scheme only). *)
let with_mw ?(schemes = [bearer ()]) h req = A.middleware ~schemes () h req

(* [failure_of ?schemes req] is [A.failure] of [req] behind the middleware. *)
let failure_of ?schemes req =
  let f = ref None in
  ignore
    (with_mw ?schemes
       (fun req ->
         f := A.failure req ;
         Response.string "" )
       req ) ;
  !f

let bearer_tests =
  group "The bearer scheme"
    [ test "no credentials: an anonymous request goes on" (fun () ->
          at before (fun _ ->
              let r = with_mw who (get "/") in
              equal string "-" (body r) ;
              equal (option (pair string string)) None (failure_of (get "/")) ) )
    ; test "the schemes run only at the first need, one time" (fun () ->
          let calls = ref 0 in
          let s =
            A.Scheme.make ~name:"Count"
              ~authenticate:(fun _ ->
                incr calls ;
                A.Authenticated (A.User.make "c") )
              ~challenge:(fun ~failure:_ _ -> Response.string "")
          in
          ignore (with_mw ~schemes:[s] (fun _ -> Response.string "") (get "/")) ;
          equal int 0 !calls ;
          ignore
            (with_mw ~schemes:[s]
               (fun req ->
                 ignore (A.user req) ;
                 ignore (A.failure req) ;
                 ignore (A.user req) ;
                 Response.string "" )
               (get "/") ) ;
          equal int 1 !calls )
    ; test "a good token gives the user" (fun () ->
          at before (fun _ ->
              let seen = ref None in
              let r =
                with_mw
                  (fun req ->
                    seen := A.user req ;
                    who req )
                  (get ~fields:[auth ("Bearer " ^ user_token ())] "/")
              in
              equal string "ada Bearer admin,dev" (body r) ;
              let u = Option.get !seen in
              equal (option string) (Some "ada") (A.User.claim "sub" u) ;
              equal (option string) (Some "read write") (A.User.claim "scope" u) ;
              (* Only the string claims. *)
              equal (option string) None (A.User.claim "n" u) ;
              is_true ~msg:"claims" (List.mem_assoc "sub" (A.User.claims u)) ) )
    ; test "the form of the field" (fun () ->
          at before (fun _ ->
              let t = user_token () in
              let run v =
                let failure = ref None in
                let r =
                  with_mw
                    (fun req ->
                      failure := A.failure req ;
                      who req )
                    (get ~fields:[auth v] "/")
                in
                (body r, Option.map snd !failure)
              in
              let outcome = Testable.(pair string (option string)) in
              let malformed = Some "a malformed Authorization field" in
              List.iter
                (fun (v, expected) -> equal ~msg:v outcome expected (run v))
                [ ("bearer " ^ t, ("ada Bearer admin,dev", None))
                ; ("BEARER  " ^ t, ("ada Bearer admin,dev", None))
                ; ("Basic YTpi", ("-", None))
                ; ("Digest x", ("-", None))
                ; ("Pearer " ^ t, ("-", None))
                ; ("Bearerx", ("-", None))
                ; ("Bearerx " ^ t, ("-", None))
                ; ("", ("-", None))
                ; ("Bearer", ("-", malformed))
                ; ("Bearer\t" ^ t, ("-", malformed))
                ; ("Bearer " ^ t ^ " x", ("-", malformed))
                ; ("Bearer ," ^ t, ("-", malformed))
                ; ("Bearer =", ("-", malformed))
                ; ("Bearer a=b", ("-", malformed))
                ; ("Bearer a==", ("-", Some {|Malformed "not three parts"|}))
                ; ( "Bearer a-._~+/b"
                  , ("-", Some {|Malformed "not three parts"|}) ) ] ) )
    ; test "two Authorization fields" (fun () ->
          at before (fun _ ->
              let req =
                get
                  ~fields:[auth ("Bearer " ^ user_token ()); auth "Basic x"]
                  "/"
              in
              let failure = ref None in
              ignore
                (with_mw
                   (fun req ->
                     failure := A.failure req ;
                     Response.string "" )
                   req ) ;
              equal
                (option (pair string string))
                (Some ("Bearer", "two Authorization fields"))
                !failure ;
              let r = with_mw (A.require () who) req in
              equal int 401 (code r) ;
              equal (option string)
                (Some {|Bearer realm="api", error="invalid_request"|})
                (field "www-authenticate" r) ) )
    ; test "a bad token: 401 with invalid_token" (fun () ->
          at before (fun _ ->
              List.iter
                (fun (label, t, m) ->
                  let req = get ~fields:[auth ("Bearer " ^ t)] "/" in
                  let failure = ref None in
                  let r =
                    with_mw
                      (fun req ->
                        failure := A.failure req ;
                        A.require () who req )
                      req
                  in
                  equal ~msg:label int 401 (code r) ;
                  equal ~msg:label (option string)
                    (Some {|Bearer realm="api", error="invalid_token"|})
                    (field "www-authenticate" r) ;
                  equal ~msg:label (option string) (Some m)
                    (Option.map snd !failure) ;
                  equal ~msg:label string "Unauthorized\n" (body r) )
                [ ( "no sub"
                  , hs_token ~payload:{|{"exp":1300819380}|} ()
                  , "no sub" )
                ; ( "expired"
                  , hs_token ~payload:{|{"exp":1,"sub":"a"}|} ()
                  , "Expired" )
                ; ( "bad signature"
                  , token ~header:(alg_header "HS256")
                      (hs ~secret:(String.make 32 'z') ())
                      user_payload
                  , "Bad_signature" ) ] ) )
    ; test "the challenge with no credentials" (fun () ->
          at before (fun _ ->
              let r = with_mw (A.require () who) (get "/") in
              equal int 401 (code r) ;
              equal (option string) (Some {|Bearer realm="api"|})
                (field "www-authenticate" r) ;
              let r =
                with_mw
                  ~schemes:[bearer ~realm:"orders" ()]
                  (A.require () who) (get "/")
              in
              equal (option string) (Some {|Bearer realm="orders"|})
                (field "www-authenticate" r) ;
              equal string "Unauthorized\n" (body r) ) )
    ; test "challenge and forbid in a handler" (fun () ->
          at before (fun _ ->
              let r = with_mw A.challenge (get "/") in
              equal int 401 (code r) ;
              equal (option string) (Some {|Bearer realm="api"|})
                (field "www-authenticate" r) ;
              let r =
                with_mw A.challenge (get ~fields:[auth "Bearer x.y.z"] "/")
              in
              equal (option string)
                (Some {|Bearer realm="api", error="invalid_token"|})
                (field "www-authenticate" r) ;
              let r = A.forbid (get "/") in
              equal int 403 (code r) ;
              equal string "Forbidden\n" (body r) ) )
    ; test "a custom user mapping" (fun () ->
          at before (fun _ ->
              let user c =
                match A.Jwt.string "iss" c with
                | Some iss ->
                    Ok
                      (A.User.make ~roles:["r"]
                         (iss ^ "|" ^ Option.get (A.Jwt.subject c)) )
                | None ->
                    Error "no iss"
              in
              let t =
                hs_token ~payload:{|{"exp":1300819380,"sub":"a","iss":"i"}|} ()
              in
              let r =
                with_mw
                  ~schemes:[bearer ~user ()]
                  who
                  (get ~fields:[auth ("Bearer " ^ t)] "/")
              in
              equal string "i|a Bearer r" (body r) ;
              equal
                (option (pair string string))
                (Some ("Bearer", "no iss"))
                (failure_of
                   ~schemes:[bearer ~user ()]
                   (get ~fields:[auth ("Bearer " ^ user_token ())] "/") ) ;
              (* A text of the application with the letters of a field failure
                 is not a field failure. *)
              let user _ = Error "two Authorization fields" in
              let r =
                with_mw
                  ~schemes:[bearer ~user ()]
                  (A.require () who)
                  (get ~fields:[auth ("Bearer " ^ user_token ())] "/")
              in
              equal (option string)
                (Some {|Bearer realm="api", error="invalid_token"|})
                (field "www-authenticate" r) ) )
    ; test "the claim of the roles, and an empty sub" (fun () ->
          at before (fun _ ->
              let r =
                with_mw
                  ~schemes:
                    [ A.Scheme.bearer ~roles_claim:"scope" ~issuer:Any
                        ~audience:Any
                        ~keys:(fun () -> [hs_key])
                        () ]
                  who
                  (get ~fields:[auth ("Bearer " ^ user_token ())] "/")
              in
              equal string "ada Bearer " (body r) ;
              (* A string, not a list: no roles. *)
              let t =
                hs_token
                  ~payload:{|{"exp":1300819380,"sub":"a","roles":"admin"}|} ()
              in
              equal string "a Bearer "
                (body (with_mw who (get ~fields:[auth ("Bearer " ^ t)] "/"))) ;
              let t = hs_token ~payload:{|{"exp":1300819380,"sub":""}|} () in
              equal
                (option (pair string string))
                (Some ("Bearer", "no sub"))
                (failure_of (get ~fields:[auth ("Bearer " ^ t)] "/")) ) )
    ; test "keys is called for each request" (fun () ->
          at before (fun _ ->
              let keys = ref [] in
              let h =
                with_mw ~schemes:[bearer ~keys:(fun () -> !keys) ()] who
              in
              let req = get ~fields:[auth ("Bearer " ^ user_token ())] "/" in
              equal string "-" (body (h req)) ;
              keys := [hs_key] ;
              equal string "ada Bearer admin,dev" (body (h req)) ) ) ]

let policy_tests =
  group "Policies"
    [ test "require and the policies" (fun () ->
          at before (fun _ ->
              let req = get ~fields:[auth ("Bearer " ^ user_token ())] "/" in
              let status policy =
                code (with_mw (A.require ~policy () who) req)
              in
              List.iter
                (fun (label, policy, expected) ->
                  equal ~msg:label int expected (status policy) )
                [ ("authenticated", A.authenticated, 200)
                ; ("role admin", A.role "admin", 200)
                ; ("role Admin", A.role "Admin", 403)
                ; ("role x", A.role "x", 403)
                ; ("scope write", A.scope "write", 200)
                ; ("scope rea", A.scope "rea", 403)
                ; ("all", A.all [A.role "admin"; A.role "dev"], 200)
                ; ("all x", A.all [A.role "admin"; A.role "x"], 403)
                ; ("all []", A.all [], 200)
                ; ("any", A.any [A.role "x"; A.role "dev"], 200)
                ; ("any x", A.any [A.role "x"; A.role "y"], 403)
                ; ("any []", A.any [], 403) ] ;
              let r = with_mw (A.require ~policy:(A.role "x") () who) req in
              equal string "Forbidden\n" (body r) ;
              is_false ~msg:"scope of no claim"
                (A.scope "read" (A.User.make "u")) ;
              is_false ~msg:"has_role"
                (A.User.has_role "admin" (A.User.make "u")) ) )
    ; test "require marks a response private" (fun () ->
          at before (fun _ ->
              let req = get ~fields:[auth ("Bearer " ^ user_token ())] "/" in
              let r = with_mw (A.require () who) req in
              equal (option string) (Some "private") (field "cache-control" r) ;
              let own _ =
                Response.string
                  ~headers:(Headers.of_list [("Cache-Control", "no-store")])
                  ""
              in
              let r = with_mw (A.require () own) req in
              equal (list string) ["no-store"]
                (Headers.find_all "cache-control" (Response.headers r)) ;
              (* No user, or a refused one: no handler, no field. *)
              let r = with_mw (A.require () who) (get "/") in
              equal (option string) None (field "cache-control" r) ) )
    ; test "with no middleware, the functions raise" (fun () ->
          let raises f =
            raises_match (function Invalid_argument _ -> true | _ -> false) f
          in
          raises (fun () -> ignore (A.require () who (get "/"))) ;
          raises (fun () -> ignore (A.user (get "/"))) ;
          raises (fun () -> ignore (A.failure (get "/"))) ;
          raises (fun () -> ignore (A.challenge (get "/"))) )
    ; test "bad setups raise" (fun () ->
          let raises label f =
            raises_match ~msg:label
              (function Invalid_argument _ -> true | _ -> false)
              f
          in
          let made (_ : Handler.middleware) = () in
          raises "no scheme" (fun () -> made (A.middleware ~schemes:[] ())) ;
          raises "two names" (fun () ->
              made (A.middleware ~schemes:[bearer (); bearer ()] ()) ) ;
          List.iter
            (fun name ->
              raises ("name " ^ name) (fun () ->
                  ignore
                    (A.Scheme.make ~name
                       ~authenticate:(fun _ -> A.No_credentials)
                       ~challenge:(fun ~failure:_ _ -> Response.string "") ) ) )
            ["Bearer"; "Session"] ;
          raises "empty name" (fun () ->
              ignore
                (A.Scheme.make ~name:""
                   ~authenticate:(fun _ -> A.No_credentials)
                   ~challenge:(fun ~failure:_ _ -> Response.string "") ) ) ;
          List.iter
            (fun realm ->
              raises
                ("realm " ^ String.escaped realm)
                (fun () -> ignore (bearer ~realm ())) )
            ["a\"b"; "a\\b"; "a\nb"; "a\x7fb"] ;
          raises "issuer" (fun () ->
              ignore
                (A.Scheme.bearer ~issuer:(One_of []) ~audience:Any
                   ~keys:(fun () -> [])
                   () ) ) ;
          raises "audience" (fun () ->
              ignore
                (A.Scheme.bearer ~issuer:Any ~audience:(One_of [])
                   ~keys:(fun () -> [])
                   () ) ) ;
          raises "leeway" (fun () ->
              ignore
                (A.Scheme.bearer ~leeway:(Span.ns (-1)) ~issuer:Any
                   ~audience:Any
                   ~keys:(fun () -> [])
                   () ) ) ;
          ignore
            (A.Scheme.bearer ~leeway:Span.zero ~realm:"a b" ~issuer:Any
               ~audience:Any
               ~keys:(fun () -> [])
               () ) ;
          List.iter
            (fun login ->
              raises
                ("login " ^ String.escaped login)
                (fun () -> ignore (A.Scheme.session ~login ())) )
            ["x"; "//x"; "/a?b"; "/\\x"; "/a\\b"; "/a\tb"; ""] ;
          ignore (A.Scheme.session ~login:"/" ()) ;
          ignore (A.Scheme.session ~login:"/login" ()) )
    ; test "a scheme of the application" (fun () ->
          let s =
            A.Scheme.make ~name:"Key"
              ~authenticate:(fun req ->
                match Headers.find "x-key" (Request.headers req) with
                | Some "good" ->
                    A.Authenticated (A.User.make "k")
                | Some _ ->
                    A.Failed "bad key"
                | None ->
                    A.No_credentials )
              ~challenge:(fun ~failure _ ->
                Response.string ~status:Status.unauthorized
                  (Option.value ~default:"none" failure) )
          in
          equal string "Key" (A.Scheme.name s) ;
          let h = with_mw ~schemes:[s] (A.require () who) in
          equal string "k Key " (body (h (get ~fields:[("X-Key", "good")] "/"))) ;
          equal string "bad key" (body (h (get ~fields:[("X-Key", "x")] "/"))) ;
          equal string "none" (body (h (get "/"))) ;
          equal string "" (A.User.scheme (A.User.make "u")) )
    ; test "require ~challenge chooses the scheme of the challenge" (fun () ->
          at before (fun _ ->
              let schemes = [A.Scheme.session ~login:"/login" (); bearer ()] in
              let nav = [("Sec-Fetch-Mode", "navigate")] in
              let h ?challenge req =
                S.middleware
                  ~keys:[S.Key.of_string (String.make 32 's')]
                  ~store:(S.memory_store ()) ()
                  (with_mw ~schemes (A.require ?challenge () who))
                  req
              in
              equal int 303 (code (h (get ~fields:nav "/"))) ;
              let r = h ~challenge:"Bearer" (get ~fields:nav "/") in
              equal int 401 (code r) ;
              equal (option string) (Some {|Bearer realm="api"|})
                (field "www-authenticate" r) ;
              (* A failure has its own challenge. *)
              let r =
                h ~challenge:"Session"
                  (get ~fields:(auth "Bearer x.y.z" :: nav) "/")
              in
              equal int 401 (code r) ;
              raises_match
                (function Invalid_argument _ -> true | _ -> false)
                (fun () -> ignore (h ~challenge:"Key" (get "/"))) ;
              (* Also for a request with a user. *)
              raises_match
                (function Invalid_argument _ -> true | _ -> false)
                (fun () ->
                  ignore
                    (h ~challenge:"Key"
                       (get ~fields:[auth ("Bearer " ^ user_token ())] "/") ) ) ) )
    ; test "with the router, a 404 gets no challenge" (fun () ->
          at before (fun _ ->
              let app =
                with_mw
                  (Caravan_router.handler
                     [ Caravan_router.group
                         ~middleware:[A.require ~policy:(A.role "admin") ()]
                         "/admin"
                         [Caravan_router.get "/x" who] ] )
              in
              equal int 401 (code (app (get "/admin/x"))) ;
              equal int 404 (code (app (get "/admin/y"))) ;
              equal int 200
                (code
                   (app
                      (get
                         ~fields:[auth ("Bearer " ^ user_token ())]
                         "/admin/x" ) ) ) ) ) ]

(* {1 The session scheme} *)

let session_key = S.Key.of_string (String.make 32 's')

let cookie_name = "__Host-session"

let cookie_of r =
  List.find_map
    (fun f ->
      match Cookie.parse_set_cookie f with
      | Some (n, v) when n = cookie_name ->
          Some v
      | _ ->
          None )
    (Headers.find_all "set-cookie" (Response.headers r))

let with_cookie ?(fields = []) cookie =
  match cookie with
  | Some c ->
      ("Cookie", cookie_name ^ "=" ^ c) :: fields
  | None ->
      fields

(* [session_app ?schemes ?login store h] is the session middleware, then the
   auth middleware with the session scheme, then [h]. *)
let session_app ?schemes ?login ?validate store h =
  let schemes =
    Option.value schemes ~default:[A.Scheme.session ?login ?validate ()]
  in
  S.middleware ~keys:[session_key] ~store () (A.middleware ~schemes () h)

(* A handler: /login signs in "ada", /logout signs out, /data sets "cart",
   others tell the user and the cart. *)
let site req =
  match Request.path_only req with
  | "/login" ->
      A.sign_in req
        (A.User.make ~roles:["admin"] ~claims:[("mail", "a@x")] "ada") ;
      Response.string "in"
  | "/login-bob" ->
      A.sign_in req (A.User.make "bob") ;
      Response.string "in"
  | "/logout" ->
      A.sign_out req ; Response.string "out"
  | "/data" ->
      S.set "cart" "3" req ; Response.string "data"
  | "/token" ->
      Response.string (S.Csrf.token req)
  | _ ->
      let u =
        match A.user req with
        | Some u ->
            A.User.id u ^ " " ^ A.User.scheme u
        | None ->
            "-"
      in
      Response.string (u ^ " " ^ Option.value ~default:"-" (S.find "cart" req))

let session_tests =
  group "The session scheme"
    [ test "sign in, a later request, sign out" (fun () ->
          at before (fun _ ->
              let store = S.memory_store () in
              let h = session_app store site in
              let send ?cookie path =
                h (get ~fields:(with_cookie cookie) path)
              in
              let r = send "/data" in
              let anon = cookie_of r in
              equal string "- 3" (body (send ?cookie:anon "/")) ;
              let r = send ?cookie:anon "/login" in
              let c = cookie_of r in
              is_true ~msg:"a new ID" (c <> anon && c <> None) ;
              (* The anonymous data stays after the first sign in. *)
              equal string "ada Session 3" (body (send ?cookie:c "/")) ;
              (* The old ID is no session. *)
              equal string "- -" (body (send ?cookie:anon "/")) ;
              let r = send ?cookie:c "/logout" in
              equal string "out" (body r) ;
              equal string "- -" (body (send ?cookie:c "/")) ) )
    ; test "a second sign in ends the old session" (fun () ->
          at before (fun _ ->
              let store = S.memory_store () in
              let h = session_app store site in
              let send ?cookie path =
                h (get ~fields:(with_cookie cookie) path)
              in
              let c = cookie_of (send "/login") in
              (* /data sets no new cookie: the session exists. *)
              is_none (cookie_of (send ?cookie:c "/data")) ;
              equal string "ada Session 3" (body (send ?cookie:c "/")) ;
              let c2 = cookie_of (send ?cookie:c "/login-bob") in
              is_true ~msg:"a new ID" (c2 <> c) ;
              equal string "bob Session -" (body (send ?cookie:c2 "/")) ;
              equal string "- -" (body (send ?cookie:c "/")) ) )
    ; test "the user keeps its roles and claims, not a scheme" (fun () ->
          at before (fun _ ->
              let store = S.memory_store () in
              let seen = ref None in
              let h =
                S.middleware ~keys:[session_key] ~store ()
                  (A.middleware
                     ~schemes:[bearer (); A.Scheme.session ()]
                     ()
                     (fun req ->
                       match Request.path_only req with
                       | "/login" ->
                           A.sign_in req (Option.get (A.user req)) ;
                           Response.string ""
                       | _ ->
                           seen := A.user req ;
                           Response.string "" ) )
              in
              let r =
                h (get ~fields:[auth ("Bearer " ^ user_token ())] "/login")
              in
              let c = cookie_of r in
              ignore (h (get ~fields:(with_cookie c) "/")) ;
              let u = Option.get !seen in
              equal string "ada" (A.User.id u) ;
              equal string "Session" (A.User.scheme u) ;
              equal (list string) ["admin"; "dev"] (A.User.roles u) ;
              equal (option string) (Some "read write") (A.User.claim "scope" u) ) )
    ; test "the order of the schemes" (fun () ->
          at before (fun _ ->
              let store = S.memory_store () in
              let schemes = [bearer (); A.Scheme.session ()] in
              let h = session_app ~schemes store site in
              let c = cookie_of (h (get "/login")) in
              (* No Authorization: the session. *)
              equal string "ada Session -"
                (body (h (get ~fields:(with_cookie c) "/"))) ;
              (* A bad token stops before the session. *)
              let bad = with_cookie ~fields:[auth "Bearer x.y.z"] c in
              equal string "- -" (body (h (get ~fields:bad "/"))) ;
              (* The challenge is of the scheme that failed. *)
              let h =
                session_app
                  ~schemes:[A.Scheme.session (); bearer ()]
                  store (A.require () site)
              in
              let r = h (get ~fields:[auth "Bearer x.y.z"] "/") in
              equal int 401 (code r) ;
              equal (option string)
                (Some {|Bearer realm="api", error="invalid_token"|})
                (field "www-authenticate" r) ;
              (* With no failure: the first scheme. *)
              let r = h (get "/") in
              equal int 401 (code r) ;
              equal (option string) None (field "www-authenticate" r) ;
              (* The session first: its user wins over a bad token. *)
              let h =
                session_app ~schemes:[A.Scheme.session (); bearer ()] store site
              in
              equal string "ada Session -"
                (body
                   (h
                      (get
                         ~fields:(with_cookie ~fields:[auth "Bearer x.y.z"] c)
                         "/" ) ) ) ) )
    ; test "session data that does not decode" (fun () ->
          at before (fun _ ->
              let store = S.memory_store () in
              List.iter
                (fun text ->
                  let failure = ref None in
                  let h =
                    S.middleware ~keys:[session_key] ~store () (fun req ->
                        match Request.path_only req with
                        | "/plant" ->
                            S.Private.set "caravan.auth.user" text req ;
                            Response.string ""
                        | _ ->
                            A.middleware
                              ~schemes:[A.Scheme.session ()]
                              ()
                              (fun req ->
                                failure := A.failure req ;
                                who req )
                              req )
                  in
                  let c = cookie_of (h (get "/plant")) in
                  equal ~msg:(String.escaped text) string "-"
                    (body (h (get ~fields:(with_cookie c) "/"))) ;
                  equal ~msg:(String.escaped text)
                    (option (pair string string))
                    (Some ("Session", "bad session data"))
                    !failure )
                [ (* No version, another version. *)
                  ""
                ; "2" ^ "1:a1:0" (* A field with no colon, no count. *)
                ; "1" ^ "1:a1:0x"
                ; "1" ^ "1:a"
                  (* A length with a leading zero, too long, empty, not digits,
                     too large for an int. *)
                ; "1" ^ "01:a1:0"
                ; "1" ^ "9:a"
                ; "1" ^ ":a"
                ; "1" ^ "1xa1:0"
                ; "1" ^ "11"
                ; "1" ^ "99999999999999999999:a"
                ; "1" ^ "1:a1:" (* A count that is not canonical, too large. *)
                ; "1" ^ "1:a2:-1"
                ; "1" ^ "1:a1:x"
                ; "1" ^ "1:a2:+0"
                ; "1" ^ "1:a2:00"
                ; "1" ^ "1:a3:0x0"
                ; "1" ^ "1:a3:0_0"
                ; "1" ^ "1:a1:2" (* A claim with no value. *)
                ; "1" ^ "1:a1:01:k"
                ; "1" ^ "1:a1:01:k1:v1:x" ] ) )
    ; test "the encoding of an empty user" (fun () ->
          at before (fun _ ->
              let store = S.memory_store () in
              let seen = ref None in
              let h =
                session_app store (fun req ->
                    ( match Request.path_only req with
                    | "/login" ->
                        A.sign_in req (A.User.make "")
                    | _ ->
                        seen := A.user req ) ;
                    Response.string "" )
              in
              let c = cookie_of (h (get "/login")) in
              ignore (h (get ~fields:(with_cookie c) "/")) ;
              let u = Option.get !seen in
              equal string "" (A.User.id u) ;
              equal (list string) [] (A.User.roles u) ;
              equal (list (pair string string)) [] (A.User.claims u) ) )
    ; test "the encoding of a user" (fun () ->
          at before (fun _ ->
              let store = S.memory_store () in
              let seen = ref None in
              let u =
                A.User.make
                  ~roles:["a:b"; ""; "1:x"; "123456789"]
                  ~claims:[("", ""); ("k\n", "v:1"); ("10", "0")]
                  ("id:12\n" ^ String.make 10000 'x')
              in
              let h =
                S.middleware ~keys:[session_key] ~store ()
                  (A.middleware
                     ~schemes:[A.Scheme.session ()]
                     ()
                     (fun req ->
                       ( match Request.path_only req with
                       | "/login" ->
                           A.sign_in req u
                       | _ ->
                           seen := A.user req ) ;
                       Response.string "" ) )
              in
              let c = cookie_of (h (get "/login")) in
              ignore (h (get ~fields:(with_cookie c) "/")) ;
              let v = Option.get !seen in
              equal string ("id:12\n" ^ String.make 10000 'x') (A.User.id v) ;
              equal (list string)
                ["a:b"; ""; "1:x"; "123456789"]
                (A.User.roles v) ;
              equal
                (list (pair string string))
                [("", ""); ("k\n", "v:1"); ("10", "0")]
                (A.User.claims v) ) )
    ; test "validate" (fun () ->
          at before (fun _ ->
              let store = S.memory_store () in
              let failure = ref None in
              let validate u =
                if A.User.id u = "ada" then
                  Some (A.User.make ~roles:["new"] "ada2")
                else None
              in
              let h =
                session_app ~validate store (fun req ->
                    failure := A.failure req ;
                    match Request.path_only req with
                    | "/login" | "/login-bob" ->
                        site req
                    | _ ->
                        who req )
              in
              let c = cookie_of (h (get "/login")) in
              equal string "ada2 Session new"
                (body (h (get ~fields:(with_cookie c) "/"))) ;
              let c = cookie_of (h (get "/login-bob")) in
              equal string "-" (body (h (get ~fields:(with_cookie c) "/"))) ;
              equal
                (option (pair string string))
                (Some ("Session", "the user is not valid"))
                !failure ) )
    ; test "the challenge of the session scheme" (fun () ->
          at before (fun _ ->
              let store = S.memory_store () in
              let h = session_app ~login:"/login" store (A.require () site) in
              let send ?(meth = Method.GET) fields path =
                h (get ~meth ~fields path)
              in
              let nav = [("Sec-Fetch-Mode", "navigate")] in
              let r = send nav "/a%20b/c?x=1&y=/" in
              equal int 303 (code r) ;
              equal (option string)
                (Some "/login?return=/a%2520b/c%3Fx%3D1%26y%3D/")
                (field "location" r) ;
              equal int 303 (code (send ~meth:Method.HEAD nav "/")) ;
              equal int 303 (code (send [("Accept", "text/html")] "/")) ;
              equal int 303
                (code
                   (send
                      [("Accept", "application/xhtml+xml,TEXT/HTML;q=0.9")]
                      "/" ) ) ;
              List.iter
                (fun (label, meth, fields) ->
                  let r = send ~meth fields "/" in
                  equal ~msg:label int 401 (code r) ;
                  equal ~msg:label (option string) None (field "location" r) )
                [ ("POST", Method.POST, nav)
                ; ( "cors"
                  , Method.GET
                  , [("Sec-Fetch-Mode", "cors"); ("Accept", "text/html")] )
                ; ("json", Method.GET, [("Accept", "application/json")])
                ; ("text/htm", Method.GET, [("Accept", "text/htm")])
                ; ("xext/html", Method.GET, [("Accept", "xext/html")])
                ; ("no accept", Method.GET, []) ] ;
              (* A path that starts with // stays in the return value, and
                 return_path refuses it. *)
              let r = send nav "//evil.example/x" in
              let location = Option.get (field "location" r) in
              equal string "/login?return=//evil.example/x" location ;
              equal (option string) None (A.return_path (get location)) ;
              (* With no login: 401. *)
              let h = session_app store (A.require () site) in
              equal int 401 (code (h (get ~fields:nav "/"))) ) )
    ; test "the user of a request does not change in the request" (fun () ->
          at before (fun _ ->
              let store = S.memory_store () in
              let seen = ref [] in
              let h =
                session_app store (fun req ->
                    ( match Request.path_only req with
                    | "/login" ->
                        A.sign_in req (A.User.make "ada")
                    | "/logout" ->
                        A.sign_out req
                    | _ ->
                        () ) ;
                    seen := Option.map A.User.id (A.user req) :: !seen ;
                    Response.string "" )
              in
              let c = cookie_of (h (get "/login")) in
              ignore (h (get ~fields:(with_cookie c) "/logout")) ;
              equal (list (option string)) [Some "ada"; None] !seen ) )
    ; test "sign_in with bad data ends the old session" (fun () ->
          at before (fun _ ->
              let store = S.memory_store () in
              let h =
                S.middleware ~keys:[session_key] ~store ()
                  (A.middleware
                     ~schemes:[A.Scheme.session ()]
                     ()
                     (fun req ->
                       match Request.path_only req with
                       | "/plant" ->
                           S.Private.set "caravan.auth.user" "bad" req ;
                           S.set "cart" "3" req ;
                           Response.string ""
                       | _ ->
                           site req ) )
              in
              let send ?cookie path =
                h (get ~fields:(with_cookie cookie) path)
              in
              let old = cookie_of (send "/plant") in
              let c = cookie_of (send ?cookie:old "/login") in
              equal string "ada Session -" (body (send ?cookie:c "/")) ;
              equal string "- -" (body (send ?cookie:old "/")) ) )
    ; test "sign_in after the response raises" (fun () ->
          at before (fun _ ->
              let store = S.memory_store () in
              let raised = ref false in
              let r =
                session_app store
                  (fun req ->
                    Response.stream (fun _ ->
                        try A.sign_in req (A.User.make "a")
                        with Invalid_argument _ -> raised := true ) )
                  (get "/")
              in
              ignore (body r) ;
              is_true !raised ) )
    ; test "return_path" (fun () ->
          let rp q = A.return_path (get ("/login" ^ q)) in
          List.iter
            (fun (q, expected) -> equal ~msg:q (option string) expected (rp q))
            [ ("?return=/a/b", Some "/a/b")
            ; ("?return=/", Some "/")
            ; ("?return=/a%20b", Some "/a b")
            ; ("?return=%2Fa%3Fx%3D1", Some "/a?x=1")
            ; ("?return=//evil.example", None)
            ; ("?return=/%5Cevil.example", None)
            ; ("?return=/a%5Cb", None)
            ; ("?return=/%09/evil.example", None)
            ; ("?return=/a%7F", None)
            ; ("?return=https://evil.example", None)
            ; ("?return=a", None)
            ; ("?return=", None)
            ; ("", None) ] )
    ; test "CSRF: a cookie POST needs a token, a bearer POST does not"
        (fun () ->
          at before (fun _ ->
              let store = S.memory_store () in
              let app =
                Handler.compose
                  [ Caravan_middleware.Cross_origin.middleware ()
                  ; S.middleware ~keys:[session_key] ~store ()
                  ; A.middleware ~schemes:[bearer (); A.Scheme.session ()] ()
                  ; S.Csrf.middleware
                      ~exempt:(fun r ->
                        match A.user r with
                        | Some u ->
                            A.User.scheme u = "Bearer"
                        | None ->
                            false )
                      () ]
                  (fun req ->
                    (* The login route is not under require. *)
                    if Request.path_only req = "/login" then site req
                    else A.require () site req )
              in
              let c = cookie_of (app (get "/login")) in
              is_some ~msg:"a session cookie" c ;
              (* A form POST with the cookie and no token. *)
              let r =
                app (get ~meth:Method.POST ~fields:(with_cookie c) "/x")
              in
              equal int 403 (code r) ;
              (* The same POST with a token of the session. *)
              let token = body (app (get ~fields:(with_cookie c) "/token")) in
              let r =
                app
                  (get ~meth:Method.POST
                     ~fields:(with_cookie ~fields:[("X-CSRF-Token", token)] c)
                     "/x" )
              in
              equal int 200 (code r) ;
              (* A POST with a bearer token and no session. *)
              let r =
                app
                  (get ~meth:Method.POST
                     ~fields:[auth ("Bearer " ^ user_token ())]
                     "/x" )
              in
              equal int 200 (code r) ) ) ]

(* {1 OpenID Connect} *)

module O = Caravan_auth.Oidc
module P = Caravan_auth.Oidc.Provider
module C = Caravan_client
module T = Caravan_testing

(* The fake provider serves its documents over TLS on the test host
   (127.0.0.1:80), with the name idp.example. *)
let issuer = "https://idp.example:80"

let oidc_t0 = 1_750_000_000.

let minute_span = Span.s 60

(* The state of the fake provider. *)
type idp =
  { mutable document: string
  ; mutable document_status: int
  ; mutable jwks: string
  ; mutable jwks_status: int
  ; mutable cache_control: string option
  ; mutable gate: unit Ivar.t option  (** A load of /jwks waits for it. *)
  ; mutable paths: string list
        (** The paths of the requests, the last first. *)
  ; mutable accepts: string list  (** The Accept fields, the last first. *)
  ; loads: int Atomic.t
  ; mutable codes: (string * grant) list  (** The codes that it gave. *)
  ; mutable token_calls: (string option * Caravan.Form.t) list
        (** The Authorization field and the form of each token request, the last
            first. *)
  ; mutable token_reply: (int * string) option
        (** A status and a body in place of the normal token response. *)
  ; mutable nonce: string option  (** A nonce in place of that of the login. *)
  }

(* What a code is for. *)
and grant =
  {g_nonce: string; challenge: string; client_id: string; redirect_uri: string}

(* [json_object ~defaults members] is a JSON object of [defaults], with
   [members] in place of those of the same name. A value [""] removes the
   member. The values are JSON texts. *)
let json_object ~defaults members =
  let all =
    List.filter (fun (n, _) -> not (List.mem_assoc n members)) defaults
    @ members
    |> List.filter (fun (_, v) -> v <> "")
  in
  "{"
  ^ String.concat "," (List.map (fun (n, v) -> Printf.sprintf "%S:%s" n v) all)
  ^ "}"

(* [doc members] is a discovery document (see [json_object]). *)
let doc members =
  json_object
    ~defaults:
      [ ("issuer", Printf.sprintf "%S" issuer)
      ; ("authorization_endpoint", Printf.sprintf "%S" (issuer ^ "/authorize"))
      ; ("token_endpoint", Printf.sprintf "%S" (issuer ^ "/token"))
      ; ("jwks_uri", Printf.sprintf "%S" (issuer ^ "/jwks"))
      ; ("response_types_supported", {|["code"]|})
      ; ("code_challenge_methods_supported", {|["S256"]|}) ]
    members

(* A JWK with a kid. *)
let with_kid kid jwk =
  Printf.sprintf {|{"kid":"%s",|} kid ^ String.sub jwk 1 (String.length jwk - 1)

let new_idp () =
  { document= doc []
  ; document_status= 200
  ; jwks= jwks [with_kid "ec1" jwk_ec; with_kid "rs1" jwk_rsa]
  ; jwks_status= 200
  ; cache_control= None
  ; gate= None
  ; paths= []
  ; accepts= []
  ; loads= Atomic.make 0
  ; codes= []
  ; token_calls= []
  ; token_reply= None
  ; nonce= None }

(* An ID token of the fake provider (see [json_object] for [members]). *)
let id_token ?(kid = "ec1") ?(alg = "ES256") ?(sign = es) ?(typ = "JWT") members
    =
  let payload =
    json_object
      ~defaults:
        [ ("iss", Printf.sprintf "%S" issuer)
        ; ("aud", {|"app"|})
        ; ("sub", {|"ada"|})
        ; ("exp", Printf.sprintf "%.0f" (oidc_t0 +. 1_000_000.))
        ; ("iat", Printf.sprintf "%.0f" oidc_t0) ]
      members
  in
  token
    ~header:(Printf.sprintf {|{"alg":"%s","kid":"%s","typ":"%s"}|} alg kid typ)
    sign payload

let idp_handler idp req =
  let path = Caravan.Request.path_only req in
  idp.paths <- path :: idp.paths ;
  idp.accepts <-
    Option.value ~default:"none"
      (Caravan.Headers.find "accept" (Caravan.Request.headers req))
    :: idp.accepts ;
  match path with
  | "/.well-known/openid-configuration" ->
      Caravan.Response.string
        ~status:(Caravan.Status.v idp.document_status)
        idp.document
  | "/jwks" ->
      Atomic.incr idp.loads ;
      Option.iter (fun g -> sync (Ivar.read g)) idp.gate ;
      let headers =
        Caravan.Headers.of_list
          (Option.fold ~none:[]
             ~some:(fun v -> [("Cache-Control", v)])
             idp.cache_control )
      in
      Caravan.Response.string ~headers
        ~status:(Caravan.Status.v idp.jwks_status)
        idp.jwks
  | "/token" -> (
      let form = Result.get_ok (Caravan.Request.form req) in
      let authorization =
        Caravan.Headers.find "authorization" (Caravan.Request.headers req)
      in
      idp.token_calls <- (authorization, form) :: idp.token_calls ;
      let f n = Option.value ~default:"" (Caravan.Form.find n form) in
      match idp.token_reply with
      | Some (status, body) ->
          Caravan.Response.string ~status:(Caravan.Status.v status) body
      | None -> (
        match List.assoc_opt (f "code") idp.codes with
        | Some g
          when f "grant_type" = "authorization_code"
               && f "redirect_uri" = g.redirect_uri
               && b64 (sha256 (f "code_verifier")) = g.challenge ->
            let nonce = Option.value idp.nonce ~default:g.g_nonce in
            let t =
              id_token
                [ ("nonce", Printf.sprintf "%S" nonce)
                ; ("aud", Printf.sprintf "%S" g.client_id) ]
            in
            Caravan.Response.string
              (Printf.sprintf
                 {|{"access_token":"at","token_type":"Bearer","expires_in":3600,"id_token":"%s"}|}
                 t )
        | _ ->
            Caravan.Response.string ~status:Caravan.Status.bad_request
              {|{"error":"invalid_grant"}|} ) )
  | _ ->
      Caravan.Response.string ~status:Caravan.Status.not_found ""

(* [with_idp ?idp f] runs [f t http idp] with the fake provider and a client
   with HTTPS, at [oidc_t0]. The guard is 3 days of virtual time. *)
let with_idp ?(idp = new_idp ()) ?app f =
  let scfg, ccfg =
    Tls_support.configs ~client_peer:false ~names:["idp.example"]
      ~time:(fun () -> Ptime.of_float_s (Time.posix_to_unix_s (Time.now Posix)))
      ()
  in
  T.run
    ~guard:(Span.s (3 * 86400))
    ~engine:(Caravan_tls.engine scfg Caravan.Server.http1)
    (fun req ->
      match app with
      | Some app
        when not
               (List.mem
                  (Caravan.Request.path_only req)
                  [ "/.well-known/openid-configuration"
                  ; "/jwks"
                  ; "/token"
                  ; "/authorize" ] ) ->
          app req
      | _ ->
          idp_handler idp req )
    (fun t ->
      Weft_sim.set_posix (T.sim t) (Time.posix_of_unix_s oidc_t0) ;
      Weft_sim.set_dns (T.sim t) "idp.example" (Addresses ["127.0.0.1"]) ;
      C.run ~tls:(Caravan_tls.https ccfg) (fun http -> f t http idp) )

let discover http = Result.get_ok (P.discover http issuer)

let oidc_result =
  Testable.make
    ~pp:(fun ppf -> function
      | Ok _ -> Format.pp_print_string ppf "Ok" | Error e -> O.pp_error ppf e )
    ~equal:(fun a b ->
      match (a, b) with
      | Ok _, Ok _ ->
          true
      | Error (O.Keys _), Error (O.Keys _) ->
          true
      | Error a, Error b ->
          a = b
      | _ ->
          false )

let verify ?nonce ?issuers p t =
  Result.map ignore (O.verify_id_token ?nonce ?issuers p ~client_id:"app" t)

let keys_error = function
  | Error (O.Keys m) ->
      m
  | Ok _ ->
      "Ok"
  | Error e ->
      Format.asprintf "%a" O.pp_error e

let contains s sub =
  let n = String.length s and m = String.length sub in
  let rec go i = i + m <= n && (String.sub s i m = sub || go (i + 1)) in
  go 0

let oidc_tests =
  group "OpenID Connect"
    [ test "discover gives the endpoints" (fun () ->
          with_idp (fun _ http idp ->
              let p = discover http in
              equal string issuer (P.issuer p) ;
              equal string (issuer ^ "/authorize") (P.authorization_endpoint p) ;
              equal string (issuer ^ "/token") (P.token_endpoint p) ;
              equal (option (list string)) None (P.auth_methods p) ;
              is_false (P.iss_parameter p) ;
              equal int ~msg:"no load of the keys" 0 (Atomic.get idp.loads) ;
              equal ~msg:"keys load at the first need" int 2
                (List.length (P.keys p)) ;
              equal (list string)
                ["application/json"; "application/json"]
                idp.accepts ;
              is_false ~msg:"make: no iss parameter"
                (P.iss_parameter
                   (P.make ~issuer ~authorization_endpoint:(issuer ^ "/a")
                      ~token_endpoint:(issuer ^ "/t") ~jwks_uri:(issuer ^ "/k")
                      http ) ) ;
              idp.document <-
                doc
                  [ ( "token_endpoint_auth_methods_supported"
                    , {|["client_secret_post"]|} )
                  ; ("authorization_response_iss_parameter_supported", "true")
                  ] ;
              let p = discover http in
              equal
                (option (list string))
                (Some ["client_secret_post"]) (P.auth_methods p) ;
              is_true (P.iss_parameter p) ) )
    ; test "discover refuses a bad document" (fun () ->
          with_idp (fun _ http idp ->
              List.iter
                (fun (label, document, status, expected) ->
                  idp.document <- document ;
                  idp.document_status <- status ;
                  match P.discover http issuer with
                  | Ok _ ->
                      fail (label ^ ": Ok")
                  | Error e ->
                      is_true ~msg:(label ^ ": " ^ e) (contains e expected) )
                [ ( "another issuer"
                  , doc [("issuer", Printf.sprintf "%S" (issuer ^ "/"))]
                  , 200
                  , "the issuer of the document is" )
                ; ( "an http endpoint"
                  , doc [("token_endpoint", {|"http://idp.example/token"|})]
                  , 200
                  , "token_endpoint is not an https URL" )
                ; ("no jwks_uri", doc [("jwks_uri", "")], 200, "no jwks_uri")
                ; ( "no code"
                  , doc [("response_types_supported", {|["id_token"]|})]
                  , 200
                  , "response type code" )
                ; ( "no response types"
                  , doc [("response_types_supported", "")]
                  , 200
                  , "response type code" )
                ; ( "no S256"
                  , doc [("code_challenge_methods_supported", {|["plain"]|})]
                  , 200
                  , "S256" )
                ; ("status 404", doc [], 404, "status 404")
                ; ( "too large"
                  , doc [("pad", Printf.sprintf "%S" (String.make 70000 'x'))]
                  , 200
                  , "too large" )
                ; ("not JSON", "{issuer:1}", 200, "the document: not JSON")
                ; ("not an object", "[]", 200, "not an object")
                ; ( "issuer not a string"
                  , doc [("issuer", "1")]
                  , 200
                  , "issuer is not a string" )
                ; ( "a list of numbers"
                  , doc [("response_types_supported", "[1]")]
                  , 200
                  , "not a list of strings" )
                ; ( "not a list"
                  , doc [("response_types_supported", {|"code"|})]
                  , 200
                  , "not a list of strings" )
                ; ( "iss parameter not a boolean"
                  , doc
                      [ ( "authorization_response_iss_parameter_supported"
                        , {|"yes"|} ) ]
                  , 200
                  , "not a boolean" ) ] ;
              idp.document <- doc [] ;
              idp.document_status <- 200 ;
              (* The limit of the caller. *)
              is_true (Result.is_error (P.discover ~max_length:100 http issuer)) ) )
    ; test "discover refuses an issuer that is not https before a request"
        (fun () ->
          with_idp (fun _ http idp ->
              List.iter
                (fun i -> is_true ~msg:i (Result.is_error (P.discover http i)))
                [ "http://idp.example:80"
                ; "https://idp.example:80?x=1"
                ; "https://idp.example:80#f"
                ; "https://"
                ; "https:///p"
                ; "HTTPS://idp example"
                ; "https://idp.example\x7f"
                ; "https://u@idp.example:80"
                ; "https://idp.example:99999"
                ; "https://:80" ] ;
              equal (list string) [] idp.paths ) )
    ; test "discover removes one terminating slash for the URL" (fun () ->
          with_idp (fun _ http idp ->
              idp.document <-
                doc [("issuer", Printf.sprintf "%S" (issuer ^ "/"))] ;
              is_true (Result.is_ok (P.discover http (issuer ^ "/"))) ;
              equal (list string)
                ["/.well-known/openid-configuration"]
                idp.paths ;
              (* An issuer in uppercase scheme is https too. *)
              idp.document <- doc [("issuer", {|"HTTPS://idp.example:80"|})] ;
              is_true (Result.is_ok (P.discover http "HTTPS://idp.example:80")) ) )
    ; test "make and discover refuse bad arguments" (fun () ->
          with_idp (fun _ http _ ->
              let is_invalid = function
                | Invalid_argument _ ->
                    true
                | _ ->
                    false
              in
              let make ?timeout ?(issuer = issuer)
                  ?(authorization = issuer ^ "/a") ?(token = issuer ^ "/token")
                  ?(jwks = issuer ^ "/k") () =
                ignore
                  ( P.make ?timeout ~issuer
                      ~authorization_endpoint:authorization
                      ~token_endpoint:token ~jwks_uri:jwks http
                    : P.t )
              in
              make () ;
              raises_match ~msg:"http authorization endpoint" is_invalid
                (fun () -> make ~authorization:"http://idp.example/a" () ) ;
              raises_match ~msg:"user information" is_invalid (fun () ->
                  make ~jwks:"https://u@idp.example:80/jwks" () ) ;
              raises_match ~msg:"http jwks_uri" is_invalid (fun () ->
                  make ~jwks:"http://idp.example/k" () ) ;
              is_true ~msg:"max_length 1"
                (Result.is_error (P.discover ~max_length:1 http issuer)) ;
              raises_match ~msg:"http" is_invalid (fun () ->
                  make ~token:"http://idp.example/token" () ) ;
              raises_match ~msg:"query" is_invalid (fun () ->
                  make ~issuer:(issuer ^ "?a") () ) ;
              raises_match ~msg:"http issuer" is_invalid (fun () ->
                  make ~issuer:"http://idp.example" () ) ;
              raises_match ~msg:"timeout" is_invalid (fun () ->
                  make ~timeout:Span.zero () ) ;
              raises_match ~msg:"discover timeout" is_invalid (fun () ->
                  ignore (P.discover ~timeout:Span.zero http issuer) ) ;
              raises_match ~msg:"max_length" is_invalid (fun () ->
                  ignore (P.discover ~max_length:0 http issuer) ) ) )
    ; test "verify_id_token accepts a good token" (fun () ->
          with_idp (fun _ http _ ->
              let p = discover http in
              equal oidc_result (Ok ()) (verify p (id_token [])) ;
              equal oidc_result (Ok ())
                (verify p (id_token ~kid:"rs1" ~alg:"RS256" ~sign:(rs ()) [])) ;
              match O.verify_id_token p ~client_id:"app" (id_token []) with
              | Ok c ->
                  equal (option string) (Some "ada") (J.subject c)
              | Error _ ->
                  fail "Ok" ) )
    ; test "verify_id_token refuses bad claims" (fun () ->
          with_idp (fun _ http _ ->
              let p = discover http in
              List.iter
                (fun (label, expected, t) ->
                  equal ~msg:label oidc_result expected (verify p t) )
                [ ( "aud"
                  , Error (O.Jwt Bad_audience)
                  , id_token [("aud", {|"other"|})] )
                ; ( "iss"
                  , Error (O.Jwt Bad_issuer)
                  , id_token [("iss", {|"https://other"|})] )
                ; ( "exp"
                  , Error (O.Jwt Expired)
                  , id_token [("exp", Printf.sprintf "%.0f" (oidc_t0 -. 3600.))]
                  )
                ; ( "no exp"
                  , Error (O.Jwt (Missing "exp"))
                  , id_token [("exp", "")] )
                ; ( "signature"
                  , Error (O.Jwt Bad_signature)
                  , id_token ~sign:(fun _ -> String.make 64 'x') [] )
                ; ( "no sub"
                  , Error (O.Jwt (Missing "sub"))
                  , id_token [("sub", "")] )
                ; ( "empty sub"
                  , Error (O.Jwt (Missing "sub"))
                  , id_token [("sub", {|""|})] )
                ; ( "access token"
                  , Error (O.Jwt (Unsupported "typ at+jwt"))
                  , id_token ~typ:"at+jwt" [] )
                ; ( "access token with a prefix"
                  , Error (O.Jwt (Unsupported "typ at+jwt"))
                  , id_token ~typ:"application/AT+JWT" [] ) ] ) )
    ; test "verify_id_token checks the nonce" (fun () ->
          with_idp (fun _ http _ ->
              let p = discover http in
              equal ~msg:"missing" oidc_result (Error O.Missing_nonce)
                (verify ~nonce:"n1" p (id_token [])) ;
              equal ~msg:"other" oidc_result (Error O.Bad_nonce)
                (verify ~nonce:"n1" p (id_token [("nonce", {|"n2"|})])) ;
              equal ~msg:"equal" oidc_result (Ok ())
                (verify ~nonce:"n1" p (id_token [("nonce", {|"n1"|})])) ;
              equal ~msg:"no nonce expected" oidc_result (Ok ())
                (verify p (id_token [("nonce", {|"n2"|})])) ) )
    ; test "verify_id_token checks azp" (fun () ->
          with_idp (fun _ http _ ->
              let p = discover http in
              let two = ("aud", {|["app","other"]|}) in
              equal ~msg:"two audiences, no azp" oidc_result (Error O.Bad_azp)
                (verify p (id_token [two])) ;
              equal ~msg:"azp other" oidc_result (Error O.Bad_azp)
                (verify p (id_token [("azp", {|"other"|})])) ;
              equal ~msg:"two audiences, azp" oidc_result (Ok ())
                (verify p (id_token [two; ("azp", {|"app"|})])) ;
              equal ~msg:"one audience, azp" oidc_result (Ok ())
                (verify p (id_token [("azp", {|"app"|})])) ) )
    ; test "verify_id_token accepts the issuers of the caller" (fun () ->
          with_idp (fun _ http _ ->
              let p = discover http in
              let t = id_token [("iss", {|"idp.example"|})] in
              equal oidc_result (Error (O.Jwt Bad_issuer)) (verify p t) ;
              equal oidc_result (Ok ())
                (verify ~issuers:[issuer; "idp.example"] p t) ) )
    ; test "the keys stay fresh for max-age, from 5 min to 24 h" (fun () ->
          (* [loads cc sleeps] is the number of loads after a check, then a
             check after each sleep. *)
          let loads cc sleeps =
            let idp = new_idp () in
            idp.cache_control <- cc ;
            with_idp ~idp (fun _ http _ ->
                let p = discover http in
                ignore (verify p (id_token [])) ;
                List.map
                  (fun s ->
                    Time.sleep Mono (Span.s s) ;
                    ignore (verify p (id_token [])) ;
                    Atomic.get idp.loads )
                  sleeps )
          in
          equal ~msg:"default 1 h" (list int) [1; 2] (loads None [3599; 1]) ;
          equal ~msg:"max-age=7200" (list int) [1; 2]
            (loads (Some "public, Max-Age=7200") [7199; 1]) ;
          equal ~msg:"max-age=1: 5 min" (list int) [1; 2]
            (loads (Some "max-age=1") [299; 1]) ;
          equal ~msg:"10 days: 24 h" (list int) [1; 2]
            (loads (Some "max-age=864000") [86399; 1]) ;
          equal ~msg:"leading zeros" (list int) [1; 2]
            (loads (Some "max-age=0000000001") [299; 1]) ;
          equal ~msg:"many digits: 24 h" (list int) [1; 2]
            (loads (Some "max-age=99999999999") [86399; 1]) ;
          equal ~msg:"no-store: 1 h" (list int) [1; 2]
            (loads (Some "max-age=86400, no-store") [3599; 1]) ;
          equal ~msg:"no-cache: 1 h" (list int) [1; 2]
            (loads (Some "no-cache, max-age=86400") [3599; 1]) ;
          equal ~msg:"bad max-age: 1 h" (list int) [1; 2]
            (loads (Some "max-age=+7200, max-age=") [3599; 1]) )
    ; test "a new kid loads the keys, at most once a minute" (fun () ->
          with_idp (fun _ http idp ->
              let p = discover http in
              equal oidc_result (Ok ()) (verify p (id_token [])) ;
              idp.jwks <- jwks [with_kid "ec2" jwk_ec] ;
              (* Less than 1 min after the load: no load. *)
              equal ~msg:"too soon" oidc_result (Error (O.Jwt No_key))
                (verify p (id_token ~kid:"ec2" [])) ;
              equal int 1 (Atomic.get idp.loads) ;
              Time.sleep Mono minute_span ;
              equal ~msg:"rotation" oidc_result (Ok ())
                (verify p (id_token ~kid:"ec2" [])) ;
              equal int 2 (Atomic.get idp.loads) ;
              equal ~msg:"an unknown kid" oidc_result (Error (O.Jwt No_key))
                (verify p (id_token ~kid:"ec9" [])) ;
              equal int 2 (Atomic.get idp.loads) ) )
    ; test "a failed load keeps the keys, and waits 1 min" (fun () ->
          with_idp (fun _ http idp ->
              let p = discover http in
              equal oidc_result (Ok ()) (verify p (id_token [])) ;
              idp.jwks_status <- 500 ;
              Time.sleep Mono (Span.s 3600) ;
              equal ~msg:"old keys" oidc_result (Ok ()) (verify p (id_token [])) ;
              equal int 2 (Atomic.get idp.loads) ;
              Time.sleep Mono (Span.s 59) ;
              ignore (verify p (id_token [])) ;
              equal ~msg:"no load before 1 min" int 2 (Atomic.get idp.loads) ;
              Time.sleep Mono (Span.s 1) ;
              idp.jwks_status <- 200 ;
              ignore (verify p (id_token [])) ;
              equal int 3 (Atomic.get idp.loads) ;
              equal int 2 (List.length (P.keys p)) ;
              (* A failed load also counts for the limit of 1 min. *)
              idp.jwks_status <- 500 ;
              Time.sleep Mono (Span.s 3600) ;
              ignore (verify p (id_token [])) ;
              equal int 4 (Atomic.get idp.loads) ;
              equal ~msg:"no load after a failure" oidc_result
                (Error (O.Jwt No_key))
                (verify p (id_token ~kid:"ec9" [])) ;
              equal int 4 (Atomic.get idp.loads) ) )
    ; test "a failed reload keeps fresh keys fresh" (fun () ->
          with_idp (fun _ http idp ->
              let p = discover http in
              equal oidc_result (Ok ()) (verify p (id_token [])) ;
              Time.sleep Mono minute_span ;
              idp.jwks_status <- 500 ;
              equal oidc_result (Error (O.Jwt No_key))
                (verify p (id_token ~kid:"ec9" [])) ;
              equal int 2 (Atomic.get idp.loads) ;
              Time.sleep Mono (Span.s 120) ;
              equal oidc_result (Ok ()) (verify p (id_token [])) ;
              equal ~msg:"the keys are fresh for 1 h" int 2
                (Atomic.get idp.loads) ) )
    ; test "with no keys, verify_id_token gives Keys with the cause" (fun () ->
          List.iter
            (fun (label, set, expected) ->
              let idp = new_idp () in
              set idp ;
              with_idp ~idp (fun _ http _ ->
                  let p = discover http in
                  let m =
                    keys_error
                      (O.verify_id_token p ~client_id:"app" (id_token []))
                  in
                  is_true ~msg:(label ^ ": " ^ m) (contains m expected) ) )
            [ ("status", (fun i -> i.jwks_status <- 503), "status 503")
            ; ("not JSON", (fun i -> i.jwks <- "x"), "the JWK Set:")
            ; ( "no usable key"
              , (fun i -> i.jwks <- jwks [{|{"kty":"oct","k":"AAAA"}|}])
              , "no key that caravan can use" )
            ; ( "too large"
              , (fun i -> i.jwks <- String.make 70000 ' ' ^ jwks [jwk_ec])
              , "too large" ) ] )
    ; test "a load that hangs ends at the timeout of the provider" (fun () ->
          with_idp (fun _ http idp ->
              let p =
                P.make ~timeout:(Span.s 2) ~issuer
                  ~authorization_endpoint:(issuer ^ "/authorize")
                  ~token_endpoint:(issuer ^ "/token")
                  ~jwks_uri:(issuer ^ "/jwks") http
              in
              idp.gate <- Some (Ivar.create ()) ;
              let m =
                keys_error (O.verify_id_token p ~client_id:"app" (id_token []))
              in
              is_true ~msg:m (contains m "timeout") ;
              Option.iter (fun g -> Ivar.fill g ()) idp.gate ) )
    ; test "the default timeout of a provider is 10 s" (fun () ->
          with_idp (fun _ http idp ->
              let elapsed p =
                let gate = Ivar.create () in
                idp.gate <- Some gate ;
                let start = Time.now Mono in
                ignore (O.verify_id_token p ~client_id:"app" (id_token [])) ;
                let d = Time.diff Mono (Time.now Mono) start in
                Ivar.fill gate () ;
                idp.gate <- None ;
                Span.to_ns d
              in
              let made =
                P.make ~issuer ~authorization_endpoint:(issuer ^ "/a")
                  ~token_endpoint:(issuer ^ "/t") ~jwks_uri:(issuer ^ "/jwks")
                  http
              in
              equal int64 ~msg:"make" 10_000_000_000L (elapsed made) ;
              equal int64 ~msg:"discover" 10_000_000_000L
                (elapsed (discover http)) ) )
    ; test "one load for callers at the same time" (fun () ->
          with_idp (fun t http idp ->
              let p = discover http in
              let gate = Ivar.create () in
              idp.gate <- Some gate ;
              Fiber.scope (fun s ->
                  let a =
                    Sim_support.call ~name:"a" s (fun () ->
                        verify p (id_token []) )
                  in
                  let b =
                    Sim_support.call ~name:"b" s (fun () ->
                        verify p (id_token []) )
                  in
                  T.settle t ;
                  equal ~msg:"one load waits" int 1 (Atomic.get idp.loads) ;
                  Ivar.fill gate () ;
                  equal oidc_result (Ok ()) (Sim_support.await a) ;
                  equal oidc_result (Ok ()) (Sim_support.await b) ) ;
              equal int 1 (Atomic.get idp.loads) ) )
    ; test "a cancel of the loader does not stop the load" (fun () ->
          with_idp (fun t http idp ->
              let p = discover http in
              let gate = Ivar.create () in
              idp.gate <- Some gate ;
              Fiber.scope (fun s ->
                  let a =
                    Sim_support.call ~name:"a" s (fun () ->
                        verify p (id_token []) )
                  in
                  T.settle t ;
                  let b =
                    Sim_support.call ~name:"b" s (fun () ->
                        verify p (id_token []) )
                  in
                  T.settle t ;
                  Sim_support.cancel a ;
                  T.settle t ;
                  Ivar.fill gate () ;
                  equal oidc_result (Ok ()) (Sim_support.await b) ;
                  (* The loader sees its cancel at its next wait, if it has one:
                     it can end with the keys. *)
                  match Sim_support.await a with
                  | r ->
                      equal oidc_result (Ok ()) r
                  | exception Promise.Cancelled ->
                      () ) ;
              equal int 1 (Atomic.get idp.loads) ;
              equal oidc_result (Ok ()) (verify p (id_token [])) ;
              equal int 1 (Atomic.get idp.loads) ) ) ]

(* {2 The login and the callback} *)

(* [keep_cookie cookie resp] is the cookie after [resp]: its Set-Cookie, if any.
   An empty value ends the cookie. *)
let keep_cookie cookie resp =
  match Caravan.Headers.find "set-cookie" (Caravan.Response.headers resp) with
  | Some sc -> (
    match Caravan.Cookie.parse_set_cookie sc with
    | Some (_, "") ->
        None
    | Some (n, v) ->
        Some (n ^ "=" ^ v)
    | None ->
        cookie )
  | None ->
      cookie

(* A browser: the client and the session cookie. *)
type browser = {http: C.t; mutable cookie: string option}

(* [visit b ?meth path] sends a request to the test host, with the cookie of
   [b], and keeps the cookie of the response. It gives the response and its
   body. *)
let visit b ?(meth = Caravan.Method.GET) path =
  let headers =
    Caravan.Headers.of_list
      (Option.fold ~none:[] ~some:(fun c -> [("Cookie", c)]) b.cookie)
  in
  let resp =
    C.request b.http
      (Caravan.Request.make ~scheme:"https" ~authority:"idp.example:80" ~headers
         meth path )
  in
  let body = Body.to_string (C.body resp) in
  b.cookie <- keep_cookie b.cookie resp ;
  (resp, body)

let header name resp =
  Option.value ~default:""
    (Caravan.Headers.find name (Caravan.Response.headers resp))

let code_of resp = Caravan.Status.code (Caravan.Response.status resp)

(* The query of the Location of [resp]. *)
let location_query resp =
  let l = header "location" resp in
  match String.index_opt l '?' with
  | Some i ->
      Caravan.Form.parse (String.sub l (i + 1) (String.length l - i - 1))
  | None ->
      []

let param name q = Option.value ~default:"" (Caravan.Form.find name q)

(* [authorize idp resp] is what the provider does after the login of [resp]: it
   keeps a code for the parameters, and gives the code and the state. *)
let authorize idp resp =
  let q = location_query resp in
  let code = Printf.sprintf "code%d" (List.length idp.codes) in
  idp.codes <-
    ( code
    , { g_nonce= param "nonce" q
      ; challenge= param "code_challenge" q
      ; client_id= param "client_id" q
      ; redirect_uri= param "redirect_uri" q } )
    :: idp.codes ;
  (code, param "state" q)

let callback_path ?(path = "/callback") ?(extra = []) (code, state) =
  path ^ "?" ^ Caravan.Form.encode ([("code", code); ("state", state)] @ extra)

(* The state of a test of the flow: the browser, the fake provider, and the
   tokens that the application got. *)
type app = {b: browser; idp: idp; seen: O.tokens option ref}

(* [new_state b] starts a login, and gives its state. *)
let new_state b =
  let resp, _ = visit b "/login" in
  param "state" (location_query resp)

let redirect_uri = issuer ^ "/callback"

(* [with_app ?idp ?client f] runs [f t browser idp] with the fake provider and
   an application on the same host: /login, /callback (the function of the
   application signs the user in and keeps the tokens in [seen]), and /me (the
   user). A failure of the callback gives a 400 with the failure. [client] makes
   the client of the provider. *)
let with_app ?idp
    ?(client = fun p -> O.client ~provider:p ~id:"app" ~redirect_uri ())
    ?(other =
      fun p ->
        O.client ~provider:p ~id:"other"
          ~redirect_uri:(issuer ^ "/callback-other")
          ()) ?(headers = Caravan.Headers.empty) f =
  let stack = ref (fun _ -> Caravan.Response.string "no app") in
  let seen = ref None in
  with_idp ?idp
    ~app:(fun req -> !stack req)
    (fun _ http idp ->
      let p = discover http in
      let c = client p in
      let signed_in req (tokens : O.tokens) ~return =
        seen := Some tokens ;
        Caravan_auth.sign_in req (Caravan_auth.User.make tokens.subject) ;
        Caravan.Response.empty ~status:Caravan.Status.see_other
          ~headers:(Caravan.Headers.of_list [("Location", return)])
          ()
      in
      let failed _ f =
        Caravan.Response.string ~status:Caravan.Status.bad_request ~headers
          (Format.asprintf "%a" O.pp_failure f)
      in
      (* An other client of the same provider, on the same session. *)
      let other = other p in
      let app req =
        match Caravan.Request.path_only req with
        | "/login" ->
            O.login c req
        | "/callback" ->
            O.callback ~failed c signed_in req
        | "/login-other" ->
            O.login other req
        | "/callback-other" ->
            O.callback ~failed other signed_in req
        | "/me" -> (
          match Caravan_auth.user req with
          | Some u ->
              Caravan.Response.string (Caravan_auth.User.id u)
          | None ->
              Caravan.Response.string ~status:Caravan.Status.unauthorized "" )
        | _ ->
            Caravan.Response.string ~status:Caravan.Status.not_found ""
      in
      stack :=
        Caravan.Handler.compose
          [ Caravan_session.middleware
              ~keys:[Caravan_session.Key.generate ()]
              ~store:(Caravan_session.memory_store ())
              ()
          ; Caravan_auth.middleware ~schemes:[Caravan_auth.Scheme.session ()] ()
          ]
          app ;
      f {b= {http; cookie= None}; idp; seen} )

(* A login and its callback; the callback response and its body. *)
let login_and_callback ?(login = "/login") ?extra b idp =
  let resp, _ = visit b login in
  visit b (callback_path ?extra (authorize idp resp))

let flow_tests =
  group "OpenID Connect: the login and the callback"
    [ test "a login, the callback, then the user is signed in" (fun () ->
          with_app (fun {b; idp; seen} ->
              let resp, _ = visit b "/login?return=/me" in
              equal int 303 (code_of resp) ;
              equal string "no-store" (header "cache-control" resp) ;
              equal string "no-referrer" (header "referrer-policy" resp) ;
              let l = header "location" resp in
              is_true ~msg:l
                (String.starts_with ~prefix:(issuer ^ "/authorize?") l) ;
              let q = location_query resp in
              equal (list string) ~msg:"the parameters"
                [ "response_type"
                ; "client_id"
                ; "redirect_uri"
                ; "scope"
                ; "state"
                ; "nonce"
                ; "code_challenge"
                ; "code_challenge_method" ]
                (List.map fst q) ;
              equal string "code" (param "response_type" q) ;
              equal string "app" (param "client_id" q) ;
              equal string redirect_uri (param "redirect_uri" q) ;
              equal string "openid email profile" (param "scope" q) ;
              equal string "S256" (param "code_challenge_method" q) ;
              List.iter
                (fun n -> equal int ~msg:n 43 (String.length (param n q)))
                ["state"; "nonce"; "code_challenge"] ;
              let resp, _ = visit b (callback_path (authorize idp resp)) in
              equal int 303 (code_of resp) ;
              equal string "/me" (header "location" resp) ;
              equal string "no-referrer" (header "referrer-policy" resp) ;
              ( match !seen with
              | Some (t : O.tokens) ->
                  equal (option string) (Some "at") t.access_token ;
                  equal (option int) (Some 3600) t.expires_in ;
                  equal (option string) (Some "ada") (J.subject t.claims) ;
                  is_true (String.length t.id_token > 0)
              | None ->
                  fail "no tokens" ) ;
              let _, me = visit b "/me" in
              equal string "ada" me ;
              (* The token request of a public client. *)
              match idp.token_calls with
              | [(auth, form)] ->
                  equal (option string) None auth ;
                  equal (list string)
                    [ "grant_type"
                    ; "code"
                    ; "redirect_uri"
                    ; "code_verifier"
                    ; "client_id" ]
                    (List.map fst form)
              | _ ->
                  fail "one token request" ) )
    ; test "a second callback of a login gives No_login" (fun () ->
          with_app (fun {b; idp; _} ->
              let resp, _ = visit b "/login" in
              let path = callback_path (authorize idp resp) in
              equal int 303 (code_of (fst (visit b path))) ;
              let resp, body = visit b path in
              equal int 400 (code_of resp) ;
              equal string "No_login" body ;
              equal string "no-referrer" (header "referrer-policy" resp) ) )
    ; test "a callback with no state or an unknown state gives No_login"
        (fun () ->
          with_app (fun {b; idp; _} ->
              let resp, _ = visit b "/login" in
              let code, _ = authorize idp resp in
              equal string "No_login" (snd (visit b ("/callback?code=" ^ code))) ;
              equal string "No_login"
                (snd (visit b (callback_path (code, "other")))) ;
              (* The login stays. *)
              let resp, _ = visit b (callback_path (authorize idp resp)) in
              equal int 303 (code_of resp) ) )
    ; test "a login older than login_timeout gives Expired" (fun () ->
          with_app
            ~client:(fun p ->
              O.client ~login_timeout:(Span.s 60) ~provider:p ~id:"app"
                ~redirect_uri () )
            (fun {b; idp; _} ->
              let resp, _ = visit b "/login" in
              let g = authorize idp resp in
              Time.sleep Mono (Span.s 59) ;
              let resp2, _ = visit b "/login" in
              let g2 = authorize idp resp2 in
              Time.sleep Mono (Span.s 1) ;
              equal string "Expired" (snd (visit b (callback_path g))) ;
              equal int 303 (code_of (fst (visit b (callback_path g2)))) ) )
    ; test "the default login timeout is 10 min" (fun () ->
          with_app (fun {b; idp; _} ->
              let g1 = authorize idp (fst (visit b "/login")) in
              Time.sleep Mono (Span.s 1) ;
              let g2 = authorize idp (fst (visit b "/login")) in
              Time.sleep Mono (Span.s 599) ;
              equal string "Expired" (snd (visit b (callback_path g1))) ;
              equal int 303 (code_of (fst (visit b (callback_path g2)))) ) )
    ; test "an error of the provider gives Denied" (fun () ->
          with_app (fun {b; _} ->
              let callback q =
                snd (visit b ("/callback?" ^ Caravan.Form.encode q))
              in
              equal string {|Denied ("access_denied", Some "no")|}
                (callback
                   [ ("error", "access_denied")
                   ; ("error_description", "no")
                   ; ("state", new_state b) ] ) ;
              equal string {|Denied ("x", None)|}
                (callback [("error", "x"); ("state", new_state b)]) ;
              equal string {|Bad_response "no code"|}
                (callback [("state", new_state b)]) ;
              equal string {|Bad_response "no code"|}
                (callback [("code", ""); ("state", new_state b)]) ;
              (* The state comes first. *)
              equal string "No_login"
                (callback [("error", "x"); ("error_description", "y")]) ;
              equal string "No_login"
                (callback [("error", "x"); ("state", "unknown")]) ) )
    ; test "a failed callback uses up its login" (fun () ->
          with_app (fun {b; idp; _} ->
              let again setup =
                let resp, _ = visit b "/login" in
                let g = authorize idp resp in
                setup () ;
                let first = snd (visit b (callback_path g)) in
                idp.token_reply <- None ;
                idp.nonce <- None ;
                (first, snd (visit b (callback_path g)))
              in
              equal (pair string string) ~msg:"token"
                ({|Token "status 400: invalid_grant"|}, "No_login")
                (again (fun () ->
                     idp.token_reply <- Some (400, {|{"error":"invalid_grant"}|}) )
                ) ;
              equal (pair string string) ~msg:"id token"
                ("Id_token (Bad_nonce)", "No_login")
                (again (fun () -> idp.nonce <- Some "other")) ;
              let state = new_state b in
              let denied () =
                snd
                  (visit b
                     ( "/callback?"
                     ^ Caravan.Form.encode [("error", "x"); ("state", state)] ) )
              in
              equal string {|Denied ("x", None)|} (denied ()) ;
              equal string "No_login" (denied ()) ) )
    ; test "a login removes the expired logins" (fun () ->
          with_app (fun {b; idp; _} ->
              let g = authorize idp (fst (visit b "/login")) in
              Time.sleep Mono (Span.s 600) ;
              ignore (visit b "/login") ;
              (* Not Expired: the login is gone. *)
              equal string "No_login" (snd (visit b (callback_path g))) ) )
    ; test "the expiry of a login is that of its client" (fun () ->
          with_app
            ~client:(fun p ->
              O.client ~login_timeout:(Span.s 60) ~provider:p ~id:"app"
                ~redirect_uri () )
            (fun {b; idp; _} ->
              let other = authorize idp (fst (visit b "/login-other")) in
              Time.sleep Mono (Span.s 120) ;
              equal int 303 (code_of (fst (login_and_callback b idp))) ;
              equal int 303
                (code_of
                   (fst (visit b (callback_path ~path:"/callback-other" other))) ) ) )
    ; test "a callback keeps the logins of the other client" (fun () ->
          with_app (fun {b; idp; _} ->
              let other = authorize idp (fst (visit b "/login-other")) in
              equal int 303 (code_of (fst (login_and_callback b idp))) ;
              equal int 303
                (code_of
                   (fst (visit b (callback_path ~path:"/callback-other" other))) ) ) )
    ; test "a failure response keeps its Referrer-Policy" (fun () ->
          with_app
            ~headers:
              (Caravan.Headers.of_list [("Referrer-Policy", "same-origin")])
            (fun {b; _} ->
              let resp, _ = visit b "/callback?state=x" in
              equal string "same-origin" (header "referrer-policy" resp) ) )
    ; test "a token response with no access_token" (fun () ->
          with_app (fun {b; idp; seen} ->
              let resp, _ = visit b "/login" in
              let g = authorize idp resp in
              let nonce = (snd (List.hd idp.codes)).g_nonce in
              idp.token_reply <-
                Some
                  ( 200
                  , Printf.sprintf
                      {|{"token_type":"Bearer","id_token":"%s","expires_in":"x"}|}
                      (id_token [("nonce", Printf.sprintf "%S" nonce)]) ) ;
              equal int 303 (code_of (fst (visit b (callback_path g)))) ;
              match !seen with
              | Some t ->
                  equal (option string) None t.access_token ;
                  equal (option int) None t.expires_in ;
                  equal string "ada" t.subject
              | None ->
                  fail "no tokens" ) )
    ; test "a bad token response gives Token or Bad_response" (fun () ->
          with_app (fun {b; idp; _} ->
              List.iter
                (fun (reply, expected) ->
                  idp.token_reply <- reply ;
                  equal string expected (snd (login_and_callback b idp)) )
                [ ( Some (400, {|{"error":"invalid_grant"}|})
                  , {|Token "status 400: invalid_grant"|} )
                ; (Some (500, "x"), {|Token "status 500"|})
                ; (Some (401, "[1]"), {|Token "status 401"|})
                ; (Some (401, {|{"error":1}|}), {|Token "status 401"|})
                ; ( Some (200, "x")
                  , {|Bad_response "the token response is not a JSON object"|}
                  )
                ; ( Some (200, "[]")
                  , {|Bad_response "the token response is not a JSON object"|}
                  )
                ; ( Some (200, {|{"token_type":"Bearer"}|})
                  , {|Bad_response "the token response has no id_token"|} )
                ; ( Some (200, {|{"token_type":"Bearer","id_token":1}|})
                  , {|Bad_response "the token response has no id_token"|} )
                ; ( Some (200, {|{"token_type":"mac","id_token":"x"}|})
                  , {|Bad_response "the token type is not Bearer"|} )
                ; ( Some (200, {|{"id_token":"x"}|})
                  , {|Bad_response "the token type is not Bearer"|} )
                ; ( Some (200, String.make 70000 ' ')
                  , {|Token "https://idp.example:80/token: the body is too large"|}
                  ) ] ;
              (* The type is case-insensitive; expires_in can be absent. *)
              idp.token_reply <- None ;
              let resp, _ = visit b "/login" in
              let g = authorize idp resp in
              let nonce = (snd (List.hd idp.codes)).g_nonce in
              idp.token_reply <-
                Some
                  ( 200
                  , Printf.sprintf {|{"token_type":"bEaReR","id_token":"%s"}|}
                      (id_token [("nonce", Printf.sprintf "%S" nonce)]) ) ;
              equal int 303 (code_of (fst (visit b (callback_path g)))) ) )
    ; test "an ID token with the nonce of an other login gives Bad_nonce"
        (fun () ->
          with_app (fun {b; idp; _} ->
              idp.nonce <- Some "other" ;
              equal string "Id_token (Bad_nonce)"
                (snd (login_and_callback b idp)) ) )
    ; test "a session keeps the 4 newest logins" (fun () ->
          (* The oldest of the 5 logins is gone; the next oldest works. *)
          with_app (fun {b; idp; _} ->
              let gs =
                List.init 5 (fun _ -> authorize idp (fst (visit b "/login")))
              in
              equal string "No_login"
                (snd (visit b (callback_path (List.nth gs 0)))) ;
              equal int 303
                (code_of (fst (visit b (callback_path (List.nth gs 1))))) ) )
    ; test "the return path" (fun () ->
          with_app (fun {b; idp; _} ->
              let return path =
                let resp, _ = visit b path in
                header "location"
                  (fst (visit b (callback_path (authorize idp resp))))
              in
              equal string "/" (return "/login") ;
              equal string "/" (return "/login?return=//evil.example") ;
              equal string "/" (return "/login?return=https://evil.example") ;
              equal string "/a,b|c%2C"
                (return
                   ("/login?" ^ Caravan.Form.encode [("return", "/a,b|c%2C")]) ) ;
              let long = "/" ^ String.make 1023 'x' in
              equal string long
                (return ("/login?" ^ Caravan.Form.encode [("return", long)])) ;
              equal string "/"
                (return
                   ("/login?" ^ Caravan.Form.encode [("return", long ^ "x")]) ) ) )
    ; test "a return path cannot add a login (login CSRF)" (fun () ->
          (* An attacker knows the state, the nonce and the verifier of a login
             that a return path would add. *)
          with_app (fun {b; _} ->
              let b64 = B64.encode ~url:true ~pad:false in
              let forged =
                String.concat "|"
                  [ b64 issuer
                  ; b64 "app"
                  ; Printf.sprintf "%.0f" oidc_t0
                  ; "S"
                  ; "N"
                  ; "V"
                  ; b64 "/" ]
              in
              ignore
                (visit b
                   ("/login?" ^ Caravan.Form.encode [("return", "/x," ^ forged)]) ) ;
              equal string "No_login" (snd (visit b (callback_path ("c", "S")))) ) )
    ; test "a state of an other client gives No_login" (fun () ->
          with_app (fun {b; idp; _} ->
              let resp, _ = visit b "/login" in
              let g = authorize idp resp in
              let other = callback_path ~path:"/callback-other" g in
              equal string "No_login" (snd (visit b other)) ;
              (* The login of the client stays. *)
              equal int 303 (code_of (fst (visit b (callback_path g)))) ) )
    ; test "the iss parameter must be the issuer" (fun () ->
          with_app (fun {b; idp; _} ->
              equal string
                {|Bad_response "the iss parameter is not the issuer"|}
                (snd
                   (login_and_callback
                      ~extra:[("iss", "https://evil.example")]
                      b idp ) ) ;
              equal int 303
                (code_of
                   (fst (login_and_callback ~extra:[("iss", issuer)] b idp)) ) ) ;
          let idp = new_idp () in
          idp.document <-
            doc [("authorization_response_iss_parameter_supported", "true")] ;
          with_app ~idp (fun {b; idp; _} ->
              equal string {|Bad_response "no iss parameter"|}
                (snd (login_and_callback b idp)) ;
              equal int 303
                (code_of
                   (fst (login_and_callback ~extra:[("iss", issuer)] b idp)) ) ) )
    ; test "the secret goes as auth tells" (fun () ->
          let calls ?auth ?methods () =
            let idp = new_idp () in
            Option.iter
              (fun m ->
                idp.document <-
                  doc [("token_endpoint_auth_methods_supported", m)] )
              methods ;
            with_app ~idp
              ~client:(fun p ->
                O.client ~secret:"s:é" ?auth ~provider:p ~id:"app id"
                  ~redirect_uri () )
              (fun {b; idp; _} ->
                ignore (login_and_callback b idp) ;
                match idp.token_calls with
                | [(a, form)] ->
                    ( Option.value a ~default:"none"
                    , List.filter_map
                        (fun (n, v) ->
                          if n = "client_id" || n = "client_secret" then
                            Some (n ^ "=" ^ v)
                          else None )
                        form )
                | _ ->
                    ("calls", []) )
          in
          let basic =
            "Basic " ^ B64.encode ~url:false ~pad:true "app+id:s%3A%C3%A9"
          in
          equal
            (pair string (list string))
            ~msg:"default" (basic, []) (calls ()) ;
          equal
            (pair string (list string))
            ~msg:"basic listed" (basic, [])
            (calls ~methods:{|["client_secret_post","client_secret_basic"]|} ()) ;
          equal
            (pair string (list string))
            ~msg:"post listed"
            ("none", ["client_id=app id"; "client_secret=s:é"])
            (calls ~methods:{|["client_secret_post"]|} ()) ;
          equal
            (pair string (list string))
            ~msg:"post"
            ("none", ["client_id=app id"; "client_secret=s:é"])
            (calls ~auth:`Post ()) ;
          equal
            (pair string (list string))
            ~msg:"basic" (basic, [])
            (calls ~auth:`Basic ~methods:{|["client_secret_post"]|} ()) )
    ; test "extra parameters and an endpoint with a query" (fun () ->
          let idp = new_idp () in
          idp.document <-
            doc
              [ ( "authorization_endpoint"
                , Printf.sprintf "%S" (issuer ^ "/authorize?tenant=t") ) ] ;
          with_app ~idp
            ~client:(fun p ->
              O.client ~scope:["email"]
                ~extra:[("prompt", "select_account")]
                ~provider:p ~id:"app" ~redirect_uri () )
            (fun {b; _} ->
              let resp, _ = visit b "/login" in
              let q = location_query resp in
              equal string "t" (param "tenant" q) ;
              equal string "select_account" (param "prompt" q) ;
              equal string "openid email" (param "scope" q) ) )
    ; test "the callback needs GET" (fun () ->
          with_app (fun {b; _} ->
              List.iter
                (fun m ->
                  let resp, _ = visit b ~meth:m "/callback?state=x" in
                  equal int 405 (code_of resp) ;
                  equal string "GET" (header "allow" resp) )
                [Caravan.Method.POST; Caravan.Method.HEAD] ) )
    ; test "client refuses bad arguments" (fun () ->
          with_idp (fun _ http _ ->
              let p = discover http in
              let is_invalid = function
                | Invalid_argument _ ->
                    true
                | _ ->
                    false
              in
              let make ?(id = "app") ?(redirect_uri = redirect_uri) ?scope
                  ?secret ?leeway ?login_timeout ?extra ?(provider = p) () =
                ignore
                  ( O.client ?scope ?secret ?leeway ?login_timeout ?extra
                      ~provider ~id ~redirect_uri ()
                    : O.client )
              in
              make () ;
              List.iter
                (fun u -> make ~redirect_uri:u ())
                [ "http://localhost/cb"
                ; "http://LOCALHOST:8080/cb"
                ; "http://127.0.0.1:9/cb"
                ; "http://[::1]/cb" ] ;
              List.iter
                (fun (label, f) -> raises_match ~msg:label is_invalid f)
                [ ("empty id", fun () -> make ~id:"" ())
                ; ( "http"
                  , fun () -> make ~redirect_uri:"http://app.example/cb" () )
                ; ( "http localhost.example"
                  , fun () ->
                      make ~redirect_uri:"http://localhost.example/cb" () )
                ; ("ftp", fun () -> make ~redirect_uri:"ftp://localhost/cb" ())
                ; ( "fragment"
                  , fun () -> make ~redirect_uri:"http://localhost/#f" () )
                ; ( "bad URL"
                  , fun () -> make ~redirect_uri:"http://localhost:0/" () )
                ; ( "login timeout"
                  , fun () -> make ~login_timeout:(Span.ms 999) () )
                ; ("leeway", fun () -> make ~leeway:(Span.of_ns (-1L)) ())
                ; ("empty scope", fun () -> make ~scope:[""] ())
                ; ("scope with a space", fun () -> make ~scope:["a b"] ())
                ; ("scope with DEL", fun () -> make ~scope:["a\x7f"] ())
                ; ( "redirect with a space"
                  , fun () -> make ~redirect_uri:"http://localhost/a b" () )
                ; ( "redirect with DEL"
                  , fun () -> make ~redirect_uri:"http://localhost/a\x7f" () )
                ; ( "reserved"
                  , fun () -> make ~extra:[("prompt", "x"); ("nonce", "n")] ()
                  )
                ; ("max_age", fun () -> make ~extra:[("max_age", "1")] ())
                ; ( "no secret method"
                  , fun () ->
                      make ~secret:"s"
                        ~provider:
                          (P.make ~auth_methods:["private_key_jwt"] ~issuer
                             ~authorization_endpoint:(issuer ^ "/a")
                             ~token_endpoint:(issuer ^ "/t")
                             ~jwks_uri:(issuer ^ "/k") http )
                        () ) ] ;
              make ~login_timeout:(Span.s 1) ~leeway:Span.zero
                ~extra:[("prompt", "x")]
                () ) )
    ; test "pp_failure" (fun () ->
          equal (list string)
            [ "No_login"
            ; "Expired"
            ; {|Denied ("e", Some "d")|}
            ; {|Bad_response "m"|}
            ; {|Token "m"|}
            ; "Id_token (Missing_nonce)" ]
            (List.map
               (Format.asprintf "%a" O.pp_failure)
               [ O.No_login
               ; Expired
               ; Denied ("e", Some "d")
               ; Bad_response "m"
               ; Token "m"
               ; Id_token Missing_nonce ] ) ) ]

(* {1 Codes by email} *)

module Code = Caravan_auth.Code
module Limiter = Caravan_middleware.Rate_limit.Limiter

(* The application of the code tests, called as a function (no network):
   /email?address=A starts a code, /code?code=C checks it and signs the user in,
   /me is the user, /raw is the session value of the code, /corrupt puts a bad
   value there. *)
let code_app codes =
  let text = Caravan.Response.string in
  let q req n =
    Option.value ~default:""
      (Caravan.Form.find n (Caravan.Request.query_params req))
  in
  (* /email<suffix> and /code<suffix> of [codes]. With [sign_in], a right code
     signs the user in. *)
  let routes_for ~suffix ~sign_in codes req =
    match Caravan.Request.path_only req with
    | p when p = "/email" ^ suffix -> (
      match Code.start codes req ~address:(q req "address") with
      | Ok code ->
          Some (text ("code:" ^ code))
      | Error wait ->
          Some (text (Printf.sprintf "wait:%Ld" (Span.to_ns wait))) )
    | p when p = "/code" ^ suffix -> (
      match Code.check codes req (q req "code") with
      | Ok address ->
          if sign_in then
            Caravan_auth.sign_in req (Caravan_auth.User.make address) ;
          Some (text ("ok:" ^ address))
      | Error f ->
          Some (text (Format.asprintf "%a" Code.pp_failure f)) )
    | _ ->
        None
  in
  let up = Code.make ~purpose:"sign-up" () in
  let routes req =
    match routes_for ~suffix:"" ~sign_in:true codes req with
    | Some r ->
        r
    | None -> (
      match routes_for ~suffix:"-up" ~sign_in:false up req with
      | Some r ->
          r
      | None -> (
        match Caravan.Request.path_only req with
        | "/me" ->
            text
              (Option.fold ~none:"none" ~some:Caravan_auth.User.id
                 (Caravan_auth.user req) )
        | "/raw" ->
            text
              (Option.value ~default:""
                 (Caravan_session.Private.find "caravan.code.sign-in" req) )
        | "/corrupt" ->
            Caravan_session.Private.set "caravan.code.sign-in" "x|1" req ;
            text ""
        | _ ->
            text "" ) )
  in
  Caravan.Handler.compose
    [ Caravan_session.middleware
        ~keys:[Caravan_session.Key.generate ()]
        ~store:(Caravan_session.memory_store ())
        ()
    ; Caravan_auth.middleware ~schemes:[Caravan_auth.Scheme.session ()] () ]
    routes

(* A browser of the code tests: its cookie. *)
type jar = {mutable jar: string option}

(* [call app j path] is the body of the response of [app] to a GET of [path],
   with the cookie of [j]. *)
let call app j path =
  let headers =
    Caravan.Headers.of_list
      (Option.fold ~none:[] ~some:(fun c -> [("Cookie", c)]) j.jar)
  in
  let resp = app (Caravan.Request.make ~headers Caravan.Method.GET path) in
  j.jar <- keep_cookie j.jar resp ;
  match Caravan.Response.kind resp with
  | Caravan.Response.Body b ->
      Body.to_string b
  | _ ->
      ""

(* [email app j address] starts a code: the code, or the wait. *)
let email app j address =
  call app j ("/email?" ^ Caravan.Form.encode [("address", address)])

let code_of_body body =
  if String.starts_with ~prefix:"code:" body then
    String.sub body 5 (String.length body - 5)
  else fail ("no code: " ^ body)

let enter app j code =
  call app j ("/code?" ^ Caravan.Form.encode [("code", code)])

(* A wrong code of [digits] digits for [code]. *)
let other code =
  String.map (fun c -> if c = '9' then '0' else Char.chr (Char.code c + 1)) code

let with_codes ?codes f =
  Sim_support.run
    ~guard:(Span.s (3 * 86400))
    (fun sim ->
      Weft_sim.set_posix sim (Time.posix_of_unix_s oidc_t0) ;
      let codes = match codes with Some c -> c () | None -> Code.make () in
      f (code_app codes) )

let big () = Limiter.take (Limiter.make ~burst:100_000 ~every:(Span.ms 1) ())

let code_tests =
  group "Codes by email"
    [ test "a code signs the user in, one time" (fun () ->
          with_codes (fun app ->
              let j = {jar= None} in
              let code = code_of_body (email app j "ada@example.com") in
              equal int 8 (String.length code) ;
              let before = j.jar in
              equal string "ok:ada@example.com" (enter app j code) ;
              is_true ~msg:"a new session ID" (j.jar <> before) ;
              equal string "ada@example.com" (call app j "/me") ;
              equal string "No_code" (enter app j code) ) )
    ; test "spaces and '-' in the text are removed" (fun () ->
          with_codes (fun app ->
              let j = {jar= None} in
              let code = code_of_body (email app j "a@x") in
              let text =
                " " ^ String.sub code 0 4 ^ "-" ^ String.sub code 4 4 ^ " "
              in
              equal string "ok:a@x" (enter app j text) ) )
    ; test "wrong codes take the attempts" (fun () ->
          with_codes (fun app ->
              let j = {jar= None} in
              let code = code_of_body (email app j "a@x") in
              List.iter
                (fun left ->
                  equal string
                    (Printf.sprintf {|Wrong {address= "a@x"; left= %d}|} left)
                    (enter app j (other code)) )
                [4; 3; 2; 1; 0] ;
              equal string "No_code" (enter app j code) ) )
    ; test "a text that is not the digits is a wrong code" (fun () ->
          with_codes (fun app ->
              let j = {jar= None} in
              let code = code_of_body (email app j "a@x") in
              List.iter
                (fun (t, left) ->
                  equal string ~msg:t
                    (Printf.sprintf {|Wrong {address= "a@x"; left= %d}|} left)
                    (enter app j t) )
                [ (String.sub code 0 7, 4)
                ; (code ^ "0", 3)
                ; ("abcdefgh", 2)
                ; ("", 1) ] ;
              equal string "ok:a@x" (enter app j code) ) )
    ; test "a code expires after ttl" (fun () ->
          with_codes (fun app ->
              let j = {jar= None} in
              let code = code_of_body (email app j "a@x") in
              Time.sleep Mono (Span.s 599) ;
              equal string {|Wrong {address= "a@x"; left= 4}|}
                (enter app j (other code)) ;
              Time.sleep Mono (Span.s 1) ;
              equal string "Expired" (enter app j code) ;
              equal string "No_code" (enter app j code) ) )
    ; test "sends: 3 codes, then one each 10 min" (fun () ->
          with_codes (fun app ->
              let j = {jar= None} in
              ignore (code_of_body (email app j "a@x")) ;
              ignore (code_of_body (email app j "a@x")) ;
              let last = code_of_body (email app j "a@x") in
              equal string "wait:600000000000" (email app j "a@x") ;
              (* The code of the session stays. *)
              equal string "ok:a@x" (enter app j last) ;
              Time.sleep Mono (Span.s 599) ;
              equal string "wait:1000000000" (email app j "a@x") ;
              Time.sleep Mono (Span.s 1) ;
              ignore (code_of_body (email app j "a@x")) ) )
    ; test "the variants of an address share the limits" (fun () ->
          with_codes (fun app ->
              let j = {jar= None} in
              List.iter
                (fun a -> ignore (code_of_body (email app j a)))
                ["Ada@Example.com"; "ada@example.com"; " ADA@EXAMPLE.COM "] ;
              equal string "wait:600000000000" (email app j "ada@example.COM") ) )
    ; test "checks: 10 for an address over all sessions, then one an hour"
        (fun () ->
          with_codes
            ~codes:(fun () -> Code.make ~sends:(big ()) ())
            (fun app ->
              (* 2 wrong codes in each of 5 sessions. *)
              for _ = 1 to 5 do
                let j = {jar= None} in
                let code = code_of_body (email app j "a@x") in
                ignore (enter app j (other code)) ;
                ignore (enter app j (other code))
              done ;
              let j = {jar= None} in
              let code = code_of_body (email app j "a@x") in
              (* The right code too: no token. The attempts stay. *)
              equal string {|Too_many {address= "a@x"; wait= 3600000000000 ns}|}
                (enter app j code) ;
              (* One token an hour. The session ends after 30 min idle: a new
                 session. *)
              Time.sleep Mono (Span.s 3600) ;
              let j = {jar= None} in
              let code = code_of_body (email app j "a@x") in
              equal string {|Wrong {address= "a@x"; left= 4}|}
                (enter app j (other code)) ;
              equal string {|Too_many {address= "a@x"; wait= 3600000000000 ns}|}
                (enter app j code) ) )
    ; test "Too_many does not take an attempt" (fun () ->
          with_codes
            ~codes:(fun () ->
              Code.make
                ~checks:
                  (Limiter.take (Limiter.make ~burst:1 ~every:(Span.s 60) ()))
                () )
            (fun app ->
              let j = {jar= None} in
              let code = code_of_body (email app j "a@x") in
              equal string {|Wrong {address= "a@x"; left= 4}|}
                (enter app j (other code)) ;
              equal string {|Too_many {address= "a@x"; wait= 60000000000 ns}|}
                (enter app j code) ;
              Time.sleep Mono (Span.s 60) ;
              equal string {|Wrong {address= "a@x"; left= 3}|}
                (enter app j (other code)) ;
              Time.sleep Mono (Span.s 60) ;
              equal string "ok:a@x" (enter app j code) ) )
    ; test "a new code replaces the code of the session" (fun () ->
          with_codes (fun app ->
              let j = {jar= None} in
              let old = code_of_body (email app j "a@x") in
              let code = code_of_body (email app j "a@x") in
              if old <> code then
                equal string {|Wrong {address= "a@x"; left= 4}|}
                  (enter app j old) ;
              equal string "ok:a@x" (enter app j code) ) )
    ; test "a code works only in its session" (fun () ->
          with_codes (fun app ->
              let code = code_of_body (email app {jar= None} "a@x") in
              equal string "No_code" (enter app {jar= None} code) ) )
    ; test "the session keeps a hash, not the code" (fun () ->
          with_codes (fun app ->
              let j = {jar= None} in
              let code = code_of_body (email app j "a@x") in
              let raw = call app j "/raw" in
              is_true ~msg:raw (String.length raw > 0) ;
              is_false ~msg:raw (contains raw code) ) )
    ; test "a bad value in the session gives No_code" (fun () ->
          with_codes (fun app ->
              let j = {jar= None} in
              ignore (call app j "/corrupt") ;
              equal string "No_code" (enter app j "12345678") ) )
    ; test "the variants of an address share the checks" (fun () ->
          with_codes
            ~codes:(fun () ->
              Code.make ~sends:(big ())
                ~checks:
                  (Limiter.take (Limiter.make ~burst:1 ~every:(Span.s 60) ()))
                () )
            (fun app ->
              let j = {jar= None} in
              let code = code_of_body (email app j "A@x") in
              ignore (enter app j (other code)) ;
              let j = {jar= None} in
              let code = code_of_body (email app j "a@X ") in
              equal string {|Too_many {address= "a@X "; wait= 60000000000 ns}|}
                (enter app j code) ) )
    ; test "9 digits: no bias of the modulo" (fun () ->
          (* 2^30 mod 10^9 = 73_741_824: with no new draw, the codes below it
             would have two times the probability (about 1370 in 10_000, not
             about 737). *)
          with_codes
            ~codes:(fun () -> Code.make ~digits:9 ~sends:(big ()) ())
            (fun app ->
              let j = {jar= None} in
              let low = ref 0 in
              for _ = 1 to 10_000 do
                if int_of_string (code_of_body (email app j "a@x")) < 73_741_824
                then incr low
              done ;
              is_true ~msg:(string_of_int !low) (!low >= 600 && !low <= 900) ) )
    ; test "a long text is a wrong code" (fun () ->
          with_codes (fun app ->
              let j = {jar= None} in
              let code = code_of_body (email app j "a@x") in
              equal string {|Wrong {address= "a@x"; left= 4}|}
                (enter app j (code ^ String.make 57 ' ')) ;
              equal string "ok:a@x" (enter app j (code ^ String.make 56 ' ')) ) )
    ; test "the codes are uniform" (fun () ->
          with_codes
            ~codes:(fun () -> Code.make ~sends:(big ()) ())
            (fun app ->
              let j = {jar= None} in
              let counts = Array.make_matrix 8 10 0 in
              for _ = 1 to 10_000 do
                String.iteri
                  (fun i c ->
                    let d = Char.code c - 48 in
                    counts.(i).(d) <- counts.(i).(d) + 1 )
                  (code_of_body (email app j "a@x"))
              done ;
              Array.iteri
                (fun i row ->
                  Array.iteri
                    (fun d n ->
                      is_true
                        ~msg:(Printf.sprintf "position %d, digit %d: %d" i d n)
                        (n >= 800 && n <= 1200) )
                    row )
                counts ) )
    ; test "digits from 6 to 9" (fun () ->
          List.iter
            (fun digits ->
              with_codes
                ~codes:(fun () -> Code.make ~digits ())
                (fun app ->
                  let j = {jar= None} in
                  let code = code_of_body (email app j "a@x") in
                  equal int digits (String.length code) ;
                  is_true (String.for_all (fun c -> c >= '0' && c <= '9') code) ;
                  equal string "ok:a@x" (enter app j code) ) )
            [6; 7; 9] )
    ; test "two purposes keep two codes" (fun () ->
          with_codes (fun app ->
              let j = {jar= None} in
              let code = code_of_body (email app j "a@x") in
              equal string "No_code"
                (call app j
                   ("/code-up?" ^ Caravan.Form.encode [("code", code)]) ) ;
              let up =
                code_of_body
                  (call app j
                     ("/email-up?" ^ Caravan.Form.encode [("address", "b@x")]) )
              in
              equal string "ok:a@x" (enter app j code) ;
              equal string "ok:b@x"
                (call app j ("/code-up?" ^ Caravan.Form.encode [("code", up)])) ) )
    ; test "one limiter can serve sends and checks" (fun () ->
          with_codes
            ~codes:(fun () ->
              let l =
                Limiter.take (Limiter.make ~burst:1 ~every:(Span.s 3600) ())
              in
              Code.make ~sends:l ~checks:l () )
            (fun app ->
              let j = {jar= None} in
              let code = code_of_body (email app j "a@x") in
              equal string "ok:a@x" (enter app j code) ) )
    ; test "start refuses an empty or long address" (fun () ->
          with_codes (fun app ->
              let j = {jar= None} in
              ignore (code_of_body (email app j (String.make 320 'a'))) ;
              List.iter
                (fun a ->
                  raises_match
                    (function Invalid_argument _ -> true | _ -> false)
                    (fun () -> ignore (email app j a)) )
                [""; String.make 321 'a'] ) )
    ; test "make refuses bad arguments" (fun () ->
          List.iter
            (fun (label, f) ->
              raises_match ~msg:label
                (function Invalid_argument _ -> true | _ -> false)
                (fun () -> ignore (f () : Code.t)) )
            [ ("digits 5", fun () -> Code.make ~digits:5 ())
            ; ("digits 10", fun () -> Code.make ~digits:10 ())
            ; ("ttl 59 s", fun () -> Code.make ~ttl:(Span.s 59) ())
            ; ("ttl 61 min", fun () -> Code.make ~ttl:(Span.s 3601) ())
            ; ("attempts 0", fun () -> Code.make ~attempts:0 ())
            ; ("attempts 11", fun () -> Code.make ~attempts:11 ())
            ; ("empty purpose", fun () -> Code.make ~purpose:"" ())
            ; ("purpose with a space", fun () -> Code.make ~purpose:"a b" ())
            ; ("purpose with a dot", fun () -> Code.make ~purpose:"a.b" ()) ] ;
          ignore
            ( Code.make ~purpose:"azAZ09-" ~digits:6 ~ttl:(Span.s 60)
                ~attempts:1 ()
              : Code.t ) ;
          ignore
            (Code.make ~digits:9 ~ttl:(Span.s 3600) ~attempts:10 () : Code.t) )
    ; test "attempts and ttl of make" (fun () ->
          with_codes
            ~codes:(fun () -> Code.make ~attempts:1 ~ttl:(Span.s 60) ())
            (fun app ->
              let j = {jar= None} in
              let code = code_of_body (email app j "a@x") in
              equal string {|Wrong {address= "a@x"; left= 0}|}
                (enter app j (other code)) ;
              equal string "No_code" (enter app j code) ;
              let code = code_of_body (email app j "a@x") in
              Time.sleep Mono (Span.s 60) ;
              equal string "Expired" (enter app j code) ) ) ]

(* {1 The parsers of passkeys} *)

module Cbor = Caravan_auth.Private.Cbor
module W = Caravan_auth.Private.Webauthn

let of_hex h =
  String.init
    (String.length h / 2)
    (fun i -> Char.chr (int_of_string ("0x" ^ String.sub h (2 * i) 2)))

(* A CBOR encoder for the tests. *)
type cb =
  [ `Int of int
  | `Bytes of string
  | `Text of string
  | `Array of cb list
  | `Map of (cb * cb) list
  | `Bool of bool
  | `Null ]

let rec cbor (v : cb) =
  let head major n =
    let b = String.make 1 in
    let c x = b (Char.chr x) in
    if n < 24 then c ((major lsl 5) lor n)
    else if n < 256 then c ((major lsl 5) lor 24) ^ c n
    else if n < 65536 then
      c ((major lsl 5) lor 25) ^ c (n lsr 8) ^ c (n land 255)
    else if n < 1 lsl 32 then
      c ((major lsl 5) lor 26)
      ^ String.init 4 (fun i -> Char.chr ((n lsr (8 * (3 - i))) land 255))
    else
      c ((major lsl 5) lor 27)
      ^ String.init 8 (fun i -> Char.chr ((n lsr (8 * (7 - i))) land 255))
  in
  match v with
  | `Int i when i >= 0 ->
      head 0 i
  | `Int i ->
      head 1 (-1 - i)
  | `Bytes b ->
      head 2 (String.length b) ^ b
  | `Text t ->
      head 3 (String.length t) ^ t
  | `Array l ->
      head 4 (List.length l) ^ String.concat "" (List.map cbor l)
  | `Map l ->
      head 5 (List.length l)
      ^ String.concat "" (List.map (fun (k, v) -> cbor k ^ cbor v) l)
  | `Bool false ->
      "\xf4"
  | `Bool true ->
      "\xf5"
  | `Null ->
      "\xf6"

let rec show_cbor = function
  | Cbor.Int i ->
      string_of_int i
  | Bytes b ->
      Printf.sprintf "h'%s'"
        (String.concat ""
           (List.map
              (fun c -> Printf.sprintf "%02x" (Char.code c))
              (List.of_seq (String.to_seq b)) ) )
  | Text t ->
      Printf.sprintf "%S" t
  | Array l ->
      "[" ^ String.concat ", " (List.map show_cbor l) ^ "]"
  | Map l ->
      "{"
      ^ String.concat ", "
          (List.map (fun (k, v) -> show_cbor k ^ ": " ^ show_cbor v) l)
      ^ "}"
  | Bool b ->
      string_of_bool b
  | Null ->
      "null"

let decode_hex h =
  match Cbor.decode_all (of_hex h) with
  | Ok v ->
      show_cbor v
  | Error e ->
      "Error: " ^ e

(* The COSE keys of the RFC keys of the JWT tests. *)
let cose_ec ?(alg = -7) ?(crv = 1) ?(x = unb64 rfc_ec_x)
    ?(y = `Bytes (unb64 rfc_ec_y)) () =
  cbor
    (`Map
       [ (`Int 1, `Int 2)
       ; (`Int 3, `Int alg)
       ; (`Int (-1), `Int crv)
       ; (`Int (-2), `Bytes x)
       ; (`Int (-3), y) ] )

let cose_ed ?(alg = -8) () =
  cbor
    (`Map
       [ (`Int 1, `Int 1)
       ; (`Int 3, `Int alg)
       ; (`Int (-1), `Int 6)
       ; (`Int (-2), `Bytes (unb64 rfc_ed_x)) ] )

let cose_rsa ?(alg = -257) ?(n = unb64 rfc_rsa_n) ?(e = "\x01\x00\x01")
    ?(more = []) () =
  cbor
    (`Map
       ( [ (`Int 1, `Int 3)
         ; (`Int 3, `Int alg)
         ; (`Int (-1), `Bytes n)
         ; (`Int (-2), `Bytes e) ]
       @ more ) )

(* The bytes of a big-endian number. *)
let bytes_of_z z =
  let bits = Z.to_bits z in
  let n = String.length bits in
  let s = String.init n (fun i -> bits.[n - 1 - i]) in
  let rec strip s =
    if String.length s > 1 && s.[0] = '\x00' then
      strip (String.sub s 1 (String.length s - 1))
    else s
  in
  strip s

let key_result s =
  match Caravan_auth.Private.key_of_cose s with
  | Ok (k, alg) ->
      Format.asprintf "%d %s" alg
        ( match J.Key.alg k with
        | ES256 ->
            "ES256"
        | EdDSA ->
            "EdDSA"
        | RS256 ->
            "RS256"
        | HS256 ->
            "HS256" )
  | Error e ->
      "Error: " ^ e

(* A DER INTEGER of the bytes [v]. *)
let der_int v = "\x02" ^ String.make 1 (Char.chr (String.length v)) ^ v

let der_seq parts =
  let body = String.concat "" parts in
  "\x30" ^ String.make 1 (Char.chr (String.length body)) ^ body

(* Authenticator data: [flags], [count], and the parts after them. *)
let auth_data ?(rp = "0123456789abcdef0123456789ABCDEF") ~flags ?(count = 0)
    rest =
  rp
  ^ String.make 1 (Char.chr flags)
  ^ String.init 4 (fun i -> Char.chr ((count lsr (8 * (3 - i))) land 255))
  ^ rest

let attested ?(id = "cred") key =
  "0123456789abcdef"
  ^ String.init 2 (fun i ->
      Char.chr ((String.length id lsr (8 * (1 - i))) land 255) )
  ^ id ^ key

let parse_result s =
  match W.parse_auth_data s with
  | Ok d ->
      Printf.sprintf "flags=%02x count=%d attested=%s ext=%b" d.flags
        d.sign_count
        ( match d.attested with
        | Some a when a.aaguid <> "0123456789abcdef" ->
            "bad aaguid"
        | Some a ->
            a.credential_id ^ "/" ^ string_of_int (String.length a.public_key)
        | None ->
            "none" )
        (Option.is_some d.extensions)
      ^
      if d.rp_id_hash <> "0123456789abcdef0123456789ABCDEF" then " bad rp"
      else ""
  | Error e ->
      "Error: " ^ e

let passkey_parser_tests =
  group "Passkeys: the parsers"
    [ test "CBOR: the vectors of RFC 8949 Appendix A" (fun () ->
          List.iter
            (fun (h, expected) -> equal string ~msg:h expected (decode_hex h))
            [ ("00", "0")
            ; ("01", "1")
            ; ("0a", "10")
            ; ("17", "23")
            ; ("1818", "24")
            ; ("1819", "25")
            ; ("1864", "100")
            ; ("1903e8", "1000")
            ; ("1a000f4240", "1000000")
            ; ("1b000000e8d4a51000", "1000000000000")
            ; ("20", "-1")
            ; ("29", "-10")
            ; ("3863", "-100")
            ; ("3903e7", "-1000")
            ; ("40", "h''")
            ; ("4401020304", "h'01020304'")
            ; ("60", {|""|})
            ; ("6161", {|"a"|})
            ; ("6449455446", {|"IETF"|})
            ; ("62225c", {|"\"\\"|})
            ; ("62c3bc", {|"\195\188"|})
            ; ("63e6b0b4", {|"\230\176\180"|})
            ; ("64f0908591", {|"\240\144\133\145"|})
            ; ("80", "[]")
            ; ("83010203", "[1, 2, 3]")
            ; ("8301820203820405", "[1, [2, 3], [4, 5]]")
            ; ( "98190102030405060708090a0b0c0d0e0f101112131415161718181819"
              , "["
                ^ String.concat ", "
                    (List.init 25 (fun i -> string_of_int (i + 1)))
                ^ "]" )
            ; ("a0", "{}")
            ; ("a201020304", "{1: 2, 3: 4}")
            ; ("a26161016162820203", {|{"a": 1, "b": [2, 3]}|})
            ; ("826161a161626163", {|["a", {"b": "c"}]|})
            ; ("f4", "false")
            ; ("f5", "true")
            ; ("f6", "null") (* Refused. *)
            ; ("1bffffffffffffffff", "Error: an integer out of range")
            ; ("3bffffffffffffffff", "Error: an integer out of range")
            ; ("c11a514b67b0", "Error: a tag")
            ; ("f7", "Error: a float or a simple value")
            ; ("f0", "Error: a float or a simple value")
            ; ("f8ff", "Error: a float or a simple value")
            ; ("f90000", "Error: a float or a simple value")
            ; ("fb3ff199999999999a", "Error: a float or a simple value")
            ; ("5f42010243030405ff", "Error: an indefinite length")
            ; ("9fff", "Error: an indefinite length")
            ; ("1c", "Error: a reserved additional information")
            ; ("1d", "Error: a reserved additional information")
            ; ("1e", "Error: a reserved additional information") ] )
    ; test "CBOR: the limits of an OCaml int" (fun () ->
          List.iter
            (fun (h, expected) -> equal string ~msg:h expected (decode_hex h))
            [ ("1b3fffffffffffffff", string_of_int max_int)
            ; ("3b3fffffffffffffff", string_of_int min_int)
            ; ("1b4000000000000000", "Error: an integer out of range")
            ; ("1b8000000000000000", "Error: an integer out of range")
            ; ("3b4000000000000000", "Error: an integer out of range") ] )
    ; test "CBOR: bad input gives an error, not an exception" (fun () ->
          List.iter
            (fun (h, expected) -> equal string ~msg:h expected (decode_hex h))
            [ ("", "Error: the input ends")
            ; ("19", "Error: the input ends")
            ; ("1a0000", "Error: the input ends")
            ; ("5bffffffffffffffff", "Error: an integer out of range")
            ; ("5b3fffffffffffffff", "Error: a length over the input")
            ; ("5a00010000", "Error: a length over the input")
            ; ("4201", "Error: a length over the input")
            ; ("83", "Error: a length over the input")
            ; ("8301", "Error: a length over the input")
            ; ("a1", "Error: a length over the input")
            ; ("a10102a1", "Error: bytes after the item")
            ; ("a201020103", "Error: two equal keys in a map")
            ; ("a2616101616101", "Error: two equal keys in a map")
            ; ("a1400102", "Error: a map key that is not an integer or a text")
            ; ("a1f401", "Error: a map key that is not an integer or a text")
            ; ("62c328", "Error: a text that is not UTF-8")
            ; ("0001", "Error: bytes after the item")
            ; ("a2010203", "Error: a length over the input")
            ; ("62c080", "Error: a text that is not UTF-8")
            ; ("63eda080", "Error: a text that is not UTF-8") ] ;
          equal string "Error: a negative position"
            ( match Cbor.decode "\x01" (-1) with
            | Ok _ ->
                "Ok"
            | Error e ->
                "Error: " ^ e ) ;
          (* The key of a map at level 16 is at level 17. *)
          equal string "Error: too deep"
            (decode_hex
               (String.concat "" (List.init 15 (fun _ -> "a100")) ^ "a18000") ) ;
          (* Maps count too: 16 maps and a leaf. *)
          equal string "Error: too deep"
            (decode_hex
               (String.concat "" (List.init 16 (fun _ -> "a100")) ^ "00") ) ;
          is_true
            (Result.is_ok
               (Cbor.decode_all
                  (of_hex
                     (String.concat "" (List.init 15 (fun _ -> "a100")) ^ "00") ) ) ) ;
          (* 16 arrays and a leaf: 17 levels. *)
          equal string "Error: too deep"
            (decode_hex
               (String.concat "" (List.init 15 (fun _ -> "81")) ^ "8100") ) ;
          (* 15 arrays and a leaf: 16 levels. *)
          equal string
            (String.make 15 '[' ^ "0" ^ String.make 15 ']')
            (String.concat ""
               (String.split_on_char ' '
                  (decode_hex
                     (String.concat "" (List.init 14 (fun _ -> "81")) ^ "8100") ) ) ) ;
          (* A map of 65 pairs. *)
          equal string "Error: a map with too many pairs"
            (decode_hex
               ( "b841"
               ^ String.concat ""
                   (List.init 65 (fun i -> Printf.sprintf "18%02x00" i)) ) ) ;
          is_true
            (Result.is_ok
               (Cbor.decode_all
                  (of_hex
                     ( "b840"
                     ^ String.concat ""
                         (List.init 64 (fun i -> Printf.sprintf "18%02x00" i))
                     ) ) ) ) )
    ; test "CBOR: decode gives the position after the item" (fun () ->
          let pos h at =
            match Cbor.decode (of_hex h) at with
            | Ok (v, i) ->
                Printf.sprintf "%s at %d" (show_cbor v) i
            | Error e ->
                e
          in
          equal string "1 at 2" (pos "ff0102ff" 1) ;
          equal string "2 at 3" (pos "ff0102" 2) ;
          equal string "{1: 2} at 4" (pos "ffa10102ff" 1) ;
          equal string "the input ends" (pos "ff01" 2) )
    ; test "COSE: the three key types" (fun () ->
          equal string "-7 ES256" (key_result (cose_ec ())) ;
          equal string "-9 ES256" (key_result (cose_ec ~alg:(-9) ())) ;
          equal string "-8 EdDSA" (key_result (cose_ed ())) ;
          equal string "-19 EdDSA" (key_result (cose_ed ~alg:(-19) ())) ;
          equal string "-257 RS256" (key_result (cose_rsa ())) ;
          (* -13 is not a private part of an RSA key. *)
          equal string "-257 RS256"
            (key_result (cose_rsa ~more:[(`Int (-13), `Bytes "x")] ())) ;
          (* An int key and a text key of the same digits. *)
          is_true
            (Result.is_ok
               (Cbor.decode_all
                  (cbor (`Map [(`Int 1, `Int 0); (`Text "1", `Int 0)])) ) ) )
    ; test "COSE: bad keys" (fun () ->
          List.iter
            (fun (label, k, expected) ->
              equal string ~msg:label ("Error: " ^ expected) (key_result k) )
            [ ( "EC2 with EdDSA"
              , cose_ec ~alg:(-8) ()
              , "an alg that is not ES256 for an EC2 key" )
            ; ( "EC2 with RS256"
              , cose_ec ~alg:(-257) ()
              , "an alg that is not ES256 for an EC2 key" )
            ; ( "OKP with ES256"
              , cose_ed ~alg:(-7) ()
              , "an alg that is not EdDSA for an OKP key" )
            ; ( "RSA with PS256"
              , cose_rsa ~alg:(-37) ()
              , "an alg that is not RS256 for an RSA key" )
            ; ("P-384", cose_ec ~crv:2 (), "a curve that is not P-256")
            ; ( "short x"
              , cose_ec ~x:(String.make 31 'x') ()
              , "x does not have 32 bytes" )
            ; ("boolean y", cose_ec ~y:(`Bool true) (), "no y byte string")
            ; ( "not on the curve"
              , cose_ec ~x:(String.make 32 '\x01') ()
              , "the point is not on P-256" )
            ; ( "RSA 1024"
              , cose_rsa
                  ~n:
                    (bytes_of_z
                       (Mirage_crypto_pk.Rsa.pub_of_priv
                          (Mirage_crypto_pk.Rsa.generate ~bits:1024 ()) )
                         .n )
                  ()
              , "not an RSA key of 2048 to 8192 bits" )
            ; ( "RSA over 8192 bits"
              , cose_rsa ~n:("\x01" ^ String.make 1023 '\x00' ^ "\x01") ()
              , "not an RSA key of 2048 to 8192 bits" )
            ; ( "RSA e of 33 bits"
              , cose_rsa ~e:"\x01\x00\x00\x00\x01" ()
              , "not an RSA key of 2048 to 8192 bits" )
            ; ("RSA e of 0", cose_rsa ~e:"\x00" (), "not an RSA key")
            ; ( "RSA with d"
              , cose_rsa ~more:[(`Int (-3), `Bytes "d")] ()
              , "a private key" )
            ; ( "RSA with qInv"
              , cose_rsa ~more:[(`Int (-8), `Bytes "q")] ()
              , "a private key" )
            ; ( "RSA with t of other primes"
              , cose_rsa ~more:[(`Int (-12), `Bytes "t")] ()
              , "a private key" )
            ; ( "a private key"
              , cbor
                  (`Map
                     [ (`Int 1, `Int 1)
                     ; (`Int 3, `Int (-8))
                     ; (`Int (-1), `Int 6)
                     ; (`Int (-2), `Bytes (unb64 rfc_ed_x))
                     ; (`Int (-4), `Bytes (String.make 32 'd')) ] )
              , "a private key" )
            ; ( "alg as a text"
              , cbor (`Map [(`Int 1, `Int 2); (`Int 3, `Text "ES256")])
              , "no alg" )
            ; ( "x of 33 bytes"
              , cose_ec ~x:(String.make 33 'x') ()
              , "x does not have 32 bytes" )
            ; ( "y of 33 bytes"
              , cose_ec ~y:(`Bytes (String.make 33 'y')) ()
              , "y does not have 32 bytes" )
            ; ( "no crv"
              , cbor (`Map [(`Int 1, `Int 2); (`Int 3, `Int (-7))])
              , "no crv" )
            ; ( "no n"
              , cbor (`Map [(`Int 1, `Int 3); (`Int 3, `Int (-257))])
              , "no n byte string" )
            ; ( "no e"
              , cbor
                  (`Map
                     [ (`Int 1, `Int 3)
                     ; (`Int 3, `Int (-257))
                     ; (`Int (-1), `Bytes (unb64 rfc_rsa_n)) ] )
              , "no e byte string" )
            ; ( "no alg"
              , cbor (`Map [(`Int 1, `Int 2); (`Int (-1), `Int 1)])
              , "no alg" )
            ; ( "kty 4"
              , cbor (`Map [(`Int 1, `Int 4); (`Int 3, `Int (-7))])
              , "a key type that is not EC2, OKP or RSA" )
            ; ("not a map", cbor (`Array []), "the key is not a map")
            ; ("trailing bytes", cose_ed () ^ "\x00", "bytes after the item")
            ; ( "OKP crv X25519"
              , cbor
                  (`Map
                     [ (`Int 1, `Int 1)
                     ; (`Int 3, `Int (-8))
                     ; (`Int (-1), `Int 4)
                     ; (`Int (-2), `Bytes (unb64 rfc_ed_x)) ] )
              , "a curve that is not Ed25519" ) ] )
    ; test "DER: r and s" (fun () ->
          let r = String.make 32 '\x7f'
          and s = "\x00" ^ String.make 31 '\x01' in
          let show = function
            | Some (r, s) ->
                Printf.sprintf "%d/%d %C%C" (String.length r) (String.length s)
                  r.[0] s.[31]
            | None ->
                "None"
          in
          equal string {|32/32 '\127''\001'|}
            (show
               (W.der_signature
                  (der_seq [der_int r; der_int (String.sub s 1 31)]) ) ) ;
          (* A high first byte needs a 0 before it. *)
          equal string {|32/32 '\255''\001'|}
            (show
               (W.der_signature
                  (der_seq
                     [der_int ("\x00" ^ String.make 32 '\xff'); der_int "\x01"] ) ) ) ;
          (* 0x00 0x80 is the shortest form of 128. *)
          equal string {|32/32 '\000''\001'|}
            (show
               (W.der_signature (der_seq [der_int "\x00\x80"; der_int "\x01"])) ) ;
          List.iter
            (fun (label, d) ->
              equal string ~msg:label "None" (show (W.der_signature d)) )
            [ ("negative", der_seq [der_int "\xff"; der_int "\x01"])
            ; ("not the shortest", der_seq [der_int "\x00\x01"; der_int "\x01"])
            ; ("zero", der_seq [der_int "\x00"; der_int "\x01"])
            ; ("zero s at the end", der_seq [der_int "\x01"; der_int "\x00"])
            ; ("0x80 is negative", der_seq [der_int "\x80"; der_int "\x01"])
            ; ("empty", der_seq [der_int ""; der_int "\x01"])
            ; ( "33 bytes of value"
              , der_seq
                  [der_int ("\x01" ^ String.make 32 '\x01'); der_int "\x01"] )
            ; ( "34 bytes"
              , der_seq
                  [der_int ("\x00\x00" ^ String.make 32 '\xff'); der_int "\x01"]
              )
            ; ("trailing", der_seq [der_int "\x01"; der_int "\x01"] ^ "\x00")
            ; ( "inner trailing"
              , der_seq [der_int "\x01"; der_int "\x01"; "\x00"] )
            ; ("one integer", der_seq [der_int "\x01"])
            ; ("not a sequence", "\x31\x06" ^ der_int "\x01" ^ der_int "\x01")
            ; ("long form", "\x30\x81\x06" ^ der_int "\x01" ^ der_int "\x01")
            ; ("short", "\x30")
            ; ("a bad length", "\x30\x07" ^ der_int "\x01" ^ der_int "\x01")
            ; ("not an integer", der_seq ["\x04\x01\x01"; der_int "\x01"])
            ; ("an integer over the end", der_seq ["\x02\x05\x01"]) ] )
    ; test "DER: a signature of mirage-crypto verifies" (fun () ->
          let data = "authenticator data and the hash" in
          let r, s =
            Mirage_crypto_ec.P256.Dsa.sign ~key:ec_priv (sha256 data)
          in
          let int v =
            let v =
              let rec strip v =
                if String.length v > 1 && v.[0] = '\x00' then
                  strip (String.sub v 1 (String.length v - 1))
                else v
              in
              strip v
            in
            der_int (if Char.code v.[0] >= 0x80 then "\x00" ^ v else v)
          in
          let signature = der_seq [int r; int s] in
          let key =
            fst (Result.get_ok (Caravan_auth.Private.key_of_cose (cose_ec ())))
          in
          is_true (Caravan_auth.Private.verify key ~data ~signature) ;
          is_false ~msg:"other data"
            (Caravan_auth.Private.verify key ~data:"x" ~signature) ;
          is_false ~msg:"raw r||s"
            (Caravan_auth.Private.verify key ~data ~signature:(r ^ s)) ;
          let ed =
            fst (Result.get_ok (Caravan_auth.Private.key_of_cose (cose_ed ())))
          in
          is_true
            (Caravan_auth.Private.verify ed ~data
               ~signature:(Mirage_crypto_ec.Ed25519.sign ~key:ed_priv data) ) ;
          let rsa =
            fst (Result.get_ok (Caravan_auth.Private.key_of_cose (cose_rsa ())))
          in
          is_true
            (Caravan_auth.Private.verify rsa ~data
               ~signature:
                 (Mirage_crypto_pk.Rsa.PKCS1.sign ~hash:`SHA256 ~key:rsa_priv
                    (`Message data) ) ) )
    ; test "authenticator data" (fun () ->
          let key = cose_ed () in
          List.iter
            (fun (label, d, expected) ->
              equal string ~msg:label expected (parse_result d) )
            [ ( "no AT"
              , auth_data ~flags:0x05 ~count:7 ""
              , "flags=05 count=7 attested=none ext=false" )
            ; ( "count u32"
              , auth_data ~flags:0x01 ~count:0xFFFFFFFF ""
              , "flags=01 count=4294967295 attested=none ext=false" )
            ; ( "AT"
              , auth_data ~flags:0x45 (attested key)
              , Printf.sprintf "flags=45 count=0 attested=cred/%d ext=false"
                  (String.length key) )
            ; ( "AT and ED"
              , auth_data ~flags:0xC5
                  (attested key ^ cbor (`Map [(`Text "x", `Int 1)]))
              , Printf.sprintf "flags=c5 count=0 attested=cred/%d ext=true"
                  (String.length key) )
            ; ( "ED"
              , auth_data ~flags:0x81 (cbor (`Map []))
              , "flags=81 count=0 attested=none ext=true" )
            ; ( "BE and BS"
              , auth_data ~flags:0x19 ""
              , "flags=19 count=0 attested=none ext=false" )
            ; ( "36 bytes"
              , String.sub (auth_data ~flags:1 "") 0 36
              , "Error: the authenticator data has less than 37 bytes" )
            ; ( "trailing"
              , auth_data ~flags:0x01 "\x00"
              , "Error: bytes after the authenticator data" )
            ; ( "AT, trailing"
              , auth_data ~flags:0x45 (attested key ^ "\x00")
              , "Error: bytes after the authenticator data" )
            ; ( "BS with no BE"
              , auth_data ~flags:0x11 ""
              , "Error: the flag BS with no flag BE" )
            ; ( "AT, short"
              , auth_data ~flags:0x41 (String.make 17 'g')
              , "Error: no attested credential data" )
            ; ( "AT, empty id"
              , auth_data ~flags:0x41 (attested ~id:"" key)
              , "Error: a credential ID of a bad length" )
            ; ( "AT, id 1024"
              , auth_data ~flags:0x41 (attested ~id:(String.make 1024 'i') key)
              , "Error: a credential ID of a bad length" )
            ; ( "AT, 18 bytes"
              , auth_data ~flags:0x41 (String.make 16 'g' ^ "\x00\x00")
              , "Error: a credential ID of a bad length" )
            ; ( "AT, an id to the end"
              , auth_data ~flags:0x41 (String.make 16 'g' ^ "\x00\x03" ^ "abc")
              , "Error: the credential public key: the input ends" )
            ; ( "AT, id over the end"
              , auth_data ~flags:0x41 (String.make 16 'g' ^ "\x00\x10" ^ "abc")
              , "Error: a credential ID of a bad length" )
            ; ( "AT, bad key"
              , auth_data ~flags:0x41 (attested "\x9f")
              , "Error: the credential public key: an indefinite length" )
            ; ( "ED, not a map"
              , auth_data ~flags:0x81 (cbor (`Array []))
              , "Error: the extensions are not a map" )
            ; ( "ED, bad"
              , auth_data ~flags:0x81 "\x9f"
              , "Error: the extensions: an indefinite length" )
            ; ( "ED, no bytes"
              , auth_data ~flags:0x81 ""
              , "Error: the extensions: the input ends" ) ] ;
          equal string ~msg:"id 1023"
            (Printf.sprintf "flags=41 count=0 attested=%s/%d ext=false"
               (String.make 1023 'i') (String.length key) )
            (parse_result
               (auth_data ~flags:0x41 (attested ~id:(String.make 1023 'i') key)) ) ;
          match W.parse_auth_data (auth_data ~flags:0x1D "") with
          | Ok d -> (
              equal (list bool) [true; true; true; true]
                [W.up d; W.uv d; W.be d; W.bs d] ;
              match W.parse_auth_data (auth_data ~flags:0x00 "") with
              | Ok d ->
                  equal (list bool)
                    [false; false; false; false]
                    [W.up d; W.uv d; W.be d; W.bs d]
              | Error _ ->
                  fail "flags 0" )
          | Error _ ->
              fail "flags" ) ]

let () =
  Mirage_crypto_rng.set_default_generator
    (Mirage_crypto_rng.create ~seed:"caravan auth tests"
       (module Mirage_crypto_rng.Fortuna) ) ;
  exit
    (Windtrap.run "auth"
       [ rfc_tests
       ; time_tests
       ; issuer_tests
       ; signature_tests
       ; jwks_tests
       ; form_tests
       ; json_tests
       ; claims_tests
       ; bearer_tests
       ; policy_tests
       ; session_tests
       ; oidc_tests
       ; flow_tests
       ; code_tests
       ; passkey_parser_tests ] )
