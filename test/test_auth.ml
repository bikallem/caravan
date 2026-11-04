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
       ; session_tests ] )
