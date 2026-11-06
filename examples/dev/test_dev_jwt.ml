(* The tests of Dev_jwt. Each test sets a seeded generator first, then
   caravan.auth verifies the tokens in the simulator of weft, on one domain. The
   posix clock of the simulator is t0. *)

open Windtrap
open Weft
open Example_dev
module J = Caravan_auth.Jwt

let seed () =
  Mirage_crypto_rng.set_default_generator
    (Mirage_crypto_rng.create ~seed:"caravan dev_jwt tests"
       (module Mirage_crypto_rng.Fortuna) )

(* The time of the tests: 2025-06-15. *)
let t0 = 1_750_000_000

(* The claims of a token that expires one hour after t0. *)
let claims sub =
  Printf.sprintf
    {|{"iss":"https://id.example","aud":"api://orders","sub":"%s","exp":%d}|}
    sub (t0 + 3600)

let error = Testable.make ~pp:J.pp_error ~equal:( = )

(* [verify ~keys token] is the result of [J.verify] at t0, with the keys of
   [keys], a JWK Set. *)
let verify ~keys token =
  Weft_sim.run (fun sim ->
      Weft_sim.set_posix sim (Time.posix_of_unix_s (float_of_int t0)) ;
      J.verify ~issuer:(One_of ["https://id.example"])
        ~audience:(One_of ["api://orders"])
        ~keys:(Result.get_ok (J.Key.of_jwks keys))
        token )

let subject ~keys token =
  match verify ~keys token with
  | Ok c ->
      J.subject c
  | Error e ->
      failf "%a" J.pp_error e

let refused e ~keys token =
  match verify ~keys token with
  | Ok _ ->
      fail "the token is valid"
  | Error got ->
      equal error e got

(* The parts of a compact JWS. *)
let parts token =
  match String.split_on_char '.' token with
  | [h; p; s] ->
      (h, p, s)
  | _ ->
      fail "not three parts"

let () =
  exit
  @@ Windtrap.run "dev_jwt"
       [ test "caravan.auth verifies a token with the JWK Set" (fun () ->
             seed () ;
             let k = Dev_jwt.make () in
             equal (option string) (Some "ada")
               (subject ~keys:(Dev_jwt.jwks k) (Dev_jwt.sign k (claims "ada"))) )
       ; test "the JWK Set has one ES256 key with the kid" (fun () ->
             seed () ;
             let k = Dev_jwt.make ~kid:"k-1" () in
             equal string "k-1" (Dev_jwt.kid k) ;
             match J.Key.of_jwks (Dev_jwt.jwks k) with
             | Ok [key] ->
                 is_true (J.Key.alg key = J.ES256) ;
                 equal (option string) (Some "k-1") (J.Key.kid key)
             | Ok _ ->
                 fail "not one key"
             | Error e ->
                 fail e )
       ; test "a token with the payload of another token: Bad_signature"
           (fun () ->
             seed () ;
             let k = Dev_jwt.make () in
             let h, _, s = parts (Dev_jwt.sign k (claims "ada")) in
             let _, p, _ = parts (Dev_jwt.sign k (claims "root")) in
             refused J.Bad_signature ~keys:(Dev_jwt.jwks k)
               (String.concat "." [h; p; s]) )
       ; test "each make gives a new key pair" (fun () ->
             seed () ;
             let k1 = Dev_jwt.make () and k2 = Dev_jwt.make () in
             (* The two keys have the kid "dev": the signature decides. *)
             equal string "dev" (Dev_jwt.kid k2) ;
             refused J.Bad_signature ~keys:(Dev_jwt.jwks k1)
               (Dev_jwt.sign k2 (claims "ada")) )
       ; test "the header of a token has the kid" (fun () ->
             seed () ;
             let a = Dev_jwt.make ~kid:"a" ()
             and b = Dev_jwt.make ~kid:"b" () in
             (* A token with no kid tries each ES256 key: Bad_signature. A token
                with the kid "a" finds no key with this kid: No_key. *)
             refused J.No_key ~keys:(Dev_jwt.jwks b)
               (Dev_jwt.sign a (claims "ada")) )
       ; test "make refuses a kid that JSON escapes" (fun () ->
             List.iter
               (fun kid ->
                 raises_match
                   (function Invalid_argument _ -> true | _ -> false)
                   (fun () -> ignore (Dev_jwt.make ~kid ())) )
               [""; {|a"b|}; {|a\b|}; "a b"] ) ]
