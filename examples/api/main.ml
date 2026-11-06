(* The API example on http://127.0.0.1:8080. App.keys_of_env selects the keys
   from the environment: see the comment at the top of app.ml. With neither JWKS
   nor JWT_SECRET, main makes a development key pair (never for production) and
   prints a token. *)

open Example_dev
module App = Example_api.App

(* A development key pair, and a token for ada that expires in 1 h. *)
let dev_keys () =
  let k = Dev_jwt.make () in
  let claims =
    `Assoc
      [ ("iss", `String App.issuer)
      ; ("aud", `String App.audience)
      ; ("sub", `String "ada")
      ; ("scope", `String "orders:read")
      ; ("exp", `Int (int_of_float (Unix.time ()) + 3600)) ]
  in
  Printf.printf "Development token for ada. It expires in 1 h.\n" ;
  Printf.printf "Never use the development key pair in production.\n" ;
  Printf.printf "T=%s\n" (Dev_jwt.sign k (Yojson.Safe.to_string claims)) ;
  Result.get_ok (Caravan_auth.Jwt.Key.of_jwks (Dev_jwt.jwks k))

(* 32 random bytes in hex. *)
let random_token () =
  Mirage_crypto_rng.generate 32
  |> String.to_seq
  |> Seq.map (fun c -> Printf.sprintf "%02x" (Char.code c))
  |> List.of_seq |> String.concat ""

let () =
  (* The generator of the OS: safe on many domains. *)
  Mirage_crypto_rng_unix.use_default () ;
  let jwt_keys, internal_token =
    match App.keys_of_env Sys.getenv_opt with
    | Error problem ->
        prerr_endline problem ; exit 2
    | Ok (From_env {jwt_keys; internal_token}) ->
        (jwt_keys, internal_token)
    | Ok (Development {internal_token= Some token}) ->
        (dev_keys (), token)
    | Ok (Development {internal_token= None}) ->
        let keys = dev_keys () in
        let token = random_token () in
        Printf.printf "Development internal token:\nI=%s\n" token ;
        (keys, token)
  in
  Example_common.serve
    (App.make ~jwt_keys ~internal_token ~report:prerr_endline ())
