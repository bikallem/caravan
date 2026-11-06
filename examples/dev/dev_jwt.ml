(* An ES256 key pair in memory and its tokens, for development only. *)

module Dsa = Mirage_crypto_ec.P256.Dsa

type t = {kid: string; priv: Dsa.priv; pub: Dsa.pub}

let alphabet =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"

(* [base64url s] is the base64url text of [s] with no padding (RFC 4648 §5). *)
let base64url s =
  let n = String.length s in
  let b = Buffer.create (((n * 4) + 2) / 3) in
  let byte i = if i < n then Char.code s.[i] else 0 in
  let rec go i =
    if i < n then (
      let v = (byte i lsl 16) lor (byte (i + 1) lsl 8) lor byte (i + 2) in
      (* 1 byte gives 2 characters, 2 bytes give 3, 3 bytes give 4. *)
      let chars = min 4 ((((n - i) * 4) + 2) / 3) in
      for k = 0 to chars - 1 do
        Buffer.add_char b alphabet.[(v lsr (18 - (6 * k))) land 63]
      done ;
      go (i + 3) )
  in
  go 0 ; Buffer.contents b

(* The kid goes into JSON texts with no escape: thus it has only characters that
   JSON does not escape. *)
let valid_kid kid =
  kid <> ""
  && String.for_all
       (function
         | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '_' | '.' ->
             true
         | _ ->
             false )
       kid

let make ?(kid = "dev") () =
  if not (valid_kid kid) then invalid_arg "Dev_jwt.make: a bad kid" ;
  let priv, pub = Dsa.generate () in
  {kid; priv; pub}

let kid t = t.kid

let jwks t =
  (* The point is 0x04, then x and y of 32 bytes each. *)
  let p = Dsa.pub_to_octets t.pub in
  let x = base64url (String.sub p 1 32)
  and y = base64url (String.sub p 33 32) in
  String.concat ""
    [ {|{"keys":[{"kty":"EC","crv":"P-256","x":"|}
    ; x
    ; {|","y":"|}
    ; y
    ; {|","kid":"|}
    ; t.kid
    ; {|","alg":"ES256","use":"sig"}]}|} ]

let sign t claims =
  let header =
    Printf.sprintf {|{"alg":"ES256","typ":"JWT","kid":"%s"}|} t.kid
  in
  let input = base64url header ^ "." ^ base64url claims in
  let digest = Digestif.SHA256.(to_raw_string (digest_string input)) in
  (* The signature is r and s, 32 bytes each (RFC 7518 §3.4). *)
  let r, s = Dsa.sign ~key:t.priv digest in
  input ^ "." ^ base64url (r ^ s)
