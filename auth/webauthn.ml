type cose =
  | Ec2 of {alg: int; x: string; y: string}
  | Okp of {alg: int; x: string}
  | Rsa of {alg: int; n: string; e: string}

let ( let* ) = Result.bind

let cose_key c =
  match c with
  | Cbor.Map pairs -> (
      let find k = List.assoc_opt (Cbor.Int k) pairs in
      let bytes k what =
        match find k with
        | Some (Cbor.Bytes b) ->
            Ok b
        | _ ->
            Error ("no " ^ what ^ " byte string")
      in
      let bytes32 k what =
        let* b = bytes k what in
        if String.length b = 32 then Ok b
        else Error (what ^ " does not have 32 bytes")
      in
      let int k what =
        match find k with Some (Cbor.Int i) -> Ok i | _ -> Error ("no " ^ what)
      in
      let* () =
        (* -4 is d of EC2 and OKP (RFC 9053), and p of RSA (RFC 8230). *)
        if List.mem_assoc (Cbor.Int (-4)) pairs then Error "a private key"
        else Ok ()
      in
      let* kty = int 1 "kty" in
      let* alg = int 3 "alg" in
      match kty with
      | 2 ->
          let* () =
            if alg = -7 || alg = -9 then Ok ()
            else Error "an alg that is not ES256 for an EC2 key"
          in
          let* crv = int (-1) "crv" in
          let* () =
            if crv = 1 then Ok () else Error "a curve that is not P-256"
          in
          let* x = bytes32 (-2) "x" in
          let* y = bytes32 (-3) "y" in
          Ok (Ec2 {alg; x; y})
      | 1 ->
          let* () =
            if alg = -8 || alg = -19 then Ok ()
            else Error "an alg that is not EdDSA for an OKP key"
          in
          let* crv = int (-1) "crv" in
          let* () =
            if crv = 6 then Ok () else Error "a curve that is not Ed25519"
          in
          let* x = bytes32 (-2) "x" in
          Ok (Okp {alg; x})
      | 3 ->
          let* () =
            if alg = -257 then Ok ()
            else Error "an alg that is not RS256 for an RSA key"
          in
          (* RFC 8230: -3 to -12 are the private parts of an RSA key. *)
          let* () =
            if
              List.exists
                (function Cbor.Int l, _ -> l <= -3 && l >= -12 | _ -> false)
                pairs
            then Error "a private key"
            else Ok ()
          in
          let* n = bytes (-1) "n" in
          let* e = bytes (-2) "e" in
          Ok (Rsa {alg; n; e})
      | _ ->
          Error "a key type that is not EC2, OKP or RSA" )
  | _ ->
      Error "the key is not a map"

(* [integer s i] is the value of the DER INTEGER at [i] of [s], with no leading
   zero, and the position after it. *)
let integer s i =
  let n = String.length s in
  if
    ( i + 2 > n )
    [@mutate off "at i + 2 = n, the length 0 or over the end gives None"]
    || s.[i] <> '\x02'
  then None
  else
    let len = Char.code s.[i + 1] in
    let i = i + 2 in
    (* A value of more than 32 bytes is refused below. *)
    if len = 0 || len > n - i then None
    else
      let first = Char.code s.[i] in
      if first >= 0x80 then None (* negative *)
      else if len > 1 && first = 0 && Char.code s.[i + 1] < 0x80 then
        None (* not the shortest form *)
      else
        let v =
          if first = 0 then String.sub s (i + 1) (len - 1)
          else String.sub s i len
        in
        if v = "" || String.length v > 32 then None (* 0, or too large *)
        else Some (String.make (32 - String.length v) '\x00' ^ v, i + len)

let der_signature s =
  let n = String.length s in
  if
    (n < 2) [@mutate off "at n = 2, the empty SEQUENCE gives None next"]
    || s.[0] <> '\x30'
    || Char.code s.[1] <> n - 2
  then None
  else
    match integer s 2 with
    | None ->
        None
    | Some (r, i) -> (
      match integer s i with
      | Some (sv, j) when j = n ->
          Some (r, sv)
      | _ ->
          None )

type attested =
  {aaguid: string; credential_id: string; public_key: string; key: Cbor.t}

type auth_data =
  { rp_id_hash: string
  ; flags: int
  ; sign_count: int
  ; attested: attested option
  ; extensions: Cbor.t option }

let flag bit d = d.flags land bit <> 0

let up = flag 0x01

let uv = flag 0x04

let be = flag 0x08

let bs = flag 0x10

let parse_auth_data s =
  let n = String.length s in
  if n < 37 then Error "the authenticator data has less than 37 bytes"
  else
    let flags = Char.code s.[32] in
    let sign_count = Int32.to_int (String.get_int32_be s 33) land 0xFFFF_FFFF in
    let* () =
      if flags land 0x10 <> 0 && flags land 0x08 = 0 then
        Error "the flag BS with no flag BE"
      else Ok ()
    in
    let* attested, pos =
      if flags land 0x40 = 0 then Ok (None, 37)
      else if n < 37 + 18 then Error "no attested credential data"
      else
        let aaguid = String.sub s 37 16 in
        let len = String.get_uint16_be s 53 in
        let pos = 55 in
        if len = 0 || len > 1023 || len > n - pos then
          Error "a credential ID of a bad length"
        else
          let credential_id = String.sub s pos len in
          let pos = pos + len in
          match Cbor.decode s pos with
          | Error e ->
              Error ("the credential public key: " ^ e)
          | Ok (key, after) ->
              Ok
                ( Some
                    { aaguid
                    ; credential_id
                    ; public_key= String.sub s pos (after - pos)
                    ; key }
                , after )
    in
    let* extensions, pos =
      if flags land 0x80 = 0 then Ok (None, pos)
      else
        match Cbor.decode s pos with
        | Ok ((Cbor.Map _ as m), after) ->
            Ok (Some m, after)
        | Ok _ ->
            Error "the extensions are not a map"
        | Error e ->
            Error ("the extensions: " ^ e)
    in
    if pos <> n then Error "bytes after the authenticator data"
    else
      Ok {rp_id_hash= String.sub s 0 32; flags; sign_count; attested; extensions}
