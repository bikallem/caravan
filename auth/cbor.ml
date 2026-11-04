type t =
  | Int of int
  | Bytes of string
  | Text of string
  | Array of t list
  | Map of (t * t) list
  | Bool of bool
  | Null

exception Bad of string

let max_depth = 16

let max_pairs = 64

let decode s pos =
  let n = String.length s in
  let byte i =
    if i >= n then raise (Bad "the input ends") else Char.code s.[i]
  in
  (* [arg ai i] is the argument of the additional information [ai], and the
     position after it. An argument over [max_int] is an error. *)
  let arg ai i =
    let big k =
      let rec go v j =
        if j = k then v else go ((v lsl 8) lor byte (i + j)) (j + 1)
      in
      go 0 0
    in
    match ai with
    | ai when ai < 24 ->
        (ai, i)
    | 24 ->
        (byte i, i + 1)
    | 25 ->
        (big 2, i + 2)
    | 26 ->
        (big 4, i + 4)
    | 27 ->
        (* 8 bytes: a value of 2^62 or more is not an OCaml int. *)
        if byte i >= 0x40 then raise (Bad "an integer out of range")
        else (big 8, i + 8)
    | 31 ->
        raise (Bad "an indefinite length")
    | _ ->
        raise (Bad "a reserved additional information")
  in
  let rec item depth i =
    if depth > max_depth then raise (Bad "too deep") ;
    let b = byte i in
    let ai = b land 31 in
    match b lsr 5 with
    | 0 ->
        let v, i = arg ai (i + 1) in
        (Int v, i)
    | 1 ->
        let v, i = arg ai (i + 1) in
        (Int (-1 - v), i)
    | (2 | 3) as major ->
        let len, i = arg ai (i + 1) in
        if len > n - i then raise (Bad "a length over the input") ;
        let str = String.sub s i len in
        if major = 2 then (Bytes str, i + len)
        else if String.is_valid_utf_8 str then (Text str, i + len)
        else raise (Bad "a text that is not UTF-8")
    | 4 ->
        let len, i = arg ai (i + 1) in
        (* Each item has one byte or more. *)
        if len > n - i then raise (Bad "a length over the input") ;
        let rec items k i acc =
          if k = 0 then (Array (List.rev acc), i)
          else
            let v, i = item (depth + 1) i in
            items (k - 1) i (v :: acc)
        in
        items len i []
    | 5 ->
        let len, i = arg ai (i + 1) in
        (* WebAuthn maps are small: no map has more than [max_pairs]. *)
        if len > max_pairs then raise (Bad "a map with too many pairs") ;
        if len > (n - i) / 2 then raise (Bad "a length over the input") ;
        let rec pairs k i acc =
          if k = 0 then (Map (List.rev acc), i)
          else
            let key, i = item (depth + 1) i in
            ( match key with
            | Int _ | Text _ ->
                ()
            | _ ->
                raise (Bad "a map key that is not an integer or a text") ) ;
            (* At most 64 pairs: a list is enough. *)
            if List.mem_assoc key acc then raise (Bad "two equal keys in a map") ;
            let v, i = item (depth + 1) i in
            pairs (k - 1) i ((key, v) :: acc)
        in
        pairs len i []
    | 6 ->
        raise (Bad "a tag")
    | _ -> (
      match ai with
      | 20 ->
          (Bool false, i + 1)
      | 21 ->
          (Bool true, i + 1)
      | 22 ->
          (Null, i + 1)
      | _ ->
          raise (Bad "a float or a simple value") )
  in
  match if pos < 0 then raise (Bad "a negative position") else item 1 pos with
  | v ->
      Ok v
  | exception Bad m ->
      Error m

let decode_all s =
  match decode s 0 with
  | Ok (v, i) when i = String.length s ->
      Ok v
  | Ok _ ->
      Error "bytes after the item"
  | Error e ->
      Error e
