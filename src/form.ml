type t = (string * string) list

(* The value of a hexadecimal digit, or -1. No option, thus no allocation. *)
let hex = function
  | '0' .. '9' as c ->
      Char.code c - Char.code '0'
  | 'a' .. 'f' as c ->
      Char.code c - Char.code 'a' + 10
  | 'A' .. 'F' as c ->
      Char.code c - Char.code 'A' + 10
  | _ ->
      -1

(* The decode of WHATWG of [s.[i..j-1]]. It is different from the decode of the
   router on purpose: here, [+] is a space and an incorrect [%] stays as it is.
   In a path, [+] is a [+] and the router refuses an incorrect [%]. *)
let decode s i j =
  let rec plain k =
    k = j || match s.[k] with '+' | '%' -> false | _ -> plain (k + 1)
  in
  if plain i then String.sub s i (j - i)
  else
    let b =
      Bytes.create
        (( j - i )
         [@mutate
           off "the size of a scratch buffer: a larger one gives the same"] )
    in
    let rec go k w =
      if k = j then w
      else
        match s.[k] with
        | '+' ->
            Bytes.set b w ' ' ;
            go (k + 1) (w + 1)
        | '%' when k + 2 < j && hex s.[k + 1] >= 0 && hex s.[k + 2] >= 0 ->
            Bytes.set b w (Char.chr ((hex s.[k + 1] * 16) + hex s.[k + 2])) ;
            go (k + 3) (w + 1)
        | c ->
            Bytes.set b w c ;
            go (k + 1) (w + 1)
    in
    Bytes.sub_string b 0 (go i 0)

(* [fold f s acc] gives [f i j acc] for each non-empty sequence [s.[i..j-1]]
   between the [&] characters, in order. *)
let fold f s acc =
  let n = String.length s in
  let rec go start k acc =
    if k = n then if k > start then f start k acc else acc
    else if s.[k] = '&' then
      go (k + 1) (k + 1) (if k > start then f start k acc else acc)
    else go start (k + 1) acc
  in
  go 0 0 acc

let parse s =
  let pair i j acc =
    (* The search stops at the end of the pair: a search to the end of [s] makes
       the parse quadratic. *)
    let rec eq k = if k = j || s.[k] = '=' then k else eq (k + 1) in
    let e = eq i in
    if e = j then (decode s i j, "") :: acc
    else (decode s i e, decode s (e + 1) j) :: acc
  in
  List.rev (fold pair s [])

let count s = fold (fun _ _ n -> n + 1) s 0

let digits = "0123456789ABCDEF"

let escape b s =
  String.iter
    (function
      | ('A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '*' | '-' | '.' | '_') as c ->
          Buffer.add_char b c
      | ' ' ->
          Buffer.add_char b '+'
      | c ->
          Buffer.add_char b '%' ;
          Buffer.add_char b digits.[Char.code c lsr 4] ;
          Buffer.add_char b digits.[Char.code c land 15] )
    s

let encode l =
  let b = Buffer.create 64 in
  List.iteri
    (fun i (name, value) ->
      if i > 0 then Buffer.add_char b '&' ;
      escape b name ;
      Buffer.add_char b '=' ;
      escape b value )
    l ;
  Buffer.contents b

let find name l =
  List.find_map (fun (n, v) -> if String.equal n name then Some v else None) l

let find_all name l =
  List.filter_map (fun (n, v) -> if String.equal n name then Some v else None) l

let mem name l = List.exists (fun (n, _) -> String.equal n name) l

let media_type = "application/x-www-form-urlencoded"

type error = Unsupported_media_type | Too_large | Too_many_fields

let error_status = function
  | Unsupported_media_type ->
      Status.unsupported_media_type
  | Too_large | Too_many_fields ->
      Status.content_too_large

let pp_error ppf e =
  Format.pp_print_string ppf
    ( match e with
    | Unsupported_media_type ->
        "Unsupported_media_type"
    | Too_large ->
        "Too_large"
    | Too_many_fields ->
        "Too_many_fields" )
