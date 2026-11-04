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

(* [plain s i j] is [true] if [s.[i..j-1]] has no [+] and no [%]: its decode is
   the same text. *)
let rec plain s i j =
  i = j || match s.[i] with '+' | '%' -> false | _ -> plain s (i + 1) j

(* The decode of WHATWG of [s.[i..j-1]]. It is different from the decode of the
   router on purpose: here, [+] is a space and an incorrect [%] stays as it is.
   In a path, [+] is a [+] and the router refuses an incorrect [%]. *)

let decode s i j =
  if plain s i j then String.sub s i (j - i)
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

(* [same s i name] is [true] if [s] has [name] at [i]. *)
let same s i name =
  let rec go k =
    k = String.length name || (s.[i + k] = name.[k] && go (k + 1))
  in
  go 0

let find_text name s =
  let n = String.length s in
  (* The value of the pair [s.[i..j-1]], if its name is [name]. A plain name is
     compared in place, with no copy. *)
  let pair i j =
    let rec eq k = if k = j || s.[k] = '=' then k else eq (k + 1) in
    let e = eq i in
    let found =
      if plain s i e then e - i = String.length name && same s i name
      else String.equal (decode s i e) name
    in
    if not found then None
    else if e = j then Some ""
    else Some (decode s (e + 1) j)
  in
  let rec go start k =
    if k = n || s.[k] = '&' then
      match if k > start then pair start k else None with
      | Some _ as v ->
          v
      | None ->
          if k = n then None else go (k + 1) (k + 1)
    else go start (k + 1)
  in
  go 0 0

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
