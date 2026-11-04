exception Bad

let max_depth = 32

let is_digit c = c >= '0' && c <= '9'

let hex = function
  | '0' .. '9' as c ->
      Char.code c - Char.code '0'
  | 'a' .. 'f' as c ->
      Char.code c - Char.code 'a' + 10
  | 'A' .. 'F' as c ->
      Char.code c - Char.code 'A' + 10
  | _ ->
      raise Bad

(* [check s] raises [Bad] if [s] is not one JSON value of RFC 8259 with a depth
   of at most [max_depth]. Each function takes the index of its first byte and
   gives the index after its last byte. *)
let check s =
  let n = String.length s in
  let rec ws i =
    if i < n then
      match s.[i] with ' ' | '\t' | '\n' | '\r' -> ws (i + 1) | _ -> i
    else i
  in
  let byte i c = if i < n && s.[i] = c then i + 1 else raise Bad in
  let word i w =
    if i + String.length w <= n && String.sub s i (String.length w) = w then
      i + String.length w
    else raise Bad
  in
  (* One digit or more. *)
  let digits i =
    let rec go j = if j < n && is_digit s.[j] then go (j + 1) else j in
    let j = go i in
    if j = i then raise Bad else j
  in
  (* [number i] runs only at a '-' or a digit. *)
  let number i =
    let i = if s.[i] = '-' then i + 1 else i in
    let i = if i < n && s.[i] = '0' then i + 1 else digits i in
    let i = if i < n && s.[i] = '.' then digits (i + 1) else i in
    if i < n && (s.[i] = 'e' || s.[i] = 'E') then
      let i = i + 1 in
      digits (if i < n && (s.[i] = '+' || s.[i] = '-') then i + 1 else i)
    else i
  in
  (* [hex4 i] checks the four hexadecimal digits at [i], and gives the value of
     the first two: the ranges of the surrogates need only these. *)
  let hex4 i =
    if (i + 4 > n) [@mutate off "a closing quote comes after the digits"] then
      raise Bad
    else (
      ignore (hex s.[i + 2]) ;
      ignore (hex s.[i + 3]) ;
      (hex s.[i] lsl 4) lor hex s.[i + 1] )
  in
  let is_high u = u >= 0xD8 && u <= 0xDB in
  let is_low u = u >= 0xDC && u <= 0xDF in
  (* A string from its opening quote at [i]. *)
  let string i =
    let rec go i =
      if i >= n then raise Bad
      else
        match s.[i] with
        | '"' ->
            i + 1
        | '\\' -> (
            if i + 1 >= n then raise Bad
            else
              match s.[i + 1] with
              | '"' | '\\' | '/' | 'b' | 'f' | 'n' | 'r' | 't' ->
                  go (i + 2)
              | 'u' ->
                  let u = hex4 (i + 2) in
                  if is_high u then
                    if is_low (hex4 (byte (byte (i + 6) '\\') 'u')) then
                      go (i + 12)
                    else raise Bad
                  else if is_low u then raise Bad
                  else go (i + 6)
              | _ ->
                  raise Bad )
        | c when Char.code c < 0x20 ->
            raise Bad
        | _ ->
            go (i + 1)
    in
    go (byte i '"')
  in
  let rec value depth i =
    if depth > max_depth then raise Bad ;
    let i = ws i in
    if i >= n then raise Bad ;
    let i =
      match s.[i] with
      | '{' ->
          members depth (ws (i + 1))
      | '[' ->
          elements depth (ws (i + 1))
      | '"' ->
          string i
      | 't' ->
          word i "true"
      | 'f' ->
          word i "false"
      | 'n' ->
          word i "null"
      | '-' | '0' .. '9' ->
          number i
      | _ ->
          raise Bad
    in
    ws i
  (* The members of an object, after its [{] and white space. *)
  and members depth i =
    if i < n && s.[i] = '}' then i + 1
    else
      let rec go i =
        let i = ws (byte (ws (string i)) ':') in
        let i = value (depth + 1) i in
        if i < n && s.[i] = ',' then go (ws (i + 1)) else byte i '}'
      in
      go i
  (* The elements of an array, after its [\[] and white space. *)
  and elements depth i =
    if i < n && s.[i] = ']' then i + 1
    else
      let rec go i =
        let i = value (depth + 1) i in
        if i < n && s.[i] = ',' then go (ws (i + 1)) else byte i ']'
      in
      go i
  in
  if value 1 0 <> n then raise Bad

exception Unclean of string

(* [clean v] raises [Unclean] if a number of [v] is not finite (a float such as
   [1e400], or an integer of many digits), or if an object of [v] has two
   members of the same name. The sort makes the check of an object [n log n]. *)
let rec clean = function
  | `Float f when not (Float.is_finite f) ->
      raise (Unclean "a number that is not finite")
  | `Intlit s when not (Float.is_finite (float_of_string s)) ->
      raise (Unclean "a number that is not finite")
  | `Assoc l ->
      let rec distinct = function
        | a :: (b :: _ as rest) ->
            if a = b then raise (Unclean "a name two times") ;
            distinct rest
        | _ ->
            ()
      in
      distinct (List.sort String.compare (List.map fst l)) ;
      List.iter (fun (_, v) -> clean v) l
  | `List l ->
      List.iter clean l
  | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ ->
      ()

let parse s =
  match check s with
  | exception Bad ->
      Error "not JSON"
  | () when not (String.is_valid_utf_8 s) ->
      Error "not UTF-8"
  | () -> (
    match Yojson.Safe.from_string s with
    | exception Yojson.Json_error m ->
        (* No text that the check accepts is known to get here. *)
        Error ("not JSON: " ^ m)
    | v -> (
      match clean v with () -> Ok v | exception Unclean m -> Error m ) )
