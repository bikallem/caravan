(* RFC 3986 §2.1: percent-encoding. *)

let hex = function
  | '0' .. '9' as c ->
      Some (Char.code c - Char.code '0')
  | 'a' .. 'f' as c ->
      Some (Char.code c - Char.code 'a' + 10)
  | 'A' .. 'F' as c ->
      Some (Char.code c - Char.code 'A' + 10)
  | _ ->
      None

(* [decode s] is [s] with each [%XX] replaced by its byte, or [None] for a bad
   sequence. *)
let decode s =
  if not (String.contains s '%') then Some s
  else
    let n = String.length s in
    let b = Buffer.create n in
    let rec go i =
      if i = n then Some (Buffer.contents b)
      else if s.[i] <> '%' then (
        Buffer.add_char b s.[i] ;
        go (i + 1) )
      else if i + 2 >= n then None
      else
        match (hex s.[i + 1], hex s.[i + 2]) with
        | Some h, Some l ->
            Buffer.add_char b (Char.chr ((h * 16) + l)) ;
            go (i + 3)
        | _ ->
            None
    in
    go 0
