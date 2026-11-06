let after prefix html =
  let n = String.length prefix in
  let rec go i =
    if i + n > String.length html then raise Not_found
    else if String.sub html i n = prefix then
      let start = i + n in
      String.sub html start (String.index_from html start '"' - start)
    else go (i + 1)
  in
  go 0

let csrf_token html = after {|name="_csrf" value="|} html

let base64url c =
  match c with
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' ->
      true
  | _ ->
      false

let grep_runs n html =
  let rec go i acc =
    if i + n > String.length html then List.rev acc
    else
      let s = String.sub html i n in
      if String.for_all base64url s then go (i + n) (s :: acc)
      else go (i + 1) acc
  in
  go 0 []
