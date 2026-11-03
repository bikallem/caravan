(* RFC 9110 §5.6.2: token = 1*tchar *)
let is_tchar = function
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '0' .. '9'
  | '!'
  | '#'
  | '$'
  | '%'
  | '&'
  | '\''
  | '*'
  | '+'
  | '-'
  | '.'
  | '^'
  | '_'
  | '`'
  | '|'
  | '~' ->
      true
  | _ ->
      false

let is_token s = s <> "" && String.for_all is_tchar s

(* RFC 9110 §5.6.3: OWS is spaces and tabs. *)
let is_ws c = c = ' ' || c = '\t'
