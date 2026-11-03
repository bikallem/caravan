let is_ws c = c = ' ' || c = '\t'

(* The first index of [s.[i..j-1]] that is not a space or a tab, or [j]. *)
let rec left s i j = if i < j && is_ws s.[i] then left s (i + 1) j else i

(* The end of [s.[i..j-1]] with no spaces or tabs at its end. *)
let rec right s i j = if j > i && is_ws s.[j - 1] then right s i (j - 1) else j

(* The index of the first [=] of [s.[i..j-1]], or [j]. *)
let index_eq s i j =
  let rec eq k = if k = j || s.[k] = '=' then k else eq (k + 1) in
  eq i

(* The value of the part [s.[i..j-1]] with its [=] at [e]: no spaces or tabs at
   the two ends, then no quotes. *)
let value s e j =
  let vi = left s (e + 1) j in
  let vj = right s vi j in
  if vj - vi >= 2 && s.[vi] = '"' && s.[vj - 1] = '"' then
    String.sub s (vi + 1) (vj - vi - 2)
  else String.sub s vi (vj - vi)

(* The pair of the part [s.[i..j-1]], if any. The trim of the name and of the
   value also trims the two ends of the part. *)
let pair s i j =
  let e = index_eq s i j in
  if e = j then None
  else
    let ni = left s i e in
    let nj = right s ni e in
    if ni = nj then None else Some (String.sub s ni (nj - ni), value s e j)

let parse s =
  let n = String.length s in
  let rec go start k acc =
    if k = n || s.[k] = ';' then
      let acc = match pair s start k with Some p -> p :: acc | None -> acc in
      if k = n then List.rev acc else go (k + 1) (k + 1) acc
    else go start (k + 1) acc
  in
  go 0 0 []

(* [is_name name s i e] is [true] iff the name of the part [s.[i..e-1]] is
   [name], with no copy. *)
let is_name name s i e =
  let ni = left s i e in
  let nj = right s ni e in
  let n = String.length name in
  ni < nj
  && nj - ni = n
  &&
  let rec same k = k = n || (s.[ni + k] = name.[k] && same (k + 1)) in
  same 0

let find name s =
  let n = String.length s in
  let rec go start k =
    if k = n || s.[k] = ';' then
      let e = index_eq s start k in
      if e < k && is_name name s start e then Some (value s e k)
      else if k = n then None
      else go (k + 1) (k + 1)
    else go start (k + 1)
  in
  go 0 0

let parse_set_cookie s =
  pair s 0 (Option.value ~default:(String.length s) (String.index_opt s ';'))

type same_site = [`Strict | `Lax | `None | `Unspecified]

type t =
  { name: string
  ; value: string
  ; expires: Weft.Clock.posix option
  ; max_age: int option
  ; domain: string option
  ; path: string
  ; secure: bool
  ; http_only: bool
  ; same_site: same_site
  ; partitioned: bool }

(* RFC 6265 §4.1.1. *)
let is_octet = function
  | '\x21'
  | '\x23' .. '\x2b'
  | '\x2d' .. '\x3a'
  | '\x3c' .. '\x5b'
  | '\x5d' .. '\x7e' ->
      true
  | _ ->
      false

let is_domain_char = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '.' | '_' ->
      true
  | _ ->
      false

(* No space: the user agent trims the value of an attribute, and a field value
   cannot end with a space. *)
let is_path_char c = c > '\x20' && c <= '\x7e' && c <> ';'

(* Labels that are not empty, after one optional dot at the start. *)
let is_domain d =
  let d =
    if String.starts_with ~prefix:"." d then String.sub d 1 (String.length d - 1)
    else d
  in
  String.for_all is_domain_char d
  && List.for_all (fun l -> l <> "") (String.split_on_char '.' d)

(* A user agent ignores a larger cookie or attribute (6265bis). *)
let max_pair = 4096

let max_attribute = 1024

let fail fn name m = invalid_arg (Printf.sprintf "Cookie.%s: %S: %s" fn name m)

let check_value fn name value =
  if not (String.for_all is_octet value) then
    fail fn name "the value has a byte that is not a cookie-octet" ;
  if String.length name + String.length value > max_pair then
    fail fn name
      (Printf.sprintf "the name and the value have more than %d bytes" max_pair)

(* [has_prefix p name] is [true] iff [name] starts with [p], with a compare that
   ignores the case. [p] is in lower case. *)
let has_prefix p name =
  let n = String.length p in
  String.length name >= n
  &&
  let rec same i =
    i = n || (Char.lowercase_ascii name.[i] = p.[i] && same (i + 1))
  in
  same 0

let make ?expires ?max_age ?domain ?(path = "/") ?(secure = false)
    ?(http_only = true) ?(same_site = `Lax) ?(partitioned = false) name value =
  let fail = fail "make" name in
  if not (Token.is_token name) then fail "the name is not a token" ;
  check_value "make" name value ;
  Option.iter
    (fun d ->
      if String.length d > max_attribute || not (is_domain d) then
        fail "incorrect domain" )
    domain ;
  if
    path = ""
    || path.[0] <> '/'
    || String.length path > max_attribute
    || not (String.for_all is_path_char path)
  then fail "incorrect path" ;
  Option.iter (fun a -> if a < 0 then fail "negative max_age") max_age ;
  if same_site = `None && not secure then fail "SameSite=None needs secure" ;
  if partitioned && not secure then fail "partitioned needs secure" ;
  if has_prefix "__secure-" name && not secure then
    fail "the prefix __Secure- needs secure" ;
  if
    has_prefix "__host-" name
    && not (secure && String.equal path "/" && Option.is_none domain)
  then fail "the prefix __Host- needs secure, the path / and no domain" ;
  { name
  ; value
  ; expires
  ; max_age
  ; domain
  ; path
  ; secure
  ; http_only
  ; same_site
  ; partitioned }

let with_value value c =
  check_value "with_value" c.name value ;
  {c with value}

let expired c = {c with value= ""; max_age= Some 0; expires= Some Date.epoch}

let attributes b c =
  let add = Buffer.add_string b in
  Option.iter
    (fun e ->
      add "; Expires=" ;
      add (Date.to_imf e) )
    c.expires ;
  Option.iter
    (fun a ->
      add "; Max-Age=" ;
      add (string_of_int a) )
    c.max_age ;
  Option.iter (fun d -> add "; Domain=" ; add d) c.domain ;
  add "; Path=" ;
  add c.path ;
  if c.secure then add "; Secure" ;
  if c.http_only then add "; HttpOnly" ;
  ( match c.same_site with
  | `Strict ->
      add "; SameSite=Strict"
  | `Lax ->
      add "; SameSite=Lax"
  | `None ->
      add "; SameSite=None"
  | `Unspecified ->
      () ) ;
  if c.partitioned then add "; Partitioned"

let to_string c =
  let b =
    Buffer.create
      (( String.length c.name + String.length c.value + 64 )
       [@mutate off "a capacity: another size gives the same text"] )
  in
  Buffer.add_string b c.name ;
  Buffer.add_char b '=' ;
  Buffer.add_string b c.value ;
  attributes b c ;
  Buffer.contents b

let pp ppf c =
  let b = Buffer.create 64 in
  Printf.bprintf b "%s=<%d bytes>" c.name (String.length c.value) ;
  attributes b c ;
  Format.pp_print_string ppf (Buffer.contents b)
