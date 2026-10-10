type framing = No_body | Length of int | Chunked | Close_delimited | Tunnel

type response =
  { head: string
  ; status_line: string
  ; code: int
  ; fields: (string * string) list
  ; framing: framing
  ; body: string }

type t = {responses: response list; rest: string; fin: bool; reset: bool}

exception Malformed of string

let malformed fmt = Printf.ksprintf (fun s -> raise (Malformed s)) fmt

let final t = List.nth t.responses (List.length t.responses - 1)

let find_all name r =
  List.filter_map
    (fun (n, v) ->
      if String.equal (String.lowercase_ascii n) (String.lowercase_ascii name)
      then Some v
      else None )
    r.fields

let find name r = match find_all name r with v :: _ -> Some v | [] -> None

let is_digit c = c >= '0' && c <= '9'

let is_ows c = c = ' ' || c = '\t'

let trim_ows s =
  let n = String.length s in
  let i = ref 0 and j = ref n in
  while !i < n && is_ows s.[!i] do
    incr i
  done ;
  while !j > !i && is_ows s.[!j - 1] do
    decr j
  done ;
  String.sub s !i (!j - !i)

(* The index of the first CRLF at or after [pos], if any. *)
let find_crlf s pos =
  let n = String.length s in
  let rec go i =
    if i + 1 >= n then None
    else if s.[i] = '\r' && s.[i + 1] = '\n' then Some i
    else go (i + 1)
  in
  go pos

(* The lines of a head without its last empty line. A line can contain a bare CR
   or LF: only CRLF ends a line. *)
let lines head =
  let rec go pos acc =
    match find_crlf head pos with
    | Some i when i = pos ->
        List.rev acc
    | Some i ->
        go (i + 2) (String.sub head pos (i - pos) :: acc)
    | None ->
        malformed "no empty line"
  in
  go 0 []

(* "HTTP/" DIGIT "." DIGIT SP 3DIGIT SP reason *)
let status_code line =
  let ok =
    String.length line >= 13
    && String.sub line 0 5 = "HTTP/"
    && is_digit line.[5]
    && line.[6] = '.'
    && is_digit line.[7]
    && line.[8] = ' '
    && is_digit line.[9]
    && is_digit line.[10]
    && is_digit line.[11]
    && line.[12] = ' '
  in
  if not ok then malformed "malformed status line %S" line ;
  int_of_string (String.sub line 9 3)

let field_line line =
  match String.index_opt line ':' with
  | None ->
      malformed "field line without a colon %S" line
  | Some i ->
      ( String.sub line 0 i
      , trim_ows (String.sub line (i + 1) (String.length line - i - 1)) )

let content_length fields =
  let values =
    List.filter_map
      (fun (n, v) ->
        if String.lowercase_ascii n = "content-length" then Some v else None )
      fields
  in
  match values with
  | [] ->
      None
  | v :: _ ->
      List.iter
        (fun v' ->
          if
            v' = ""
            || (not (String.for_all is_digit v'))
            || String.length v' > 18
            || v' <> v
          then malformed "bad Content-Length values" )
        values ;
      Some (int_of_string v)

(* The transfer codings of all Transfer-Encoding fields, in order. *)
let codings fields =
  List.concat_map
    (fun (n, v) ->
      if String.lowercase_ascii n = "transfer-encoding" then
        List.map
          (fun c -> String.lowercase_ascii (trim_ows c))
          (String.split_on_char ',' v)
      else [] )
    fields

(* Decodes a chunked body that starts at [pos]. Gives the content and the
   position after the last CRLF. *)
let chunked s pos =
  let content = Buffer.create 64 in
  let line pos =
    match find_crlf s pos with
    | Some i ->
        (String.sub s pos (i - pos), i + 2)
    | None ->
        malformed "chunked body: no CRLF"
  in
  let rec chunk pos =
    let size_line, pos = line pos in
    let size =
      let hex =
        match String.index_opt size_line ';' with
        | Some i ->
            String.sub size_line 0 i
        | None ->
            size_line
      in
      match int_of_string_opt ("0x" ^ hex) with
      | Some n when n >= 0 && hex <> "" ->
          n
      | _ ->
          malformed "chunked body: bad size %S" size_line
    in
    if size = 0 then trailers pos
    else if pos + size + 2 > String.length s then
      malformed "chunked body: short chunk"
    else if String.sub s (pos + size) 2 <> "\r\n" then
      malformed "chunked body: no CRLF after the chunk"
    else (
      Buffer.add_string content (String.sub s pos size) ;
      chunk (pos + size + 2) )
  and trailers pos =
    let l, pos = line pos in
    if l = "" then pos else trailers pos
  in
  let pos = chunk pos in
  (Buffer.contents content, pos)

let parse_exn ~meth (out : Memory_server.output) =
  let s = out.written in
  let n = String.length s in
  let rec go pos acc =
    let head_end =
      let rec find i =
        if i + 3 >= n then malformed "no complete head"
        else if String.sub s i 4 = "\r\n\r\n" then i + 4
        else find (i + 1)
      in
      find pos
    in
    let head = String.sub s pos (head_end - pos) in
    let status_line, field_lines =
      match lines head with l :: ls -> (l, ls) | [] -> malformed "empty head"
    in
    let code = status_code status_line in
    let fields = List.map field_line field_lines in
    let response framing body =
      {head; status_line; code; fields; framing; body}
    in
    let finish framing body next =
      let r = response framing body in
      (List.rev (r :: acc), String.sub s next (n - next))
    in
    if code >= 100 && code < 200 && code <> 101 then
      go head_end (response No_body "" :: acc)
    else if code = 101 || (meth = "CONNECT" && code >= 200 && code < 300) then
      finish Tunnel "" head_end
    else if meth = "HEAD" || code = 204 || code = 304 then
      finish No_body "" head_end
    else
      match (codings fields, content_length fields) with
      | (_ :: _ as cs), _ when List.nth cs (List.length cs - 1) = "chunked" ->
          let body, next = chunked s head_end in
          finish Chunked body next
      | _ :: _, _ | [], None ->
          finish Close_delimited (String.sub s head_end (n - head_end)) n
      | [], Some len ->
          if head_end + len > n then malformed "short body" ;
          finish (Length len) (String.sub s head_end len) (head_end + len)
  in
  let responses, rest = go 0 [] in
  {responses; rest; fin= out.fin; reset= out.reset}

let parse ~meth out =
  match parse_exn ~meth out with t -> Ok t | exception Malformed m -> Error m
