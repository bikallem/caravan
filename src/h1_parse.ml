type limits = {
  max_request_line : int;
  max_header_bytes : int;
  max_header_count : int;
}

type head = {
  meth : Method.t;
  target : string;
  version : Version.t;
  headers : Headers.t;
}

let fail e = raise (H1_error.E e)
let bad fmt = Printf.ksprintf (fun s -> fail (H1_error.Bad_request s)) fmt

let read_line r ~max ~too_long =
  match Buf_reader.read_line r ~max with
  | line -> line
  | exception Buf_reader.Line_too_long -> fail too_long
  | exception Buf_reader.Bad_line_ending -> bad "bare CR or LF"

let is_digit = function '0' .. '9' -> true | _ -> false

(* RFC 9112 §2.3: HTTP-version = "HTTP/" DIGIT "." DIGIT. A later 1.x minor
   version is answered as 1.1 (RFC 9110 §6.2). *)
let version s =
  if String.length s <> 8
  || not (String.starts_with ~prefix:"HTTP/" s)
  || s.[6] <> '.'
  || not (is_digit s.[5] && is_digit s.[7])
  then bad "invalid HTTP version %S" s;
  match s.[5], s.[7] with
  | '1', '0' -> Version.Http_1_0
  | '1', _ -> Version.Http_1_1
  | '0', _ -> bad "unsupported HTTP version %S" s
  | _ -> fail H1_error.Version_not_supported

let is_target_char c = c >= '\x21' && c <= '\x7e'

let request_line limits r =
  let rec skip_empty n =
    match read_line r ~max:limits.max_request_line ~too_long:Uri_too_long with
    | "" when n < 4 -> skip_empty (n + 1)
    | "" -> bad "too many empty lines before the request line"
    | line -> line
  in
  match String.split_on_char ' ' (skip_empty 0) with
  | [ m; target; v ] ->
      let meth =
        try Method.of_string m with Invalid_argument _ -> bad "invalid method %S" m
      in
      if target = "" || not (String.for_all is_target_char target)
      then bad "invalid request target %S" target;
      (meth, target, version v)
  | _ -> bad "malformed request line"

let is_ows = function ' ' | '\t' -> true | _ -> false

let trim_ows s =
  let n = String.length s in
  let rec first i = if i < n && is_ows s.[i] then first (i + 1) else i in
  let rec last j = if j > i0 && is_ows s.[j - 1] then last (j - 1) else j
  and i0 = first 0 in
  String.sub s i0 (last n - i0)

(* RFC 9112 §5: field-line = field-name ":" OWS field-value OWS *)
let field_line line =
  if is_ows line.[0] then bad "obsolete line folding";
  match String.index_opt line ':' with
  | None -> bad "field line without a colon"
  | Some i ->
      let name = String.sub line 0 i in
      if not (Headers.is_valid_name name) then bad "invalid field name %S" name;
      let value = trim_ows (String.sub line (i + 1) (String.length line - i - 1)) in
      if not (Headers.is_valid_value value) then bad "invalid value for %s" name;
      (name, value)

let field_lines ~max_bytes ~max_count r =
  (* [budget] bytes remain for the next lines, CRLFs included. *)
  let rec loop budget count fields =
    let max = Int.max 0 (budget - 2) in
    match read_line r ~max ~too_long:Header_fields_too_large with
    | "" -> Headers.of_list (List.rev fields)
    | _ when count = max_count -> fail Header_fields_too_large
    | line ->
        loop (budget - String.length line - 2) (count + 1) (field_line line :: fields)
  in
  loop max_bytes 0 []

let request_head limits r =
  let meth, target, version = request_line limits r in
  let headers =
    field_lines ~max_bytes:limits.max_header_bytes ~max_count:limits.max_header_count r
  in
  { meth; target; version; headers }

(* Response heads, RFC 9112 §4 *)

type response_head = {
  version : Version.t;
  status : Status.t;
  reason : string;
  headers : Headers.t;
}

(* reason-phrase = 1*( HTAB / SP / VCHAR / obs-text ) *)
let is_reason_char c = c = '\t' || (c >= ' ' && c <> '\x7f')

(* status-line = HTTP-version SP status-code SP [ reason-phrase ]. Many
   servers end the line after the code, thus we also accept that. *)
let status_line limits r =
  let line =
    read_line r ~max:limits.max_request_line ~too_long:(Bad_request "status line too long")
  in
  let n = String.length line in
  if n < 12 || line.[8] <> ' ' then bad "malformed status line";
  let v = String.sub line 0 8 in
  let version =
    try version v with H1_error.E Version_not_supported -> bad "unsupported HTTP version %S" v
  in
  let code = String.sub line 9 3 in
  if not (String.for_all is_digit code) then bad "invalid status code %S" code;
  let code = int_of_string code in
  if code < 100 || code > 599 then bad "status code %d out of range" code;
  let reason =
    if n = 12 then ""
    else if line.[12] <> ' ' then bad "malformed status line"
    else String.sub line 13 (n - 13)
  in
  if not (String.for_all is_reason_char reason) then bad "invalid reason phrase";
  (version, Status.v code, reason)

let response_head limits r =
  let version, status, reason = status_line limits r in
  let headers =
    field_lines ~max_bytes:limits.max_header_bytes ~max_count:limits.max_header_count r
  in
  { version; status; reason; headers }

(* Framing, RFC 9112 §6 *)

type framing = No_body | Fixed of int | Chunked

(* At most 18 digits: always fits in an OCaml int. [s] is never empty:
   [Headers.tokens] drops empty list elements. *)
let length_value s =
  if String.length s > 18 || not (String.for_all is_digit s)
  then bad "invalid Content-Length %S" s;
  int_of_string s

let content_length headers =
  if not (Headers.mem "content-length" headers)
  then None
  else
  match List.map length_value (Headers.tokens "content-length" headers) with
  | [] -> bad "empty Content-Length"
  | n :: rest ->
      if List.exists (fun m -> m <> n) rest then bad "conflicting Content-Length";
      Some n

let request_framing (h : head) =
  let length = content_length h.headers in
  if not (Headers.mem "transfer-encoding" h.headers)
  then match length with None -> No_body | Some n -> Fixed n
  else begin
    if h.version = Version.Http_1_0 then bad "Transfer-Encoding in HTTP/1.0";
    if length <> None then bad "both Transfer-Encoding and Content-Length";
    match List.rev_map String.lowercase_ascii (Headers.tokens "transfer-encoding" h.headers) with
    | [ "chunked" ] -> Chunked
    | "chunked" :: rest when List.mem "chunked" rest -> bad "chunked applied twice"
    | "chunked" :: _ ->
        fail (H1_error.Not_implemented "transfer codings other than chunked")
    | _ -> bad "chunked is not the final transfer coding"
  end

type response_framing = No_body | Fixed of int | Chunked | Until_close | Tunnel

(* RFC 9112 §6.3, in its order. A response with the two framing fields is
   an error, as for requests: a proxy that guesses can be the victim of
   response smuggling. *)
let response_framing ~meth h =
  let code = Status.code h.status in
  if code = 101 || (meth = Method.CONNECT && code >= 200 && code < 300)
  then Tunnel
  else if meth = Method.HEAD || code < 200 || code = 204 || code = 304
  then No_body
  else begin
    let length = content_length h.headers in
    if not (Headers.mem "transfer-encoding" h.headers)
    then match length with None -> Until_close | Some n -> Fixed n
    else begin
      if length <> None then bad "both Transfer-Encoding and Content-Length";
      if h.version = Version.Http_1_0 then bad "Transfer-Encoding in HTTP/1.0";
      match List.map String.lowercase_ascii (Headers.tokens "transfer-encoding" h.headers) with
      | [ "chunked" ] -> Chunked
      | _ -> fail (H1_error.Not_implemented "transfer codings other than chunked")
    end
  end

(* Request target, RFC 9112 §3.2 *)

type target = { scheme : string; authority : string option; path : string }

(* RFC 3986: reg-name = *( unreserved / pct-encoded / sub-delims ) *)
let is_reg_name_char = function
| 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '.' | '_' | '~' | '%' | '!' | '$'
| '&' | '\'' | '(' | ')' | '*' | '+' | ',' | ';' | '=' -> true
| _ -> false

let is_ip_literal_char = function
| '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' | ':' | '.' -> true
| _ -> false

(* host [ ":" port ], with a non-empty host; [port] says whether a non-empty
   port is required. *)
let is_authority ?(port = false) s =
  let n = String.length s in
  let host_end =
    if n > 0 && s.[0] = '['
    then match String.index_opt s ']' with
    | Some i when i > 1 && String.for_all is_ip_literal_char (String.sub s 1 (i - 1)) ->
        Some (i + 1)
    | _ -> None
    else
    let i = Option.value (String.index_opt s ':') ~default:n in
    if i > 0 && String.for_all is_reg_name_char (String.sub s 0 i) then Some i else None
  in
  match host_end with
  | None -> false
  | Some i when i = n -> not port
  | Some i ->
      let digits = String.sub s (i + 1) (n - i - 1) in
      s.[i] = ':' && String.for_all is_digit digits && not (port && digits = "")

let host (h : head) =
  match Headers.find_all "host" h.headers with
  | [] -> if h.version = Version.Http_1_1 then bad "missing Host" else None
  | [ "" ] -> None
  | [ v ] -> if is_authority v then Some v else bad "invalid Host %S" v
  | _ -> bad "more than one Host"

let absolute_form t =
  let bad_target () = bad "invalid request target %S" t in
  match String.index_opt t ':' with
  | None -> bad_target ()
  | Some i ->
      let scheme = String.lowercase_ascii (String.sub t 0 i) in
      let n = String.length t in
      if not ((scheme = "http" || scheme = "https") && n > i + 2 && t.[i + 1] = '/' && t.[i + 2] = '/')
      then bad_target ();
      let start = i + 3 in
      let rec end_of_authority j =
        if j < n && t.[j] <> '/' && t.[j] <> '?' then end_of_authority (j + 1) else j
      in
      let j = end_of_authority start in
      let authority = String.sub t start (j - start) in
      if String.contains authority '@' then bad "userinfo in request target";
      if not (is_authority authority) then bad_target ();
      let path =
        match String.sub t j (n - j) with
        | "" -> "/"
        | p when p.[0] = '?' -> "/" ^ p
        | p -> p
      in
      { scheme; authority = Some authority; path }

let request_target ~scheme (h : head) =
  let authority = host h in
  let t = h.target in
  match h.meth with
  | Method.CONNECT ->
      if is_authority ~port:true t
      then { scheme; authority = Some t; path = "" }
      else bad "CONNECT needs a host:port target"
  | Method.OPTIONS when t = "*" -> { scheme; authority; path = "*" }
  | _ when t = "*" -> bad "* is only for OPTIONS"
  | _ when t.[0] = '/' -> { scheme; authority; path = t }
  | _ ->
      (* An https target over plain HTTP, or the reverse, would make the
         request claim a security it does not have. *)
      let target = absolute_form t in
      if target.scheme <> scheme then fail H1_error.Misdirected_request;
      target
