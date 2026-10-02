type limits = {
  max_request_line : int;
  max_header_bytes : int;
  max_header_count : int;
}

let default_limits =
  { max_request_line = 8192; max_header_bytes = 16384; max_header_count = 100 }

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
