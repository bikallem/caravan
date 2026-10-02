let fixed r n =
  let remaining = ref n in
  let read buf off len =
    if !remaining = 0
    then 0
    else begin
      match Buf_reader.read_into r buf off (Int.min len !remaining) with
      | 0 -> raise Body.Truncated
      | k -> remaining := !remaining - k; k
    end
  in
  Body.of_source ~length:n read

let fail e = raise (H1_error.E e)
let bad s = fail (H1_error.Bad_request s)

(* Chunk size line, RFC 9112 §7.1.1:
   chunk-size [ chunk-ext ], chunk-size = 1*HEXDIG,
   chunk-ext = *( BWS ";" BWS ext-name [ BWS "=" BWS ext-val ] ),
   ext-val = token / quoted-string *)

let is_hex = function '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true | _ -> false
let is_qdtext = function '\t' | ' ' | '!' | '#' .. '[' | ']' .. '~' | '\x80' .. '\xff' -> true | _ -> false

let chunk_size line =
  let n = String.length line in
  let rec skip p i = if i < n && p line.[i] then skip p (i + 1) else i in
  let skip_ws = skip (fun c -> c = ' ' || c = '\t') in
  let token i =
    let j = skip Token.is_tchar i in
    if j = i then bad "invalid chunk extension";
    j
  in
  let rec quoted i =
    if i >= n then bad "unterminated chunk extension value"
    else match line.[i] with
    | '"' -> i + 1
    | '\\' when i + 1 < n && (is_qdtext line.[i + 1] || line.[i + 1] = '"' || line.[i + 1] = '\\') ->
        quoted (i + 2)
    | c when is_qdtext c -> quoted (i + 1)
    | _ -> bad "invalid chunk extension value"
  in
  let rec extensions i =
    if i < n
    then begin
      let i = skip_ws i in
      if i >= n || line.[i] <> ';' then bad "invalid chunk size line";
      let i = token (skip_ws (i + 1)) in
      let k = skip_ws i in
      if k < n && line.[k] = '='
      then begin
        let v = skip_ws (k + 1) in
        extensions (if v < n && line.[v] = '"' then quoted (v + 1) else token v)
      end
      else extensions i
    end
  in
  let digits = skip is_hex 0 in
  if digits = 0 then bad "invalid chunk size";
  if digits > 15 then bad "chunk size too large";
  extensions digits;
  int_of_string ("0x" ^ String.sub line 0 digits)

let max_chunk_line = 4096

let chunked ?(max_body = max_int) ~(limits : H1_parse.limits) r =
  let state = ref `Size and total = ref 0 and trailers = ref Headers.empty in
  let no_eof f = try f () with Buf_reader.Eof -> raise Body.Truncated in
  let line ~max ~error =
    no_eof (fun () ->
        try Buf_reader.read_line r ~max
        with Buf_reader.Line_too_long | Buf_reader.Bad_line_ending -> bad error)
  in
  let rec read buf off len =
    match !state with
    | `Done -> 0
    | `Size ->
        let n = chunk_size (line ~max:max_chunk_line ~error:"invalid chunk size line") in
        if n = 0
        then begin
          trailers :=
            no_eof (fun () ->
                H1_parse.field_lines ~max_bytes:limits.max_header_bytes
                  ~max_count:limits.max_header_count r);
          state := `Done;
          0
        end
        else begin
          if n > max_body - !total then fail H1_error.Content_too_large;
          state := `Data n;
          read buf off len
        end
    | `Data n ->
        let k = Buf_reader.read_into r buf off (Int.min len n) in
        if k = 0 then raise Body.Truncated;
        total := !total + k;
        if k = n
        then begin
          ignore (line ~max:0 ~error:"missing CRLF after chunk data");
          state := `Size
        end
        else state := `Data (n - k);
        k
  in
  Body.of_source ~trailers:(fun () -> !trailers) read

let of_framing ?max_body ~limits r : H1_parse.framing -> Body.t = function
| No_body -> Body.empty
| Fixed n ->
    (match max_body with
    | Some max when n > max -> fail H1_error.Content_too_large
    | _ -> fixed r n)
| Chunked -> chunked ?max_body ~limits r
