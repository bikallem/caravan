let fail e = raise (H1_error.E e)
let bad s = fail (H1_error.Bad_request s)

(* A body as read from the connection: [read] has [Body.of_source]'s
   contract, [finished ()] is true once the body was read to its end,
   [unread ()] is the number of bytes left, if known, and [failed ()] is true
   once a read raised. *)
type source = {
  read : bytes -> int -> int -> int;
  finished : unit -> bool;
  unread : unit -> int option;
  failed : unit -> bool;
  trailers : unit -> Headers.t;
  length : int option;
}

(* A read that raised can leave the framing half consumed: reading on would
   take the rest of a malformed body as framing, and what follows it as the
   next request. So the first error is final: later reads raise it again, and
   the body never counts as finished. *)
let fail_for_good src =
  let error = ref None in
  let read buf off len =
    match !error with
    | Some (e, bt) -> Printexc.raise_with_backtrace e bt
    | None -> (
        try src.read buf off len
        with e ->
          let bt = Printexc.get_raw_backtrace () in
          error := Some (e, bt);
          Printexc.raise_with_backtrace e bt)
  in
  let ok () = Option.is_none !error in
  { src with read; finished = (fun () -> ok () && src.finished ());
             unread = (fun () -> if ok () then src.unread () else None);
             failed = (fun () -> not (ok ())) }

let fixed_source r n =
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
  { read; finished = (fun () -> !remaining = 0); unread = (fun () -> Some !remaining);
    failed = (fun () -> false); trailers = (fun () -> Headers.empty); length = Some n }


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

(* [max_body] bounds the body's bytes as sent, framing included: chunk
   extensions could otherwise carry any amount of bytes in tiny chunks. *)
let chunked_source ?(max_body = max_int) ~(limits : H1_parse.limits) r =
  let state = ref `Size and trailers = ref Headers.empty in
  let start = Buf_reader.consumed r in
  let sent () = Buf_reader.consumed r - start in
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
          if n > max_body - sent () then fail H1_error.Content_too_large;
          state := `Data n;
          read buf off len
        end
    | `Data n ->
        let k = Buf_reader.read_into r buf off (Int.min len n) in
        if k = 0 then raise Body.Truncated;
        if k = n
        then begin
          ignore (line ~max:0 ~error:"missing CRLF after chunk data");
          state := `Size
        end
        else state := `Data (n - k);
        k
  in
  let finished () = !state = `Done in
  { read; finished; unread = (fun () -> if finished () then Some 0 else None);
    failed = (fun () -> false); trailers = (fun () -> !trailers); length = None }

(* A reset is not a correct end of a body that ends at the close. Only
   [to_body] uses this source, and it reads only [read], [length] and
   [trailers]. *)
let until_close_source r =
  let read buf off len =
    try Buf_reader.read_into r buf off len with Flow.Closed -> raise Body.Truncated
  in
  { read; finished = (fun () -> false); unread = (fun () -> None); failed = (fun () -> false);
    trailers = (fun () -> Headers.empty); length = None }

let to_body src = Body.of_source ?length:src.length ~trailers:src.trailers src.read
let fixed r n = to_body (fail_for_good (fixed_source r n))
let chunked ?max_body ~limits r = to_body (fail_for_good (chunked_source ?max_body ~limits r))
let until_close r = to_body (fail_for_good (until_close_source r))

(* The request body handle *)

type continue = No_expectation | Expected of (unit -> unit) | Sent

type t = {
  r : Buf_reader.t;
  body : Body.t;
  src : source option;
  continue : continue ref;
  completed : unit Affect.Cell.Once.t; (* set when the handler's reads reach the end *)
}

let make ?max_body ?expect_continue ~limits r (framing : H1_parse.framing) =
  let raw =
    match framing with
    | No_body -> None
    | Fixed n ->
        (match max_body with
        | Some max when n > max -> fail H1_error.Content_too_large
        | _ -> Some (fixed_source r n))
    | Chunked -> Some (chunked_source ?max_body ~limits r)
  in
  let continue =
    ref (match expect_continue with None -> No_expectation | Some f -> Expected f)
  in
  let completed = Affect.Cell.Once.make () in
  (* Reads send 100 Continue first, if it is expected. A read that times out
     is the client's fault: 408. A reset connection is a client that left.
     Whatever fails, fails for good: [failed] then tells that the request is
     to blame. *)
  let wrap raw =
    let read buf off len =
      (match !continue with
      | Expected f -> continue := Sent; f ()
      | No_expectation | Sent -> ());
      let n =
        try raw.read buf off len with
        | Flow.Timeout -> fail H1_error.Request_timeout
        | Flow.Closed -> raise Body.Truncated
      in
      if raw.finished () then Affect.Cell.Once.try_set completed ();
      n
    in
    fail_for_good { raw with read }
  in
  let src = Option.map wrap raw in
  (* No body, or [Content-Length: 0]: nothing to read. *)
  if Option.fold ~none:true ~some:(fun src -> src.finished ()) src
  then Affect.Cell.Once.try_set completed ();
  { r; body = Option.fold ~none:Body.empty ~some:to_body src; src; continue; completed }

let body t = t.body
let completed t = Affect.Cell.Once.get' t.completed
let is_complete t = match t.src with None -> true | Some src -> src.finished ()
let unread t = match t.src with None -> Some 0 | Some src -> src.unread ()
let failed t = match t.src with None -> false | Some src -> src.failed ()

let continue_owed t =
  match !(t.continue) with Expected _ -> not (is_complete t) | No_expectation | Sent -> false

let drain t ~max =
  match t.src with
  | None -> true
  | Some src when src.failed () -> false
  | Some src when src.finished () -> true
  | Some _ when continue_owed t -> false (* the client may be holding the body back *)
  | Some src when Option.fold ~none:false ~some:(fun n -> n > max) (src.unread ()) -> false
  | Some src ->
      (* [max] bounds the bytes taken from the connection, framing
         included, so that chunk extensions cannot stretch it. *)
      let buf = Bytes.create 4096 and start = Buf_reader.consumed t.r in
      let rec discard () =
        let left = max - (Buf_reader.consumed t.r - start) in
        left >= 0
        && match src.read buf 0 (1 + Int.min (Bytes.length buf - 1) left) with
        | 0 -> true
        | _ -> discard ()
      in
      (try discard () with Body.Truncated | H1_error.E _ -> false)
