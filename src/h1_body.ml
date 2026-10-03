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
  fail_for_good
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

let chunked_source ?(max_body = max_int) ~(limits : H1_parse.limits) r =
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
  let finished () = !state = `Done in
  fail_for_good
    { read; finished; unread = (fun () -> if finished () then Some 0 else None);
      failed = (fun () -> false); trailers = (fun () -> !trailers); length = None }

let to_body src = Body.of_source ?length:src.length ~trailers:src.trailers src.read
let fixed r n = to_body (fixed_source r n)
let chunked ?max_body ~limits r = to_body (chunked_source ?max_body ~limits r)

(* The request body handle *)

type continue = No_expectation | Expected of (unit -> unit) | Sent

type t = {
  mutable body : Body.t;
  src : source option;
  mutable continue : continue;
  completed : unit Affect.Cell.Once.t; (* set when the handler's reads reach the end *)
}

let make ?max_body ?expect_continue ~limits r (framing : H1_parse.framing) =
  let src =
    match framing with
    | No_body -> None
    | Fixed n ->
        (match max_body with
        | Some max when n > max -> fail H1_error.Content_too_large
        | _ -> Some (fixed_source r n))
    | Chunked -> Some (chunked_source ?max_body ~limits r)
  in
  let continue =
    match expect_continue with None -> No_expectation | Some f -> Expected f
  in
  let t = { body = Body.empty; src; continue; completed = Affect.Cell.Once.make () } in
  match src with
  | None -> Affect.Cell.Once.try_set t.completed (); t
  | Some src ->
      (* The handler's reads send 100 Continue first, if it is expected. A
         read that times out is the client's fault: 408. *)
      let read buf off len =
        (match t.continue with
        | Expected f -> t.continue <- Sent; f ()
        | No_expectation | Sent -> ());
        let n = try src.read buf off len with Flow.Timeout -> fail H1_error.Request_timeout in
        if src.finished () then Affect.Cell.Once.try_set t.completed ();
        n
      in
      t.body <- to_body { src with read };
      t

let body t = t.body
let completed t = Affect.Cell.Once.get' t.completed
let is_complete t = match t.src with None -> true | Some src -> src.finished ()
let unread t = match t.src with None -> Some 0 | Some src -> src.unread ()
let failed t = match t.src with None -> false | Some src -> src.failed ()

let drain t ~max =
  match t.src, t.continue with
  | None, _ -> true
  | Some src, _ when src.failed () -> false
  | Some src, _ when src.finished () -> true
  | Some _, Expected _ -> false (* the client may be holding the body back *)
  | Some src, _ when Option.fold ~none:false ~some:(fun n -> n > max) (src.unread ()) -> false
  | Some src, (No_expectation | Sent) ->
      let buf = Bytes.create 4096 in
      let rec discard left =
        match src.read buf 0 (if left < Bytes.length buf then left + 1 else Bytes.length buf) with
        | 0 -> true
        | n when n > left -> false
        | n -> discard (left - n)
      in
      (try discard max with Body.Truncated | H1_error.E _ -> false)
