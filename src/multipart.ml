let media_type = "multipart/form-data"

type part =
  { name: string
  ; filename: string option
  ; content_type: string option
  ; headers: Headers.t
  ; body: Body.t
  ; head_bytes: int (* the padding and the lines of the head, for read_all *) }

let name p = p.name

let filename p = p.filename

let content_type p = p.content_type

let headers p = p.headers

let body p = p.body

type error =
  | Unsupported_media_type
  | Malformed of string
  | Too_many_parts
  | Header_too_large
  | Too_large

let error_status = function
  | Unsupported_media_type ->
      Status.unsupported_media_type
  | Malformed _ ->
      Status.bad_request
  | Too_many_parts | Header_too_large | Too_large ->
      Status.content_too_large

let pp_error ppf = function
  | Unsupported_media_type ->
      Format.pp_print_string ppf "Unsupported_media_type"
  | Malformed m ->
      Format.fprintf ppf "Malformed (%s)" m
  | Too_many_parts ->
      Format.pp_print_string ppf "Too_many_parts"
  | Header_too_large ->
      Format.pp_print_string ppf "Header_too_large"
  | Too_large ->
      Format.pp_print_string ppf "Too_large"

(* {1 Parameters} *)

let is_ws = Token.is_ws

let rec skip_ws s i =
  if i < String.length s && is_ws s.[i] then skip_ws s (i + 1) else i

let rec token_end s i =
  if i < String.length s && Token.is_tchar s.[i] then token_end s (i + 1) else i

(* [params ~escapes s i] is the [;]-separated parameters of [s] from [i]: the
   names in lower case, in order. A quoted value has the [\] escapes of RFC 9110
   iff [escapes]. [None] if the syntax is not correct. *)
let params ~escapes s i =
  let n = String.length s in
  let quoted i =
    let b = Buffer.create 16 in
    let rec go i =
      if i >= n then None
      else
        match s.[i] with
        | '"' ->
            Some (Buffer.contents b, i + 1)
        | '\\' when escapes && i + 1 < n ->
            Buffer.add_char b s.[i + 1] ;
            go (i + 2)
        | c ->
            Buffer.add_char b c ;
            go (i + 1)
    in
    go i
  in
  let rec go i acc =
    let i = skip_ws s i in
    if i = n then Some (List.rev acc)
    else if s.[i] <> ';' then None
    else
      let i = skip_ws s (i + 1) in
      let e = token_end s i in
      if e = i then None
      else
        let key = String.lowercase_ascii (String.sub s i (e - i)) in
        let j = skip_ws s e in
        if j = n || s.[j] <> '=' then None
        else
          let j = skip_ws s (j + 1) in
          if j < n && s.[j] = '"' then
            match quoted (j + 1) with
            | None ->
                None
            | Some (v, k) ->
                go k ((key, v) :: acc)
          else
            let k = token_end s j in
            if k = j then None else go k ((key, String.sub s j (k - j)) :: acc)
  in
  go i []

(* The value of [key] in [ps], if it is there one time. *)
let single key ps =
  match List.filter (fun (k, _) -> String.equal k key) ps with
  | [] ->
      `Missing
  | [(_, v)] ->
      `One v
  | _ ->
      `Many

let is_bchar = function
  | '0' .. '9'
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '\''
  | '('
  | ')'
  | '+'
  | '_'
  | ','
  | '-'
  | '.'
  | '/'
  | ':'
  | '='
  | '?'
  | ' ' ->
      true
  | _ ->
      false

let is_boundary b =
  let n = String.length b in
  n >= 1 && n <= 70 && String.for_all is_bchar b && b.[n - 1] <> ' '

let boundary v =
  let i =
    match String.index_opt v ';' with Some i -> i | None -> String.length v
  in
  match params ~escapes:true v i with
  | None ->
      None
  | Some ps -> (
    match single "boundary" ps with
    | `One b when is_boundary b ->
        Some b
    | `One _ | `Missing | `Many ->
        None )

(* {1 The parse} *)

(* Ends the parse with an error. Only the code of [fold] raises it, never the
   read of a part body. *)
exception Stop of error

let malformed m = raise (Stop (Malformed m))

type next = [`Part of int | `Close | `Error of error]

type state =
  { src: Body.t
  ; buf: Bytes.t
  ; delim: string (* CRLF--boundary *)
  ; max_head: int
  ; mutable pos: int (* the first byte that is not consumed *)
  ; mutable lim: int (* the end of the bytes in [buf] *)
  ; mutable scan: int (* no delimiter starts before it *)
  ; mutable failed: (exn * Printexc.raw_backtrace) option (* of [src] *)
  ; mutable broken: string option (* the syntax error of a part read *)
  ; mutable next: next }

(* [fill st] reads more bytes after those of [st], and gives [false] at the end
   of the body. The callers make sure that [st] has less than the length of the
   delimiter, thus the buffer has space. A failure of the body is kept, and each
   later call raises it again. *)
let fill st =
  match st.failed with
  | Some (e, bt) ->
      Printexc.raise_with_backtrace e bt
  | None -> (
      Bytes.blit st.buf st.pos st.buf 0 (st.lim - st.pos) ;
      st.lim <- st.lim - st.pos ;
      st.scan <- max 0 (st.scan - st.pos) ;
      st.pos <- 0 ;
      match Body.read st.src st.buf st.lim (Bytes.length st.buf - st.lim) with
      (* [Body] gives 0 again after the end, with no read of the source. *)
      | 0 ->
          false
      | n ->
          st.lim <- st.lim + n ;
          true
      | exception e ->
          let bt = Printexc.get_raw_backtrace () in
          st.failed <- Some (e, bt) ;
          Printexc.raise_with_backtrace e bt )

(* [ensure st n] is [true] iff [st] has [n] bytes, after reads if necessary. *)
let rec ensure st n = st.lim - st.pos >= n || (fill st && ensure st n)

(* [same buf delim i k] is [true] iff the bytes of [delim] from [k] are at [i +
   k] in [buf]. A function at the top, thus no closure for each CR of the
   content. *)
let rec same buf delim i k =
  k = String.length delim
  || (Bytes.get buf (i + k) = delim.[k] && same buf delim i (k + 1))

let is_delim st i = same st.buf st.delim i 0

(* The index of the first delimiter in the bytes of [st], or [-1]. Each search
   starts at [scan], thus each byte is examined once. *)
let find_delim st =
  let last = st.lim - String.length st.delim in
  let rec go i =
    if i > last then begin
      st.scan <- i ;
      -1
    end
    else if Bytes.get st.buf i = '\r' && is_delim st i then begin
      st.scan <- i ;
      i
    end
    else go (i + 1)
  in
  go (max st.scan st.pos)

(* The bytes of [st] that cannot be the start of a delimiter, when no delimiter
   is in [st]. *)
let safe st =
  max 0
    ( ( st.lim - String.length st.delim + 1 )
      [@mutate off "a smaller count gives the same bytes in a later read"]
    - st.pos )

(* After a delimiter: [--] for the close delimiter, or spaces and tabs then
   CRLF. The spaces and tabs count in the head of the next part. *)
let after_delim st =
  if
    ensure st 2
    && Bytes.get st.buf st.pos = '-'
    && Bytes.get st.buf (st.pos + 1) = '-'
  then begin
    st.pos <- st.pos + 2 ;
    `Close
  end
  else
    let rec pad n =
      if
        ( n > st.max_head )
        [@mutate off "a bound of the loop: read_head gives the same error"]
      then raise (Stop Header_too_large)
      else if not (ensure st 1) then malformed "no close delimiter"
      else
        match Bytes.get st.buf st.pos with
        | ' ' | '\t' ->
            st.pos <- st.pos + 1 ;
            pad (n + 1)
        | '\r' when ensure st 2 && Bytes.get st.buf (st.pos + 1) = '\n' ->
            st.pos <- st.pos + 2 ;
            `Part n
        | _ ->
            malformed "incorrect bytes after a boundary"
    in
    pad 0

(* [consume_delim st d] consumes the delimiter at [d] and the bytes after it. *)
let consume_delim st d =
  st.pos <- d + String.length st.delim ;
  st.next <- after_delim st

(* [content st b off len] reads the content of the part into [b], [0] at its
   delimiter. A syntax error raises [Body.Truncated], thus a copy of the part
   does not end as if it were complete. *)
let rec content st b off len =
  if Option.is_some st.broken then raise Body.Truncated ;
  let copy n =
    Bytes.blit st.buf st.pos b off n ;
    st.pos <- st.pos + n ;
    n
  in
  let d = find_delim st in
  if d >= 0 then
    if d > st.pos then copy (min len (d - st.pos))
    else begin
      ( match consume_delim st d with
      | () ->
          ()
      | exception Stop (Malformed m) ->
          st.broken <- Some m ;
          raise Body.Truncated
      | exception Stop e ->
          st.next <- `Error e ) ;
      0
    end
  else if safe st > 0 then copy (min len (safe st))
  else if fill st then content st b off len
  else begin
    st.broken <- Some "no close delimiter" ;
    raise Body.Truncated
  end

(* [skip_preamble st] discards the bytes before the first delimiter, at most
   [max_head], and the delimiter. The two bytes of the CRLF that the parse puts
   before the body do not count. *)
let skip_preamble st =
  let rec go budget =
    let d = find_delim st in
    if d >= 0 then
      if
        ( d - st.pos > budget )
        [@mutate off "pos is 0 here: fill moves the bytes to the start"]
      then malformed "the preamble is too long"
      else consume_delim st d
    else
      let n = safe st in
      if n > budget then malformed "the preamble is too long"
      else begin
        st.pos <- st.pos + n ;
        if fill st then go (budget - n) else malformed "no delimiter"
      end
  in
  go (st.max_head + 2)

(* The epilogue: read and ignored, at most [max_head] bytes. Then the server
   discards the rest. *)
let skip_epilogue st =
  let rec go budget =
    let n = st.lim - st.pos in
    if n <= budget then begin
      st.pos <- st.lim ;
      if fill st then go (budget - n)
    end
  in
  go st.max_head

(* [read_head st used] is the head of a part, with no final empty line, and its
   size in bytes. [used] bytes of padding came before it. Each scan for a LF
   starts after the last one and stops at the limit, thus the parse is
   linear. *)
let read_head st used =
  let b = Buffer.create 256 in
  let rec go used line_start =
    (* The limit first: a head at the limit with no end is too large, also at
       the end of the body. *)
    let room = st.max_head - used in
    if room <= 0 then raise (Stop Header_too_large)
    else if st.pos = st.lim && not (fill st) then
      malformed "the body ends in the head of a part"
    else
      let stop = min st.lim (st.pos + room) in
      let rec lf k =
        if k = stop || Bytes.get st.buf k = '\n' then k else lf (k + 1)
      in
      let k = lf st.pos in
      let taken = (if k = stop then k else k + 1) - st.pos in
      Buffer.add_subbytes b st.buf st.pos taken ;
      st.pos <- st.pos + taken ;
      let used = used + taken in
      let n = Buffer.length b in
      if k = stop then go used line_start
      else if n >= 2 && Buffer.nth b (n - 2) = '\r' then
        if n - 2 = line_start then (Buffer.sub b 0 line_start, used)
        else go used n
      else go used line_start
  in
  go used 0

(* The fields of a head: [Name: value] lines that end with CRLF. The text is
   empty or ends with LF. *)
let head_fields text =
  let len = String.length text in
  let rec go i acc =
    if i = len then Headers.of_list (List.rev acc)
    else
      let j = String.index_from text i '\n' in
      (* [read_head] ends a line only at CRLF: thus a LF with no CR before it is
         a bare LF. The checks of [Headers] refuse a CR in a name or a value. *)
      if j = i || text.[j - 1] <> '\r' then
        malformed "a bare LF in the head of a part"
      else
        (* The line is [text.[i..e-1]], and [text.[e]] is its CR. *)
        let e = j - 1 in
        let rec colon k =
          if k = e || text.[k] = ':' then k else colon (k + 1)
        in
        match colon i with
        | c when c < e ->
            let vi = skip_ws text (c + 1) in
            let rec right k =
              if k > vi && is_ws text.[k - 1] then right (k - 1) else k
            in
            let vj = right e in
            let name = String.sub text i (c - i) in
            let value = String.sub text vi (vj - vi) in
            if Headers.is_valid_name name && Headers.is_valid_value value then
              go (j + 1) ((name, value) :: acc)
            else malformed "an incorrect field in the head of a part"
        | _ ->
            malformed "a line with no colon in the head of a part"
  in
  go 0 []

(* RFC 7578 §4.2. In a quoted [name] or [filename], [\] is a literal byte: the
   HTML Standard encodes only a double quote, CR and LF. *)
let disposition headers =
  match Headers.find_all "content-disposition" headers with
  | [v] -> (
      (* [Headers] gives a value with no spaces at its two ends. *)
      let e = token_end v 0 in
      if
        not
          (String.equal (String.lowercase_ascii (String.sub v 0 e)) "form-data")
      then malformed "the disposition is not form-data"
      else
        match params ~escapes:false v e with
        | None ->
            malformed "an incorrect Content-Disposition"
        | Some ps -> (
          match (single "name" ps, single "filename" ps) with
          | `One name, `Missing ->
              (name, None)
          | `One name, `One f ->
              (name, Some f)
          | _ ->
              malformed "an incorrect name or filename" ) )
  | _ ->
      malformed "not one Content-Disposition"

let check_limit fn what n =
  if n < 0 then invalid_arg (Printf.sprintf "Multipart.%s: %s < 0" fn what)

let fold ?(max_parts = 1000) ?(max_header_bytes = 16384) ?(buffer_size = 16384)
    ~boundary:bd src ~init f =
  check_limit "fold" "max_parts" max_parts ;
  check_limit "fold" "max_header_bytes" max_header_bytes ;
  if not (is_boundary bd) then invalid_arg "Multipart.fold: incorrect boundary" ;
  let delim = "\r\n--" ^ bd in
  if buffer_size < (2 * String.length delim) + 4 then
    invalid_arg "Multipart.fold: buffer_size is too small" ;
  let buf = Bytes.create buffer_size in
  (* The body can start with the boundary: a CRLF comes before it. *)
  Bytes.blit_string "\r\n" 0 buf 0 2 ;
  let st =
    { src
    ; buf
    ; delim
    ; max_head= max_header_bytes
    ; pos= 0
    ; lim= 2
    ; scan= 0
    ; failed= None
    ; broken= None
    ; next= `Close }
  in
  let scratch = Bytes.create 4096 in
  let rec parts acc count =
    match st.next with
    | `Close ->
        skip_epilogue st ; acc
    | `Error e ->
        raise (Stop e)
    | `Part _ when count = max_parts ->
        raise (Stop Too_many_parts)
    | `Part pad ->
        let text, head_bytes = read_head st pad in
        let headers = head_fields text in
        let name, filename = disposition headers in
        (* RFC 2046: the CRLF of the empty line can also start the delimiter,
           thus an empty part can have no other CRLF before its delimiter. *)
        let tail = String.length st.delim - 2 in
        let empty = ensure st tail && same st.buf st.delim (st.pos - 2) 2 in
        if empty then begin
          st.pos <- st.pos + tail ;
          st.next <- after_delim st
        end ;
        let ended = ref empty in
        let reading = Atomic.make false in
        let read b off len =
          if !ended then 0
          else begin
            Atomic.set reading true ;
            match content st b off len with
            | 0 ->
                Atomic.set reading false ;
                ended := true ;
                0
            | n ->
                Atomic.set reading false ; n
            | exception e ->
                Atomic.set reading false ; raise e
          end
        in
        let body = Body.of_source read in
        let part =
          { name
          ; filename
          ; content_type= Headers.find "content-type" headers
          ; headers
          ; body
          ; head_bytes }
        in
        let acc =
          match f acc part with
          | acc ->
              if Atomic.get reading then
                invalid_arg
                  "Multipart.fold: a read of a part body after its callback" ;
              Body.close body ;
              acc
          | exception e -> (
              let bt = Printexc.get_raw_backtrace () in
              (* A hook of the caller on the body can raise in the close: the
                 exception of [f] has priority. *)
              (try Body.close body with _ -> ()) ;
              match (e, st.broken) with
              | Body.Truncated, Some m ->
                  raise (Stop (Malformed m))
              | _ ->
                  Printexc.raise_with_backtrace e bt )
        in
        (* The discard of a part that [f] did not read to its end can meet the
           syntax error. *)
        ( try
            while not !ended do
              ignore (read scratch 0 (Bytes.length scratch))
            done
          with Body.Truncated when Option.is_some st.broken ->
            malformed (Option.get st.broken) ) ;
        parts acc (count + 1)
  in
  match skip_preamble st ; parts init 0 with
  | acc ->
      Ok acc
  | exception Stop e ->
      Error e

(* {1 The buffered parse} *)

type field =
  { name: string
  ; filename: string option
  ; content_type: string option
  ; data: string }

exception Over

let read_all ?max_parts ?max_header_bytes ?(max_bytes = 8 * 1024 * 1024)
    ~boundary src =
  check_limit "read_all" "max_bytes" max_bytes ;
  let used = ref 0 in
  (* One buffer for the reads of all parts. *)
  let chunk = Bytes.create 16384 in
  let data = Buffer.create 256 in
  let take acc (p : part) =
    used := !used + p.head_bytes ;
    if !used > max_bytes then raise Over ;
    Buffer.clear data ;
    let rec copy () =
      match Body.read p.body chunk 0 (Bytes.length chunk) with
      | 0 ->
          ()
      | n ->
          used := !used + n ;
          if !used > max_bytes then raise Over ;
          Buffer.add_subbytes data chunk 0 n ;
          copy ()
    in
    copy () ;
    { name= p.name
    ; filename= p.filename
    ; content_type= p.content_type
    ; data= Buffer.contents data }
    :: acc
  in
  match fold ?max_parts ?max_header_bytes ~boundary src ~init:[] take with
  | Ok l ->
      Ok (List.rev l)
  | Error e ->
      Error e
  | exception Over ->
      Error Too_large

let find name fields =
  List.find_opt (fun (f : field) -> String.equal f.name name) fields

(* {1 Encode} *)

let prefix = "caravan-"

(* A delimiter in the data starts with CRLF, or at the start of the data, after
   the CRLF of the head. It cannot end after the data: CR is only at the start
   of a delimiter. Thus the boundary [caravan-<s>] is good if no [marker] of the
   data has [s] after it. *)
let marker = "\r\n--" ^ prefix

(* [suffixes k data set] adds to [set] the [k] bytes after each [marker] in
   [CRLF ^ data], and gives the count of the markers. *)
let suffixes k data set =
  let s = "\r\n" ^ data in
  let n = String.length s and m = String.length marker in
  let rec is_marker j i =
    i = m || (s.[j + i] = marker.[i] && is_marker j (i + 1))
  in
  let rec go j count =
    if j + m > n then count
    else if is_marker j 0 then begin
      if j + m + k <= n then Hashtbl.replace set (String.sub s (j + m) k) () ;
      go (j + m) (count + 1)
    end
    else go (j + 1) count
  in
  go 0 0

(* [binary k v] is [v] in [k] binary digits. *)
let binary k v =
  String.init k (fun i -> if (v lsr (k - 1 - i)) land 1 = 1 then '1' else '0')

let choose_boundary fields =
  let markers k =
    let set = Hashtbl.create 16 in
    let count =
      List.fold_left (fun n (f : field) -> n + suffixes k f.data set) 0 fields
    in
    (set, count)
  in
  let _, count = markers 0 in
  (* [2^k > count], thus one of the [2^k] suffixes is free. *)
  let rec bits k = if 1 lsl k > count then k else bits (k + 1) in
  let k = bits 0 in
  let used, _ = markers k in
  let rec free v =
    if Hashtbl.mem used (binary k v) then free (v + 1) else binary k v
  in
  prefix ^ free 0

(* The HTML Standard: a double quote, CR and LF in a name or a file name. *)
let escape_name s =
  let b = Buffer.create (String.length s) in
  String.iter
    (function
      | '"' ->
          Buffer.add_string b "%22"
      | '\r' ->
          Buffer.add_string b "%0D"
      | '\n' ->
          Buffer.add_string b "%0A"
      | c ->
          Buffer.add_char b c )
    s ;
  Buffer.contents b

let disposition_of (f : field) =
  "form-data; name=\"" ^ escape_name f.name ^ "\""
  ^ Option.fold ~none:""
      ~some:(fun n -> "; filename=\"" ^ escape_name n ^ "\"")
      f.filename

let encode fields =
  let parts =
    List.map
      (fun (f : field) ->
        let d = disposition_of f in
        if not (Headers.is_valid_value d) then
          invalid_arg "Multipart.encode: incorrect name or filename" ;
        ( match f.content_type with
        | Some t when not (Headers.is_valid_value t) ->
            invalid_arg "Multipart.encode: incorrect content_type"
        | Some _ | None ->
            () ) ;
        (f, d) )
      fields
  in
  let bd = choose_boundary fields in
  let size =
    (List.fold_left
       (fun n ((f : field), d) ->
         n + String.length bd + String.length d + String.length f.data + 64
         + Option.fold ~none:0 ~some:String.length f.content_type )
       (String.length bd + 8)
       parts [@mutate off "a capacity: another size gives the same text"] )
  in
  let b = Buffer.create size in
  let add = Buffer.add_string b in
  List.iter
    (fun ((f : field), d) ->
      add "--" ;
      add bd ;
      add "\r\nContent-Disposition: " ;
      add d ;
      add "\r\n" ;
      Option.iter
        (fun t -> add "Content-Type: " ; add t ; add "\r\n")
        f.content_type ;
      add "\r\n" ;
      add f.data ;
      add "\r\n" )
    parts ;
  add "--" ;
  add bd ;
  add "--\r\n" ;
  (bd, Buffer.contents b)
