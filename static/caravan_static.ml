open Caravan
module File = Weft.File
module Time = Weft.Time
module Date = Caravan.Private.Date

(* {1 Media types} *)

let content_type name =
  match String.lowercase_ascii (Filename.extension name) with
  | ".html" | ".htm" ->
      "text/html; charset=utf-8"
  | ".css" ->
      "text/css; charset=utf-8"
  | ".js" | ".mjs" ->
      "text/javascript; charset=utf-8"
  | ".json" | ".map" ->
      "application/json; charset=utf-8"
  | ".txt" ->
      "text/plain; charset=utf-8"
  | ".csv" ->
      "text/csv; charset=utf-8"
  | ".xml" ->
      "application/xml; charset=utf-8"
  | ".svg" ->
      "image/svg+xml; charset=utf-8"
  | ".png" ->
      "image/png"
  | ".jpg" | ".jpeg" ->
      "image/jpeg"
  | ".gif" ->
      "image/gif"
  | ".webp" ->
      "image/webp"
  | ".avif" ->
      "image/avif"
  | ".ico" ->
      "image/x-icon"
  | ".woff" ->
      "font/woff"
  | ".woff2" ->
      "font/woff2"
  | ".ttf" ->
      "font/ttf"
  | ".otf" ->
      "font/otf"
  | ".wasm" ->
      "application/wasm"
  | ".pdf" ->
      "application/pdf"
  | ".zip" ->
      "application/zip"
  | ".gz" ->
      "application/gzip"
  | ".mp4" ->
      "video/mp4"
  | ".webm" ->
      "video/webm"
  | ".mp3" ->
      "audio/mpeg"
  | ".ogg" ->
      "audio/ogg"
  | ".wav" ->
      "audio/wav"
  | _ ->
      "application/octet-stream"

(* {1 The path guard} *)

(* A byte that no segment can contain: a separator of a path on an OS, a drive
   or a stream of Windows (':'), and the controls. *)
let is_bad_byte = function
  | '/' | '\\' | ':' | '\x00' .. '\x1f' | '\x7f' ->
      true
  | _ ->
      false

(* [short_name s] is [true] if [s] has a '~' and then a digit: Windows opens
   ".git" from "GIT~1". *)
let short_name s =
  let n = String.length s in
  let rec go i =
    i < n - 1
    && ((s.[i] = '~' && s.[i + 1] >= '0' && s.[i + 1] <= '9') || go (i + 1))
  in
  go 0

(* [ignorable s] is [true] if [s] has a code point that HFS+ ignores in a
   compare of names, thus "\u{200C}.git" opens ".git" (the list of Git,
   CVE-2014-9390): U+200C to U+200F, U+202A to U+202E, U+206A to U+206F and
   U+FEFF. [s] is valid UTF-8, thus a lead byte has its continuation bytes. *)
let ignorable s =
  let n = String.length s in
  let rec go i =
    i < n - 2
    && ( ( match (s.[i], s.[i + 1], s.[i + 2]) with
           | '\xe2', '\x80', ('\x8c' .. '\x8f' | '\xaa' .. '\xae')
           | '\xe2', '\x81', '\xaa' .. '\xaf'
           | '\xef', '\xbb', '\xbf' ->
               true
           | _ ->
               false )
       || go (i + 1) )
  in
  go 0

(* [reserved s] is [true] if the base name of [s] (the text before the first
   dot, with no spaces around it) is a device of Windows, with no case. *)
let reserved s =
  let base =
    match String.index_opt s '.' with Some i -> String.sub s 0 i | None -> s
  in
  match String.uppercase_ascii (String.trim base) with
  | "CON" | "PRN" | "AUX" | "NUL" | "CONIN$" | "CONOUT$" ->
      true
  | base -> (
      ( String.starts_with ~prefix:"COM" base
      || String.starts_with ~prefix:"LPT" base )
      &&
      (* A digit, or a superscript 1, 2 or 3 in UTF-8. *)
      match String.sub base 3 (String.length base - 3) with
      | "\xc2\xb9" | "\xc2\xb2" | "\xc2\xb3" ->
          true
      | d ->
          String.length d = 1 && d.[0] >= '0' && d.[0] <= '9' )

(* [segment_ok ~dotfiles s] is [true] if [s] can be a name in a path. ["."] and
   [".."] end with a dot, thus the rule of the end refuses them. *)
let segment_ok ~dotfiles s =
  let n = String.length s in
  n > 0 && n <= 255 && String.is_valid_utf_8 s
  && (not (String.exists is_bad_byte s))
  && (not (short_name s))
  && (not (ignorable s))
  && s.[n - 1] <> '.'
  && s.[n - 1] <> ' '
  && (not (reserved s))
  && (dotfiles || s.[0] <> '.')

(* [valid ~dotfiles segments]: each segment is correct. Only the last can be
   empty: a path that ends with '/'. *)
let rec valid ~dotfiles = function
  | [] | [""] ->
      true
  | s :: rest ->
      segment_ok ~dotfiles s && valid ~dotfiles rest

(* {1 Files} *)

(* The errors that tell that no file is there for the client. EACCES is also
   here: a 403 tells that a file exists. *)
let is_missing = function
  | File.Error
      ( ( No_entry
        | Not_dir
        | Other ("EACCES" | "ENAMETOOLONG" | "ELOOP" | "EINVAL") )
      , _ ) ->
      true
  | _ ->
      false

type opened = Found of File.t * File.stat | Dir | Missing

(* [close_on_raise fd f] is [f ()]. If [f] raises, also for a cancel, [fd]
   closes and the exception goes on. [File.close] never waits. *)
let close_on_raise fd f =
  match f () with
  | v ->
      v
  | exception e ->
      let bt = Printexc.get_raw_backtrace () in
      File.close fd ;
      Printexc.raise_with_backtrace e bt

(* [open_path path] opens [path] and examines its kind on the fd: no check comes
   before the use. *)
let open_path path =
  match
    File.openfile
      ([Read] [@mutate off "[] also opens read-only: O_RDONLY is 0"])
      path
  with
  | exception e when is_missing e ->
      Missing
  | fd -> (
      let st = close_on_raise fd (fun () -> File.fstat fd) in
      match st.kind with
      | Regular ->
          Found (fd, st)
      | Directory ->
          File.close fd ; Dir
      | Symlink | Special ->
          File.close fd ; Missing )

(* {1 Dates} *)

(* [ns p] is the POSIX time [p] in nanoseconds. *)
let ns p = Weft.Span.to_ns (Time.diff Posix p Date.epoch)

(* [seconds p] is the POSIX time [p] in whole seconds, rounded down. *)
let seconds p =
  let ns = ns p in
  let s = Int64.div ns 1_000_000_000L in
  Int64.to_int (if Int64.rem ns 1_000_000_000L < 0L then Int64.pred s else s)

(* [date ~year s] is the HTTP date [s] in seconds, or [None] if it is not
   correct. [year] is the year of the time of the response. An RFC 850 date has
   two digits: its year is the year with these digits nearest to [year], and a
   year more than 50 years after [year] is in the past century (RFC 9110
   §5.6.7). *)
let date ~year s =
  let utc (y, month, day) (hour, minute, second) =
    Time.of_utc
      { year= y
      ; month
      ; day
      ; hour
      ; minute
      ; second
      ; weekday= (0 [@mutate off "Time.of_utc ignores the weekday"]) }
    |> Option.map seconds
  in
  match Http_date.decode s with
  | exception (Invalid_argument _ | Failure _) ->
      None
  | `IMF (_, d, t) | `ASCTIME (_, d, t) ->
      utc d t
  | `RFC850 (_, (yy, m, d), t) ->
      let y = year - (year mod 100) + yy in
      let y =
        if y > year + 50 then y - 100 else if y <= year - 50 then y + 100 else y
      in
      utc (y, m, d) t

(* [one_date ~year name headers] is the date of the field [name], if it has one
   line with a correct date. A recipient ignores a field with more than one
   member (RFC 9110 §13.1.3). *)
let one_date ~year name headers =
  match Headers.find_all name headers with [v] -> date ~year v | _ -> None

(* {1 Entity tags} *)

type tags = Any | Tags of (bool * string) list

let is_ws = Caravan.Private.Token.is_ws

(* [tags s] is the list of entity tags of [s] (RFC 9110 §8.8.3): ["*"], or a
   list of [W/"..."] and ["..."], as weak and the opaque tag with its quotes.
   Empty elements are permitted. A syntax error gives [Tags []]: it matches
   nothing. *)
let tags s =
  let s = String.trim s in
  if s = "*" then Any
  else
    let n = String.length s in
    let rec skip i =
      if i < n && (is_ws s.[i] || s.[i] = ',') then skip (i + 1) else i
    in
    let rec ws i = if i < n && is_ws s.[i] then ws (i + 1) else i in
    (* The index of the quote at the end of an opaque tag. *)
    let rec quote_end i =
      if i >= n then None
      else
        match s.[i] with
        | '"' ->
            Some i
        | '\x21' | '\x23' .. '\x7e' | '\x80' .. '\xff' ->
            quote_end (i + 1)
        | _ ->
            None
    in
    let rec go acc i =
      let i = skip i in
      if i = n then Tags (List.rev acc)
      else
        let weak = i + 1 < n && s.[i] = 'W' && s.[i + 1] = '/' in
        let q = if weak then i + 2 else i in
        if q >= n || s.[q] <> '"' then Tags []
        else
          match quote_end (q + 1) with
          | None ->
              Tags []
          | Some e ->
              let j = ws (e + 1) in
              if j < n && s.[j] <> ',' then Tags []
              else go ((weak, String.sub s q (e - q + 1)) :: acc) j
    in
    go [] 0

(* [field_tags name headers] is the list of the field lines of [name], if there
   is one. *)
let field_tags name headers =
  match Headers.find_all name headers with
  | [] ->
      None
  | lines ->
      Some (tags (String.concat ", " lines))

(* {1 Responses} *)

let method_not_allowed () =
  Response.string ~status:Status.method_not_allowed
    ~headers:(Headers.of_list [("Allow", "GET, HEAD")])
    "Method Not Allowed\n"

let is_get_or_head req =
  match Request.meth req with Method.GET | Method.HEAD -> true | _ -> false

(* [body fd ~first ~length] reads [length] bytes of [fd] from the byte [first].
   The hook closes [fd] at the end of the body: the server closes each response
   body, thus the hook runs also for HEAD and an error. *)
let body fd ~first ~length =
  let pos = ref 0 in
  Body.of_source ~length (fun buf off len ->
      let remaining = length - !pos in
      if remaining = 0 then 0
      else
        let k =
          File.pread fd ~file_offset:(first + !pos) buf off (min len remaining)
        in
        pos := !pos + k ;
        k )
  |> Body.on_end (fun _ ~bytes:_ -> File.close fd)

(* {1 Ranges} *)

(* A range of a Range field (RFC 9110 §14.1.1): [From (first, last)] is
   [first-last], or [first-] with the last byte [cap]. [Suffix n] is [-n]. *)
type spec = From of int * int | Suffix of int

(* A number stays at [cap] after it reaches it, and [cap] is more than the size
   of each file (RFC 9110 §14.1.2: large numerals). [10 * cap + 9] is less than
   [max_int], thus the step to [cap] has no overflow. *)
let cap = 1 lsl 58

(* The most elements of a Range field. A server can ignore a field with many
   ranges (RFC 9110 §14.2), thus the parse of a long field costs nothing. *)
let max_ranges = 100

(* [after s i] is the text of [s] after the index [i]. *)
let after s i = String.sub s (i + 1) (String.length s - i - 1)

(* [number s] is the value of the decimal digits [s], or [None] if [s] is empty
   or has another character. *)
let number s =
  if s = "" || not (String.for_all (fun c -> c >= '0' && c <= '9') s) then None
  else
    Some
      (String.fold_left
         (fun n c -> min cap ((n * 10) + Char.code c - Char.code '0'))
         0 s )

(* [digits_le x y] is [true] if the decimal number [x] is at most [y]. It
   compares the digits, thus two numbers above [cap] keep their order. *)
let digits_le x y =
  let strip s =
    let n = String.length s in
    let rec go i = if i < n && s.[i] = '0' then go (i + 1) else i in
    let i = go 0 in
    String.sub s i (n - i)
  in
  let x = strip x and y = strip y in
  String.length x < String.length y
  || (String.length x = String.length y && x <= y)

(* [spec s] is the range of the element [s], with no OWS. *)
let spec s =
  match String.index_opt s '-' with
  | Some 0 ->
      Option.map (fun n -> Suffix n) (number (after s 0))
  | Some i -> (
      let f = String.sub s 0 i in
      let first = number f in
      match after s i with
      | "" ->
          Option.map (fun a -> From (a, cap)) first
      | l -> (
        match (first, number l) with
        | Some a, Some b when digits_le f l ->
            Some (From (a, b))
        | _ ->
            None ) )
  | None ->
      None

(* [ranges v] is the list of the ranges of the Range value [v], or [None] if it
   is not a correct [bytes] range, or if it has more than [max_ranges] elements.
   One incorrect element makes the whole value incorrect. Empty elements and OWS
   around each element are permitted. *)
let ranges v =
  match String.index_opt v '=' with
  | Some 5
    when String.lowercase_ascii (String.sub v 0 5) = "bytes"
         && String.fold_left (fun n c -> if c = ',' then n + 1 else n) 0 v
            < max_ranges ->
      let elements =
        String.split_on_char ',' (after v 5)
        |> List.map String.trim
        |> List.filter (( <> ) "")
      in
      let specs = List.filter_map spec elements in
      if specs = [] || List.compare_lengths specs elements <> 0 then None
      else Some specs
  | _ ->
      None

(* [satisfy size r] is the first and the last byte of [r] in a file of [size]
   bytes, if [r] is satisfiable. [size] is more than 0. *)
let satisfy size = function
  | From (a, b) ->
      if a < size then Some (a, min b (size - 1)) else None
  | Suffix n ->
      if n > 0 then Some (max 0 (size - n), size - 1) else None

(* [range ~year ~now_s ~etag ~modified_s size req] is the range of the response,
   after the preconditions. RFC 9110 §14.2: Range only for GET, and an empty
   file ignores it. More than one satisfiable range gives the full file, as a
   server may.

   If-Range has one line with the ETag, or one date. The date matches if it is
   the second of Last-Modified, and this second is before the second of the
   response: a heuristic for a strong date (RFC 9110 §8.8.2.2). A tag never
   decodes as a date. *)
let range ~year ~now_s ~etag ~modified_s size req =
  let headers = Request.headers req in
  let if_range () =
    match Headers.find_all "if-range" headers with
    | [] ->
        true
    | [v] -> (
        v = etag
        ||
        match date ~year v with
        | Some d ->
            d = modified_s && modified_s < now_s
        | None ->
            false )
    | _ ->
        false
  in
  match Headers.find_all "range" headers with
  | [v] when Method.equal (Request.meth req) Method.GET && size > 0 -> (
    match ranges v with
    | Some specs when if_range () -> (
      match List.filter_map (satisfy size) specs with
      | [] ->
          `Unsatisfiable
      | [r] ->
          `Part r
      | _ ->
          `Full )
    | _ ->
        `Full )
  | _ ->
      `Full

(* [serve ~fields fd st req] is the response for the regular file [fd], after
   the preconditions of RFC 9110 §13.2.2, then the range. [fields ()] gives the
   [Content-Type] and the [Cache-Control]: it runs after the open, thus in the
   protect of [fd]. *)
let serve ~fields fd (st : File.stat) req =
  close_on_raise fd (fun () ->
      let content_type, cache_control = fields () in
      let now = Time.now Posix in
      let year = (Time.to_utc now).year in
      let now_s = seconds now in
      (* A Last-Modified after the time of the response is the time of the
         response (RFC 9110 §8.8.2.1). *)
      let modified =
        if
          ( seconds st.mtime > now_s )
          [@mutate off "two times in one second give one date and one second"]
        then now
        else st.mtime
      in
      let modified_s = seconds modified in
      let etag = Printf.sprintf "\"%x-%Lx\"" st.size (ns st.mtime) in
      let headers = Request.headers req in
      let cache =
        match cache_control with Some v -> [("Cache-Control", v)] | None -> []
      in
      let respond status fields =
        File.close fd ;
        Response.empty ~status ~headers:(Headers.of_list fields) ()
      in
      let failed =
        match field_tags "if-match" headers with
        | Some Any ->
            false
        | Some (Tags l) ->
            not (List.exists (fun (weak, t) -> (not weak) && t = etag) l)
        | None -> (
          match one_date ~year "if-unmodified-since" headers with
          | Some d ->
              modified_s > d
          | None ->
              false )
      in
      let not_modified =
        match field_tags "if-none-match" headers with
        | Some Any ->
            true
        | Some (Tags l) ->
            List.exists (fun (_, t) -> t = etag) l
        | None -> (
          match one_date ~year "if-modified-since" headers with
          | Some d ->
              d <= now_s && modified_s <= d
          | None ->
              false )
      in
      if failed then respond Status.precondition_failed []
      else if not_modified then
        respond Status.not_modified (("ETag", etag) :: cache)
      else
        let entity =
          [ ("Content-Type", content_type)
          ; ("ETag", etag)
          ; ("Last-Modified", Date.to_imf modified)
          ; ("X-Content-Type-Options", "nosniff")
          ; ("Accept-Ranges", "bytes") ]
          @ cache
        in
        let ok status extra first length =
          Response.make ~status
            ~headers:(Headers.of_list (entity @ extra))
            (body fd ~first ~length)
        in
        match range ~year ~now_s ~etag ~modified_s st.size req with
        | `Unsatisfiable ->
            respond Status.range_not_satisfiable
              [("Content-Range", Printf.sprintf "bytes */%d" st.size)]
        | `Part (first, last) ->
            ok Status.partial_content
              [ ( "Content-Range"
                , Printf.sprintf "bytes %d-%d/%d" first last st.size ) ]
              first
              (last - first + 1)
        | `Full ->
            ok Status.ok [] 0 st.size )

let file ?content_type:ct ?cache_control path req =
  if not (is_get_or_head req) then method_not_allowed ()
  else
    match open_path path with
    | Found (fd, st) ->
        let fields () =
          ( (match ct with Some t -> t | None -> content_type path)
          , cache_control )
        in
        serve ~fields fd st req
    | Dir | Missing ->
        Handler.not_found req

(* [redirect req] is a 301 to the directory of the path of [req], with a
   relative [Location]: the raw last segment, a '/', and the query. *)
let redirect req =
  let path = Request.path_only req in
  let last =
    match String.rindex_opt path '/' with
    | Some i ->
        String.sub path (i + 1) (String.length path - i - 1)
    | None ->
        path
  in
  let query = match Request.query req with Some q -> "?" ^ q | None -> "" in
  Response.empty ~status:Status.moved_permanently
    ~headers:(Headers.of_list [("Location", last ^ "/" ^ query)])
    ()

(* [lookup] is the response for [segments] under [root], or [None] if there is
   no file. *)
let lookup ~index ~dotfiles ~content_type ~cache_control ~root segments req =
  if not (valid ~dotfiles segments) then None
  else
    let rel = String.concat "/" segments in
    (* A path that ends with '/' has an empty last segment. With no segment, the
       path of the request tells it. *)
    let slash =
      match List.rev segments with
      | "" :: _ ->
          true
      | [] ->
          String.ends_with ~suffix:"/" (Request.path_only req)
      | _ ->
          false
    in
    let serve_rel rel fd st =
      serve ~fields:(fun () -> (content_type rel, cache_control rel)) fd st req
    in
    if slash then
      (* The index first: one open, not an open of the directory too. "a.txt/"
         gives ENOTDIR. *)
      match index with
      | None ->
          None
      | Some ix -> (
          let irel = rel ^ ix in
          match open_path (root ^ "/" ^ irel) with
          | Found (fd, st) ->
              Some (serve_rel irel fd st)
          | Dir | Missing ->
              None )
    else
      match open_path (root ^ "/" ^ rel) with
      | Missing ->
          None
      | Found (fd, st) ->
          Some (serve_rel rel fd st)
      | Dir -> (
        match index with
        | None ->
            None
        | Some ix -> (
            (* No segment is empty here: the path does not end with '/'. *)
            let irel = String.concat "/" (segments @ [ix]) in
            match open_path (root ^ "/" ^ irel) with
            | Found (fd, _) ->
                File.close fd ;
                Some (redirect req)
            | Dir | Missing ->
                None ) )

let default_index = Some "index.html"

let no_cache_control _ = None

let check_root fn root =
  if root = "" then
    invalid_arg (Printf.sprintf "Caravan_static.%s: an empty root" fn)

let dir ?(index = default_index) ?(dotfiles = false)
    ?(content_type = content_type) ?(cache_control = no_cache_control) ~root
    segments req =
  check_root "dir" root ;
  if not (is_get_or_head req) then method_not_allowed ()
  else
    match
      lookup ~index ~dotfiles ~content_type ~cache_control ~root segments req
    with
    | Some r ->
        r
    | None ->
        Handler.not_found req

(* [segments_of prefix path] are the decoded segments of [path] after [prefix],
   or [None] if [path] is not under [prefix] or has a bad [%] sequence. [prefix]
   is [""] for "/". *)
let segments_of prefix path =
  let lp = String.length prefix in
  if not (String.starts_with ~prefix path) then None
  else if String.length path > lp && path.[lp] = '/' then
    let rest = String.sub path (lp + 1) (String.length path - lp - 1) in
    List.fold_right
      (fun s acc ->
        match (Caravan.Private.Percent.decode s, acc) with
        | Some s, Some l ->
            Some (s :: l)
        | _ ->
            None )
      (String.split_on_char '/' rest)
      (Some [])
  else if path = prefix && prefix <> "" then Some []
  else None

let middleware ?(prefix = "/") ?(index = default_index) ?(dotfiles = false)
    ?(content_type = content_type) ?(cache_control = no_cache_control) ~root ()
    =
  check_root "middleware" root ;
  if
    (not (String.starts_with ~prefix:"/" prefix))
    || (prefix <> "/" && String.ends_with ~suffix:"/" prefix)
  then
    invalid_arg
      (Printf.sprintf
         "Caravan_static.middleware: the prefix %S does not start with / or \
          ends with /"
         prefix ) ;
  let prefix = if prefix = "/" then "" else prefix in
  fun next req ->
    if not (is_get_or_head req) then next req
    else
      match segments_of prefix (Request.path_only req) with
      | None ->
          next req
      | Some segments -> (
        match
          lookup ~index ~dotfiles ~content_type ~cache_control ~root segments
            req
        with
        | Some r ->
            r
        | None ->
            next req )
