open Caravan

type coding = Gzip | Deflate

(* {1 The negotiation} *)

(* [qvalue s] is the weight of the text [s] (RFC 9110 §12.4.2), in
   thousandths. *)
let qvalue s =
  let n = String.length s in
  if n = 1 then match s with "0" -> Some 0 | "1" -> Some 1000 | _ -> None
  else if n < 2 || n > 5 || s.[1] <> '.' then None
  else
    let rest = String.sub s 2 (n - 2) in
    match s.[0] with
    | '0' when String.for_all (fun c -> c >= '0' && c <= '9') rest ->
        (* "0.5" is "0500" thousandths, with a "0" first for "0.". *)
        Some (int_of_string ("0" ^ rest ^ String.make (5 - n) '0'))
    | '1' when String.for_all (fun c -> c = '0') rest ->
        Some 1000
    | _ ->
        None

exception Bad

(* [element e] is the coding and the weight of the element [e]. [x-gzip] is
   [gzip]. *)
let element e =
  let name, weight =
    match String.index_opt e ';' with
    | None ->
        (e, 1000)
    | Some i -> (
        let param =
          String.trim (String.sub e (i + 1) (String.length e - i - 1))
        in
        if not (String.starts_with ~prefix:"q=" (String.lowercase_ascii param))
        then raise Bad ;
        match qvalue (String.sub param 2 (String.length param - 2)) with
        | Some w ->
            (String.trim (String.sub e 0 i), w)
        | None ->
            raise Bad )
  in
  if not (Private.Token.is_token name) then raise Bad ;
  match String.lowercase_ascii name with
  | "x-gzip" ->
      ("gzip", weight)
  | name ->
      (name, weight)

let negotiate values =
  match
    List.concat_map (String.split_on_char ',') values
    |> List.map String.trim
    |> List.filter (( <> ) "")
    |> List.map element
  with
  | exception Bad ->
      None
  | elements ->
      (* The first element of a coding wins; [*] is for the others. *)
      let weight name =
        match List.assoc_opt name elements with
        | Some w ->
            w
        | None ->
            Option.value ~default:0 (List.assoc_opt "*" elements)
      in
      let gzip = weight "gzip" and deflate = weight "deflate" in
      if gzip = 0 && deflate = 0 then None
      else if gzip >= deflate then Some Gzip
      else Some Deflate

(* {1 The decision} *)

let media_type ct =
  let t =
    match String.index_opt ct ';' with
    | Some i ->
        String.sub ct 0 i
    | None ->
        ct
  in
  String.lowercase_ascii (String.trim t)

let default_compressible t =
  (String.starts_with ~prefix:"text/" t && t <> "text/event-stream")
  || List.mem t
       [ "application/json"
       ; "application/javascript"
       ; "application/xml"
       ; "image/svg+xml" ]
  || String.ends_with ~suffix:"+json" t
  || String.ends_with ~suffix:"+xml" t

let skip : unit Attrs.key = Attrs.key ~name:"caravan.compress.skip" ()

let with_vary h =
  let tokens = List.map String.lowercase_ascii (Headers.tokens "vary" h) in
  if List.mem "*" tokens || List.mem "accept-encoding" tokens then h
  else Headers.add "Vary" "Accept-Encoding" h

(* The ETag of a compressed representation (RFC 9110 §8.8.1: the bytes differ):
   ["x"] becomes [W/"x"] for gzip and [W/"x-deflate"] for deflate, so that a
   cache never gives the gzip bytes to a client of deflate. *)
let weak_etags coding h =
  let weak t =
    let t =
      if String.starts_with ~prefix:"W/" t then
        String.sub t 2 (String.length t - 2)
      else t
    in
    let n = String.length t in
    if n < 2 || t.[0] <> '"' || t.[n - 1] <> '"' then None
    else
      match coding with
      | Gzip ->
          Some ("W/" ^ t)
      | Deflate ->
          Some ("W/" ^ String.sub t 0 (n - 1) ^ "-deflate\"")
  in
  List.fold_left
    (fun h t -> Headers.add "ETag" (Option.value ~default:t (weak t)) h)
    (Headers.remove "etag" h)
    (Headers.find_all "etag" h)

let digests = ["content-digest"; "repr-digest"; "content-md5"]

let without_digests h = List.fold_left (fun h d -> Headers.remove d h) h digests

(* {1 Copies}

   Copies of 8 bytes at a time between bytes and bigstrings. The checked
   primitives raise on a bad range. *)

external bs_get64 : De.bigstring -> int -> int64 = "%caml_bigstring_get64"

external bs_set64 : De.bigstring -> int -> int64 -> unit
  = "%caml_bigstring_set64"

external by_get64 : Bytes.t -> int -> int64 = "%caml_bytes_get64"

external by_set64 : Bytes.t -> int -> int64 -> unit = "%caml_bytes_set64"

(* [to_bigstring src soff dst doff n] copies [n] bytes of [src] to [dst]. *)
let to_bigstring src soff dst doff n =
  let words = n / (8 [@mutate off "the byte loop copies the rest"]) in
  for w = 0 to words - 1 do
    bs_set64 dst (doff + (8 * w)) (by_get64 src (soff + (8 * w)))
  done ;
  for k = 8 * words to n - 1 do
    Bigarray.Array1.set dst (doff + k) (Bytes.get src (soff + k))
  done

(* [of_bigstring src soff dst doff n] copies [n] bytes of [src] to [dst]. *)
let of_bigstring src soff dst doff n =
  let words = n / (8 [@mutate off "the byte loop copies the rest"]) in
  for w = 0 to words - 1 do
    by_set64 dst (doff + (8 * w)) (bs_get64 src (soff + (8 * w)))
  done ;
  for k = 8 * words to n - 1 do
    Bytes.set dst (doff + k) (Bigarray.Array1.get src (soff + k))
  done

(* [deflate_tags coding req] is [req] with each [W/"t-deflate"] of
   [If-None-Match] changed to [W/"t"] for a client of deflate: the handler
   compares the tags of its own bytes, and the middleware gives the deflate tag
   back on the 304. *)
let deflate_tags coding req =
  let h = Request.headers req in
  if not (Headers.mem "if-none-match" h) then req
  else
    match Lazy.force coding with
    | Some Deflate ->
        let suffix = "-deflate\"" in
        let plain t =
          if String.starts_with ~prefix:"W/\"" t && String.ends_with ~suffix t
          then String.sub t 0 (String.length t - String.length suffix) ^ "\""
          else t
        in
        let tags = List.map plain (Headers.tokens "if-none-match" h) in
        Request.with_headers
          (Headers.set "If-None-Match" (String.concat ", " tags) h)
          req
    | _ ->
        req

(* {1 The encoder} *)

module type ENCODER = sig
  type encoder

  val src : encoder -> De.bigstring -> int -> int -> encoder

  val dst : encoder -> De.bigstring -> int -> int -> encoder

  val dst_rem : encoder -> int

  val encode :
    encoder -> [`Await of encoder | `Flush of encoder | `End of encoder]
end

(* The output buffer, and the input block: a yield after each block of input
   keeps the domain free for the other fibers (about 0.1 ms of work). *)
let output_size = 16384

let block = 4096

let yield () = try Weft.Fiber.yield () with Weft.Fiber.Not_in_fiber _ -> ()

(* [compress (module E) e old w] writes the compressed bytes of the body [old]
   into [w]. The output goes to [w] only on [`Flush] and [`End]: the encoder
   reports its output only then. *)
let compress (type e) (module E : ENCODER with type encoder = e) (e : e) old w =
  let i = De.bigstring_create block in
  let o = De.bigstring_create output_size in
  let out = Bytes.create output_size in
  let emit e =
    let n = output_size - E.dst_rem e in
    of_bigstring o 0 out 0 n ;
    Body.Writer.write w out 0 n
  in
  (* [run e] encodes until the encoder awaits input, or ends. *)
  let rec run e =
    match E.encode e with
    | `Await e ->
        e
    | `Flush e ->
        emit e ;
        run (E.dst e o 0 output_size)
    | `End e ->
        emit e [@mutate off "decompress 1.6 gives all output by Flush first"] ;
        e
  in
  let e = ref (E.dst e o 0 output_size) in
  let declared = Body.length old in
  let count = ref 0 in
  let check n =
    match declared with
    | Some d when n <> d ->
        failwith
          (Printf.sprintf
             "Caravan_compress: the body has %d bytes, not the declared %d" n d )
    | _ ->
        ()
  in
  (* The input collects in [i]: the encoder runs, and the fiber yields, for each
     full block, not for each write. *)
  let fill = ref 0 in
  let encode_block () =
    if
      ( !fill > 0 )
      [@mutate off "decompress 1.6 ends one time: a second end adds no byte"]
    then (
      e := run (E.src !e i 0 !fill) ;
      fill := 0 ;
      yield () )
  in
  let write buf off len =
    count := !count + len ;
    (* A body larger than its declared length stops at once. *)
    ( match declared with
    | Some d when !count > d ->
        failwith
          (Printf.sprintf
             "Caravan_compress: the body has more than its declared %d bytes" d )
    | _ ->
        () ) ;
    let rec go off len =
      if len > 0 then (
        let n = min len (block - !fill) in
        to_bigstring buf off i !fill n ;
        fill := !fill + n ;
        if !fill = block then encode_block () ;
        go (off + n) (len - n) )
    in
    go off len
  in
  let inner = Body.Writer.make ~write ~flush:(fun () -> Body.Writer.flush w) in
  Body.write_to old inner ;
  (* A shorter body: no end of the stream. *)
  check !count ;
  encode_block () ;
  e := run (E.src !e i (0 [@mutate off "the offset of an empty input"]) 0) ;
  (* A digest of the identity bytes is not one of the compressed bytes. *)
  let t = without_digests (Body.Writer.trailers inner) in
  if not (Headers.is_empty t) then Body.Writer.set_trailers w t

let compressed coding level old w =
  let q = De.Queue.create 16384 in
  let window =
    De.Lz77.make_window
      ~bits:(15 [@mutate off "make_window makes one size for each bits"])
  in
  match coding with
  | Gzip ->
      compress
        (module Gz.Def)
        (Gz.Def.encoder `Manual `Manual ~mtime:0l Gz.Unix ~q ~w:window ~level)
        old w
  | Deflate ->
      compress
        (module Zl.Def)
        (Zl.Def.encoder ~q ~w:window ~level `Manual `Manual)
        old w

(* {1 The middleware} *)

let middleware ?(min_size = 2048) ?(max_size = 8 * 1024 * 1024)
    ?(max_active = 64) ?(streams = false) ?(compressible = default_compressible)
    ?(level = 4) () =
  let bad m = invalid_arg ("Caravan_compress.middleware: " ^ m) in
  if min_size < 0 then bad "a negative min_size" ;
  if max_size < min_size then bad "max_size is less than min_size" ;
  if max_active < 1 then bad "max_active is less than 1" ;
  if level < 0 || level > 9 then bad "level is not in 0 to 9" ;
  let active = Atomic.make 0 in
  (* [take ()] takes a slot of an encoder, if one is free. *)
  let rec take () =
    let n = Atomic.get active in
    n < max_active && (Atomic.compare_and_set active n (n + 1) || take ())
  in
  (* [in_limits length] is [true] if a body of [length] can be compressed. *)
  let in_limits = function
    | Some n ->
        n >= min_size && n <= max_size
    | None ->
        streams
  in
  fun next req ->
    let coding =
      lazy
        (negotiate (Headers.find_all "accept-encoding" (Request.headers req)))
    in
    let resp = next (deflate_tags coding req) in
    let h = Response.headers resp in
    let typed =
      match Headers.find "content-type" h with
      | Some ct ->
          compressible (media_type ct)
      | None ->
          false
    in
    let skipped =
      (not typed)
      || ( match Response.kind resp with
        | Response.Upgrade _ ->
            true
        | Response.Body _ ->
            false )
      || Method.equal (Request.meth req) Method.CONNECT
      || Version.equal (Request.version req) Version.Http_1_0
      || Option.is_some (Response.find_attr skip resp)
      || Headers.mem "content-encoding" h
      || Headers.mem "content-range" h
      || Headers.has_token "cache-control" "no-transform" h
      || List.exists
           (fun d -> Headers.mem d h || Headers.has_token "trailer" d h)
           digests
    in
    let code = Status.code (Response.status resp) in
    let coding () = Lazy.force coding in
    (* A response that the middleware would compress for some requests. The 304
       of caravan.static has the Content-Length of its file; the engine does not
       send it. A HEAD response forwarded by a proxy has a body of no known
       length and the Content-Length of its GET: the HEAD then gets the fields
       of the GET. *)
    let field_length () =
      (* The rule of the engine for the Content-Length of a handler: each value
         is the same number of 1 to 18 digits. *)
      let valid s =
        String.length s <= 18
        && String.for_all (function '0' .. '9' -> true | _ -> false) s
      in
      match Headers.tokens "content-length" h with
      | n :: rest when valid n && List.for_all (String.equal n) rest ->
          Some (int_of_string n)
      | _ ->
          None
    in
    let length () =
      match Response.kind resp with
      | Response.Body b when code <> 304 -> (
        match Body.length b with
        | None when Method.equal (Request.meth req) Method.HEAD ->
            field_length ()
        | l ->
            l )
      | _ ->
          field_length ()
    in
    if skipped || code = 204 || code = 205 || code = 206 then resp
    else if not (in_limits (length ())) then resp
    else
      match (coding (), Response.kind resp) with
      | None, _ ->
          Response.with_headers (with_vary h) resp
      | Some coding, Response.Body old when code <> 304 && take () ->
          let h =
            with_vary (weak_etags coding h)
            |> Headers.remove "content-length"
            |> Headers.remove "accept-ranges"
            |> Headers.set "Content-Encoding"
                 (match coding with Gzip -> "gzip" | Deflate -> "deflate")
          in
          let body =
            Body.of_writer (compressed coding level old)
            |> Body.on_end (fun _ ~bytes:_ ->
                Atomic.decr active ;
                try Body.close old with _ -> () )
          in
          Response.with_body body (Response.with_headers h resp)
      | Some coding, _ ->
          (* A 304, or no free encoder: the ETag and the Vary of a compressed
             200, so that a cache keeps one validator. *)
          Response.with_headers (with_vary (weak_etags coding h)) resp

(* {1 The decompression of request bodies} *)

(* A body that does not decode. Only the decoder raises it, and only the
   middleware sees it. *)
exception Malformed of string

type format = Gzip_format | Zlib_format

(* The CRC-32 of the gzip header, with a table (RFC 1952 §8). *)
let crc_table =
  Array.init 256 (fun n ->
      let c = ref n in
      for _ = 1 to 8 do
        c := if !c land 1 = 1 then 0xedb88320 lxor (!c lsr 1) else !c lsr 1
      done ;
      !c )

let crc_byte crc b = crc_table.(crc lxor b land 0xff) lxor (crc lsr 8)

(* The state of a decoder: made at the first read, thus a body that nobody reads
   costs nothing. [ib] has [history] bytes before each block: at the end of a
   deflate stream, [De.Inf] gives back bytes that it read in an earlier
   block. *)
type decoder =
  {ib: De.bigstring; tmp: Bytes.t; o: De.bigstring; d: De.Inf.decoder}

let history = 8

(* [decoded ~max_bytes ~max_input ~too_large format old] is the body that
   decodes [old]. The framing of gzip (RFC 1952) and zlib (RFC 1950) is parsed
   here, and only the deflate data goes to [De.Inf]: the gzip decoder of
   decompress 1.6 refuses valid input at some splits, the zlib decoder raises on
   some headers, and one state machine counts the compressed bytes and finds the
   bytes after the stream. An error of the decoder sets [too_large] for a size
   error, and each later read raises it again. An error of [old] goes out with
   no change. *)
let decoded ~max_bytes ~max_input ~too_large format old =
  let st =
    lazy
      (let o = De.bigstring_create block in
       { ib= De.bigstring_create (history + block)
       ; tmp= Bytes.create block
       ; o
       ; d= De.Inf.decoder `Manual ~o ~w:(De.make_window ~bits:15) } )
  in
  let ib_pos = ref 0 and ib_len = ref 0 in
  (* The compressed bytes, the decoded bytes of the body, and of a member. *)
  let input = ref 0 and total = ref 0 in
  (* [header] sets it for each member. *)
  let size = ref (0 [@mutate off "header sets it first"]) in
  let o_pos = ref 0 and o_len = ref 0 in
  let digest, start =
    match format with
    | Gzip_format ->
        (Checkseum.Crc32.digest_bigstring, Checkseum.Crc32.default)
    | Zlib_format ->
        (Checkseum.Adler32.digest_bigstring, Checkseum.Adler32.default)
  in
  let check = ref start in
  let phase = ref `Header in
  let error = ref None in
  let fail e =
    error := Some e ;
    raise e
  in
  let malformed m = fail (Malformed m) in
  let too_large_error () =
    too_large := true ;
    fail Body.Too_large
  in
  (* [more st] is [true] if a byte of [old] is in [ib], after a read of the next
     block when [ib] is empty. An error of [old] goes out. *)
  let more st =
    ( if !ib_pos = !ib_len then
        (* After the end, a read of [old] gives 0 again. *)
        let n = Body.read old st.tmp 0 block in
        if n <> 0 then (
          let keep = min history !ib_len in
          Bigarray.Array1.blit
            (Bigarray.Array1.sub st.ib (!ib_len - keep) keep)
            (Bigarray.Array1.sub st.ib (history - keep) keep) ;
          to_bigstring st.tmp 0 st.ib history n ;
          ib_pos := history ;
          ib_len := history + n ;
          input := !input + n ;
          if !input > max_input then too_large_error () ;
          yield () ) ) ;
    !ib_pos < !ib_len
  in
  let byte st =
    if not (more st) then malformed "the body ends in a header or a trailer" ;
    let c = Bigarray.Array1.get st.ib !ib_pos in
    incr ib_pos ; Char.code c
  in
  let le st n =
    let rec go k acc =
      if k = n then acc else go (k + 1) (acc lor (byte st lsl (8 * k)))
    in
    go 0 0
  in
  let be st n =
    let rec go k acc =
      if k = n then acc else go (k + 1) ((acc lsl 8) lor byte st)
    in
    go 0 0
  in
  let gzip_header st =
    let crc = ref 0xffff_ffff in
    let hbyte () =
      let c = byte st in
      crc := crc_byte !crc c ;
      c
    in
    let skip n =
      for _ = 1 to n do
        ignore (hbyte ())
      done
    in
    let skip_string () =
      while hbyte () <> 0 do
        ()
      done
    in
    if hbyte () <> 0x1f || hbyte () <> 0x8b || hbyte () <> 8 then
      malformed "not a gzip member" ;
    let flags = hbyte () in
    if flags land 0xe0 <> 0 then malformed "reserved gzip flags" ;
    (* MTIME, XFL and OS. *)
    skip 6 ;
    ( if flags land 4 <> 0 then
        let lo = hbyte () in
        skip (lo lor (hbyte () lsl 8)) ) ;
    if flags land 8 <> 0 then skip_string () ;
    if flags land 16 <> 0 then skip_string () ;
    if flags land 2 <> 0 then
      let expected = !crc lxor 0xffff_ffff land 0xffff in
      if le st 2 <> expected then malformed "a bad gzip header CRC"
  in
  let zlib_header st =
    let cmf = byte st in
    let flg = byte st in
    let fcheck = (cmf lsl 8) lor flg in
    if
      cmf land 0x0f <> 8
      || cmf lsr 4 > 7
      || fcheck mod 31 <> 0
      || flg land 0x20 <> 0
    then malformed "not a zlib stream"
  in
  let to_u32 t = Optint.to_int t land 0xffff_ffff in
  let header st =
    ( match format with
    | Gzip_format ->
        gzip_header st
    | Zlib_format ->
        zlib_header st ) ;
    De.Inf.reset st.d ;
    check := start ;
    size := 0 ;
    phase := `Data
  in
  let trailer st =
    match format with
    | Gzip_format ->
        let crc = le st 4 in
        let isize = le st 4 in
        if crc <> to_u32 !check || isize <> !size land 0xffff_ffff then
          malformed "a bad gzip trailer" ;
        (* Another member can follow. *)
        phase := if more st then `Header else `Done
    | Zlib_format ->
        if be st 4 <> to_u32 !check then malformed "a bad zlib trailer" ;
        if more st then malformed "bytes after the zlib stream" ;
        phase := `Done
  in
  let decode d =
    try De.Inf.decode d with
    | (Out_of_memory | Stack_overflow) as e ->
        raise e
    | e ->
        malformed (Printexc.to_string e)
  in
  (* [inflate st] runs [De.Inf] until it gives output, or ends. *)
  let rec inflate st =
    match decode st.d with
    | `Await ->
        if more st then (
          De.Inf.src st.d st.ib !ib_pos (!ib_len - !ib_pos) ;
          ib_pos := !ib_len )
        else
          De.Inf.src st.d st.ib (0 [@mutate off "the offset of an empty input"])
            (0
            [@mutate off "a stale byte: the decoder refuses it, the same 400"]
            ) ;
        inflate st
    | `Flush ->
        o_pos := 0 ;
        (* [`Flush] comes with a full buffer. *)
        o_len :=
          (block - De.Inf.dst_rem st.d) [@mutate off "dst_rem is 0 at Flush"] ;
        De.Inf.flush st.d
    | `End ->
        (* At the end of the input, [De.Inf] can end a block with the bits that
           it holds: no trailer can follow. *)
        if De.Inf.src_rem st.d < 0 then
          malformed "the body ends before the trailer" ;
        (* [De.Inf] gives back the bytes of its bit buffer, one for each call:
           also bytes of the block before, thus the history in [ib]. *)
        let rec settle r =
          match decode st.d with
          | `End when De.Inf.src_rem st.d > r ->
              settle (De.Inf.src_rem st.d)
          | _ ->
              r
        in
        ib_pos := !ib_len - settle (De.Inf.src_rem st.d) ;
        o_pos := 0 ;
        o_len := block - De.Inf.dst_rem st.d ;
        phase := `Trailer
    | `Malformed m ->
        malformed m
  in
  (* [step st] gives decoded bytes in [o], or ends. *)
  let rec step st =
    match !phase with
    | `Header ->
        header st ; step st
    | `Data ->
        inflate st ;
        let n = !o_len in
        check := digest st.o 0 n !check ;
        size := !size + n ;
        total := !total + n ;
        if !total > max_bytes then too_large_error () ;
        if n = 0 then step st else yield ()
    | `Trailer ->
        trailer st ; step st
    | `Done ->
        ()
  in
  let read buf off len =
    Option.iter raise !error ;
    let st = Lazy.force st in
    if !o_pos = !o_len && !phase <> `Done then step st ;
    let n = min len (!o_len - !o_pos) in
    of_bigstring st.o !o_pos buf off n ;
    o_pos := !o_pos + n ;
    n
  in
  Body.of_source
    ~trailers:(fun () -> without_digests (Body.trailers old))
    ~close:(fun () -> Body.close old)
    read

let decompress ?(max_bytes = 1_048_576) () =
  if max_bytes < 0 then
    invalid_arg "Caravan_compress.decompress: a negative max_bytes" ;
  (* The compressed bytes can pass the decoded bytes a little: stored blocks.
     The add saturates. *)
  let max_input =
    if
      ( max_bytes > max_int - 65536 )
      [@mutate off "the two give max_int at the limit"]
    then max_int
    else max_bytes + 65536
  in
  let unsupported () =
    Response.string ~status:Status.unsupported_media_type
      ~headers:(Headers.of_list [("Accept-Encoding", "gzip, deflate")])
      "Unsupported Media Type\n"
  in
  let too_large_response () =
    Response.string ~status:Status.content_too_large "Content Too Large\n"
  in
  fun next req ->
    let h = Request.headers req in
    let old = Request.body req in
    let codings =
      List.map String.lowercase_ascii (Headers.tokens "content-encoding" h)
      |> List.filter (fun c -> c <> "identity")
    in
    let decode format =
      match Body.length old with
      | Some n when n > max_input ->
          too_large_response ()
      | _ -> (
          let too_large = ref false in
          let h =
            without_digests
              (Headers.remove "content-encoding"
                 (Headers.remove "content-length" h) )
          in
          let req =
            Request.with_headers h
              (Request.with_body
                 (decoded ~max_bytes ~max_input ~too_large format old)
                 req )
          in
          match next req with
          | r ->
              r
          | exception Body.Too_large when !too_large ->
              too_large_response ()
          (* Only the decoder raises [Malformed]. *)
          | exception Malformed _ ->
              Response.string ~status:Status.bad_request "Bad Request\n" )
    in
    if codings = [] || Body.length old = Some 0 || Headers.mem "content-range" h
    then next req
    else
      match codings with
      | ["gzip"] | ["x-gzip"] ->
          decode Gzip_format
      | ["deflate"] ->
          decode Zlib_format
      | _ ->
          unsupported ()
