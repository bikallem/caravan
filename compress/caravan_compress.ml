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
    for k = 0 to n - 1 do
      Bytes.set out k (Bigarray.Array1.get o k)
    done ;
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
        for k = 0 to n - 1 do
          Bigarray.Array1.set i (!fill + k) (Bytes.get buf (off + k))
        done ;
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
  let t =
    List.fold_left
      (fun t d -> Headers.remove d t)
      (Body.Writer.trailers inner)
      digests
  in
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
       send it. *)
    let length () =
      match Response.kind resp with
      | Response.Body b when code <> 304 ->
          Body.length b
      | _ ->
          Option.bind (Headers.find "content-length" h) int_of_string_opt
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
