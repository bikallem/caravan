type framing =
  | No_body
  | Head_length of int
  | Length of int
  | Chunked
  | Close_delimited

let framing ~meth ~version r =
  match Response.kind r with
  | Response.Upgrade _ ->
      No_body
  | Response.Body body -> (
      let code = Status.code (Response.status r) in
      if code < 200 || code = 204 || code = 304 then No_body
      else if Method.equal meth Method.CONNECT && code < 300 then No_body
      else if Method.equal meth Method.HEAD then
        match Body.length body with Some n -> Head_length n | None -> No_body
      else
        match Body.length body with
        | Some n ->
            Length n
        | None ->
            if version = Version.Http_1_0 then Close_delimited else Chunked )

let wants_close r = Headers.has_token "connection" "close" (Response.headers r)

(* Fields that the connection owns: the handler's are dropped. *)
let connection_fields =
  [ "content-length"
  ; "transfer-encoding"
  ; "connection"
  ; "keep-alive"
  ; "proxy-connection"
  ; "te"
  ; "upgrade" ]

let field w name value =
  Buf_writer.write_string w name ;
  Buf_writer.write_string w ": " ;
  Buf_writer.write_string w value ;
  Buf_writer.write_string w "\r\n"

let head w ?date ~version ~keep_alive framing r =
  let status = Response.status r and headers = Response.headers r in
  let upgrade = Status.code status = 101 in
  Buf_writer.write_string w "HTTP/1.1 " ;
  Buf_writer.write_string w (string_of_int (Status.code status)) ;
  Buf_writer.write_char w ' ' ;
  Buf_writer.write_string w (Status.reason status) ;
  Buf_writer.write_string w "\r\n" ;
  ( match date with
  | Some d when not (Headers.mem "date" headers) ->
      field w "Date" d
  | _ ->
      () ) ;
  let owned name =
    let name = String.lowercase_ascii name in
    List.mem name connection_fields && not (upgrade && name = "upgrade")
  in
  Headers.iter (fun n v -> if not (owned n) then field w n v) headers ;
  ( match framing with
  | Length n | Head_length n ->
      field w "Content-Length" (string_of_int n)
  | Chunked ->
      field w "Transfer-Encoding" "chunked"
  | No_body | Close_delimited ->
      () ) ;
  if upgrade then field w "Connection" "Upgrade"
  else if (not keep_alive) || framing = Close_delimited then
    field w "Connection" "close"
  else if version = Version.Http_1_0 then field w "Connection" "keep-alive" ;
  Buf_writer.write_string w "\r\n"

(* Requests, for a client *)

(* RFC 9110 §8.6: a request of these methods without content has no framing
   field. Other methods send Content-Length: 0. *)
let no_content_methods = Method.[GET; HEAD; DELETE; OPTIONS; TRACE; CONNECT]

let request_framing req =
  match Body.length (Request.body req) with
  | Some 0 when List.exists (Method.equal (Request.meth req)) no_content_methods
    ->
      No_body
  | Some n ->
      Length n
  | None ->
      Chunked

let request_closes req =
  Headers.has_token "connection" "close" (Request.headers req)

let invalid fmt =
  Printf.ksprintf (fun s -> invalid_arg ("H1_write.request_head: " ^ s)) fmt

(* RFC 9112 §3.2: origin-form, authority-form for CONNECT, asterisk-form for a
   server-wide OPTIONS. *)
let request_target req authority =
  match (Request.meth req, Request.path req) with
  | Method.CONNECT, _ ->
      authority
  | Method.OPTIONS, "*" ->
      "*"
  | _, "" ->
      "/"
  | _, p when p.[0] = '/' && not (String.contains p '#') ->
      p
  | _, p ->
      invalid "invalid path %S" p

let authority_and_target req =
  let authority =
    match Request.authority req with
    | Some a ->
        a
    | None ->
        invalid "no authority"
  in
  (authority, request_target req authority)

let check_request req = ignore (authority_and_target req)

let request_head w framing req =
  let authority, target = authority_and_target req
  and headers = Request.headers req in
  (* The fields that Connection names are for this connection only. *)
  let named =
    List.map String.lowercase_ascii (Headers.tokens "connection" headers)
  in
  let owned name =
    let name = String.lowercase_ascii name in
    name = "host" || List.mem name connection_fields || List.mem name named
  in
  Buf_writer.write_string w (Method.to_string (Request.meth req)) ;
  Buf_writer.write_char w ' ' ;
  Buf_writer.write_string w target ;
  Buf_writer.write_string w " HTTP/1.1\r\n" ;
  field w "Host" authority ;
  Headers.iter (fun n v -> if not (owned n) then field w n v) headers ;
  ( match framing with
  | Length n ->
      field w "Content-Length" (string_of_int n)
  | Chunked ->
      field w "Transfer-Encoding" "chunked"
  | No_body | Head_length _ | Close_delimited ->
      () ) ;
  if request_closes req then field w "Connection" "close" ;
  Buf_writer.write_string w "\r\n"

(* RFC 9110 §6.5.1: fields that framing, routing, request modifiers,
   authentication, response control or content processing depend on cannot come
   after the content. A proxied body may carry them as trailers. *)
let not_trailers =
  [ "content-length"
  ; "transfer-encoding"
  ; "trailer"
  ; "te"
  ; "host"
  ; "cache-control"
  ; "expect"
  ; "max-forwards"
  ; "pragma"
  ; "range"
  ; "if-match"
  ; "if-none-match"
  ; "if-modified-since"
  ; "if-unmodified-since"
  ; "if-range"
  ; "authorization"
  ; "proxy-authorization"
  ; "www-authenticate"
  ; "proxy-authenticate"
  ; "cookie"
  ; "set-cookie"
  ; "age"
  ; "date"
  ; "expires"
  ; "location"
  ; "retry-after"
  ; "vary"
  ; "content-encoding"
  ; "content-type"
  ; "content-range"
  ; "connection"
  ; "keep-alive"
  ; "proxy-connection"
  ; "upgrade" ]

exception Length_mismatch

let () =
  Printexc.register_printer (function
    | Length_mismatch ->
        Some "response body length differs from its Content-Length"
    | _ ->
        None )

let body ?(before = ignore) w framing b =
  Fun.protect ~finally:(fun () -> Body.close b)
  @@ fun () ->
  let started = ref false in
  let start () =
    if not !started then (
      started := true ;
      before () )
  in
  let flush () = start () ; Buf_writer.flush w in
  let write_all write =
    Body.write_to b
      (Body.Writer.make
         ~write:(fun buf off len -> start () ; write buf off len)
         ~flush ) ;
    flush ()
  in
  match framing with
  | No_body | Head_length _ ->
      flush ()
  | Close_delimited ->
      write_all (Buf_writer.write_bytes w)
  | Length n ->
      let count = ref 0 in
      write_all (fun buf off len ->
          if len > n - !count then raise Length_mismatch ;
          count := !count + len ;
          Buf_writer.write_bytes w buf off len ) ;
      if !count <> n then raise Length_mismatch
  | Chunked ->
      let chunk buf off len =
        start () ;
        Buf_writer.write_string w (Printf.sprintf "%x\r\n" len) ;
        Buf_writer.write_bytes w buf off len ;
        Buf_writer.write_string w "\r\n"
      in
      let sink = Body.Writer.make ~write:chunk ~flush in
      Body.write_to b sink ;
      start () ;
      Buf_writer.write_string w "0\r\n" ;
      Headers.iter
        (fun n v ->
          if not (List.mem (String.lowercase_ascii n) not_trailers) then
            field w n v )
        (Body.Writer.trailers sink) ;
      Buf_writer.write_string w "\r\n" ;
      flush ()
