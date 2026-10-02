type framing = No_body | Head_length of int | Length of int | Chunked | Close_delimited

let framing ~meth ~version r =
  match Response.kind r with
  | Response.Upgrade _ -> No_body
  | Response.Body body ->
      let code = Status.code (Response.status r) in
      if code < 200 || code = 204 || code = 304
      then No_body
      else if Method.equal meth Method.CONNECT && code < 300
      then No_body
      else if Method.equal meth Method.HEAD
      then match Body.length body with Some n -> Head_length n | None -> No_body
      else match Body.length body with
      | Some n -> Length n
      | None -> if version = Version.Http_1_0 then Close_delimited else Chunked

let wants_close r = Headers.has_token "connection" "close" (Response.headers r)

(* Fields that the connection owns: the handler's are dropped. *)
let connection_fields =
  [ "content-length"; "transfer-encoding"; "connection"; "keep-alive";
    "proxy-connection"; "te"; "upgrade" ]

let field w name value =
  Buf_writer.write_string w name;
  Buf_writer.write_string w ": ";
  Buf_writer.write_string w value;
  Buf_writer.write_string w "\r\n"

let head w ?date ~version ~keep_alive framing r =
  let status = Response.status r and headers = Response.headers r in
  let upgrade = Status.code status = 101 in
  Buf_writer.write_string w "HTTP/1.1 ";
  Buf_writer.write_string w (string_of_int (Status.code status));
  Buf_writer.write_char w ' ';
  Buf_writer.write_string w (Status.reason status);
  Buf_writer.write_string w "\r\n";
  (match date with
  | Some d when not (Headers.mem "date" headers) -> field w "Date" d
  | _ -> ());
  let owned name =
    let name = String.lowercase_ascii name in
    List.mem name connection_fields && not (upgrade && name = "upgrade")
  in
  Headers.iter (fun n v -> if not (owned n) then field w n v) headers;
  (match framing with
  | Length n | Head_length n -> field w "Content-Length" (string_of_int n)
  | Chunked -> field w "Transfer-Encoding" "chunked"
  | No_body | Close_delimited -> ());
  if upgrade
  then field w "Connection" "Upgrade"
  else if not keep_alive || framing = Close_delimited
  then field w "Connection" "close"
  else if version = Version.Http_1_0
  then field w "Connection" "keep-alive";
  Buf_writer.write_string w "\r\n"

exception Length_mismatch

let body w framing b =
  Fun.protect ~finally:(fun () -> Body.close b) @@ fun () ->
  let flush () = Buf_writer.flush w in
  let write_all write =
    Body.write_to b (Body.Writer.make ~write ~flush);
    flush ()
  in
  match framing with
  | No_body | Head_length _ -> ()
  | Close_delimited -> write_all (Buf_writer.write_bytes w)
  | Length n ->
      let count = ref 0 in
      write_all (fun buf off len ->
          if len > n - !count then raise Length_mismatch;
          count := !count + len;
          Buf_writer.write_bytes w buf off len);
      if !count <> n then raise Length_mismatch
  | Chunked ->
      let chunk buf off len =
        Buf_writer.write_string w (Printf.sprintf "%x\r\n" len);
        Buf_writer.write_bytes w buf off len;
        Buf_writer.write_string w "\r\n"
      in
      let sink = Body.Writer.make ~write:chunk ~flush in
      Body.write_to b sink;
      Buf_writer.write_string w "0\r\n";
      Headers.iter (field w) (Body.Writer.trailers sink);
      Buf_writer.write_string w "\r\n";
      flush ()
