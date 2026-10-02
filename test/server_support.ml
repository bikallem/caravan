(* Shared by the server test suites: a handler, request kinds, and a small
   client that parses responses. *)

open Affect
open Affect_http
module Mtime = Affect_unix.Mtime
module Buf_reader = Private.Buf_reader

let loopback = Unix.ADDR_INET (Unix.inet_addr_loopback, 0)

(* Handlers read request bodies, except /ignore, which leaves its body to be
   drained or lingered over. *)
let handler req =
  let body = Request.body req in
  let read () = ignore (Body.to_string body) in
  match Request.path_only req with
  | "/hello" -> read (); Response.string "hello"
  | "/echo" -> Response.stream ?length:(Body.length body) (fun w -> Body.write_to body w)
  | "/empty" -> read (); Response.empty ()
  | "/close" ->
      read ();
      Response.string ~headers:(Headers.of_list [ ("Connection", "close") ]) "bye"
  | "/stream" ->
      read ();
      Response.stream (fun w ->
          Body.Writer.write_string w "ab";
          Body.Writer.flush w;
          Body.Writer.write_string w "cd")
  | "/ignore" -> Response.string "ignored"
  | "/drip" ->
      Response.stream (fun w ->
          for i = 1 to 3 do
            Body.Writer.write_string w (string_of_int i);
            Body.Writer.flush w;
            Mtime.wait_for Mtime.Span.(100 * ms)
          done)
  | _ -> read (); Response.string ~status:Status.not_found "not found"

let config = { Config.default with addr = loopback; date_header = false }

(* Requests *)

type body = No_body | Length of string | Chunked of string

type kind = {
  meth : string;
  path : string;
  v10 : bool;
  conn : string option; (* the Connection field, if any *)
  body : body;
}

let pp_kind ppf k =
  Format.fprintf ppf "%s %s %s%s%s" k.meth k.path
    (if k.v10 then "1.0" else "1.1")
    (match k.conn with None -> "" | Some c -> " conn:" ^ c)
    (match k.body with
    | No_body -> ""
    | Length s when String.length s > 16 -> Printf.sprintf " len:%d" (String.length s)
    | Length s -> Printf.sprintf " len:%S" s
    | Chunked s -> Printf.sprintf " chunked:%S" s)

let serialize k =
  let fields, body =
    match k.body with
    | No_body -> ("", "")
    | Length s -> (Printf.sprintf "Content-Length: %d\r\n" (String.length s), s)
    | Chunked s ->
        ("Transfer-Encoding: chunked\r\n", Printf.sprintf "%x\r\n%s\r\n0\r\n\r\n" (String.length s) s)
  in
  Printf.sprintf "%s %s %s\r\nHost: x\r\n%s%s\r\n%s" k.meth k.path
    (if k.v10 then "HTTP/1.0" else "HTTP/1.1")
    (match k.conn with None -> "" | Some c -> "Connection: " ^ c ^ "\r\n")
    fields body

(* Responses, summarised as "status body" and " close" when the server says
   it will close. *)

let summary code body close = Printf.sprintf "%d %S%s" code body (if close then " close" else "")

let read_response ~head r =
  let status = Buf_reader.read_line r ~max:1000 in
  let code = int_of_string (String.sub status 9 3) in
  let rec fields acc =
    match Buf_reader.read_line r ~max:1000 with
    | "" -> acc
    | line ->
        let i = String.index line ':' in
        let name = String.lowercase_ascii (String.sub line 0 i) in
        let value = String.trim (String.sub line (i + 1) (String.length line - i - 1)) in
        fields ((name, String.lowercase_ascii value) :: acc)
  in
  let fields = fields [] in
  let find n = List.assoc_opt n fields in
  let close = find "connection" = Some "close" in
  let rec to_eof acc =
    let buf = Bytes.create 4096 in
    match Buf_reader.read_into r buf 0 4096 with
    | 0 -> acc
    | n -> to_eof (acc ^ Bytes.sub_string buf 0 n)
  in
  let body =
    if head || code = 204 || code = 304
    then ""
    else if find "transfer-encoding" = Some "chunked"
    then Body.to_string (Private.H1_body.chunked ~limits:Private.H1_parse.default_limits r)
    else
    match find "content-length" with
    | Some n ->
        let n = int_of_string n in
        let buf = Bytes.create n in
        let rec fill got = if got < n then fill (got + Buf_reader.read_into r buf got (n - got)) in
        fill 0;
        Bytes.to_string buf
    | None -> to_eof ""
  in
  summary code body close

(* Runs [f server] with a server inside affect's scheduler. *)
(* Two domains unless TEST_DOMAINS says otherwise: mutation testing forks,
   which OCaml forbids once a domain was spawned. *)
let domain_count =
  Option.value ~default:2 (Option.bind (Sys.getenv_opt "TEST_DOMAINS") int_of_string_opt)

let with_server ?(config = config) f =
  Affect_unix.Unix.main ~domain_count @@ fun () ->
  let server = Server.start ~config handler in
  Fun.protect ~finally:(fun () -> Server.shutdown server; Server.wait server) (fun () -> f server)

