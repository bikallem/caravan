(* A small WebSocket client for the tests of the examples. It reads the response
   head and the frames through its own buffer. *)

open Caravan

type t =
  { flow: Flow.t
  ; buf: Bytes.t
  ; mutable pos: int
  ; mutable len: int
  ; mutable protocol: string option }

exception Refused of int

type frame =
  | Text of string
  | Binary of string
  | Close of int * string
  | Ping of string
  | Pong of string

(* The sample key and the sample mask of RFC 6455 (§1.3 and §5.7). *)
let key = "dGhlIHNhbXBsZSBub25jZQ=="

let mask = "\x37\xfa\x21\x3d"

(* Reads more bytes when the buffer has none. A reset of the connection is also
   an end of the input. *)
let fill c =
  if c.pos = c.len then (
    let n =
      try Flow.read c.flow c.buf 0 (Bytes.length c.buf) with Flow.Closed -> 0
    in
    if n = 0 then raise End_of_file ;
    c.pos <- 0 ;
    c.len <- n )

let byte c =
  fill c ;
  let b = Bytes.get_uint8 c.buf c.pos in
  c.pos <- c.pos + 1 ;
  b

(* [take c n] reads [n] bytes. *)
let take c n =
  let out = Buffer.create n in
  while Buffer.length out < n do
    fill c ;
    let k = min (n - Buffer.length out) (c.len - c.pos) in
    Buffer.add_subbytes out c.buf c.pos k ;
    c.pos <- c.pos + k
  done ;
  Buffer.contents out

(* A line of the response head, with no CRLF. *)
let line c =
  let b = Buffer.create 64 in
  let rec next () =
    match Char.chr (byte c) with
    | '\n' ->
        let s = Buffer.contents b in
        if String.ends_with ~suffix:"\r" s then
          String.sub s 0 (String.length s - 1)
        else s
    | ch ->
        Buffer.add_char b ch ; next ()
  in
  next ()

(* The status code and the fields of the response head, with the names in lower
   case. *)
let head c =
  let status =
    match String.split_on_char ' ' (line c) with
    | _ :: code :: _ ->
        int_of_string code
    | _ ->
        failwith "Ws_client: no status line"
  in
  let rec fields acc =
    match line c with
    | "" ->
        List.rev acc
    | l -> (
      match String.index_opt l ':' with
      | Some i ->
          let name = String.lowercase_ascii (String.sub l 0 i) in
          let value = String.sub l (i + 1) (String.length l - i - 1) in
          fields ((name, String.trim value) :: acc)
      | None ->
          fields acc )
  in
  (status, fields [])

let request ?origin ?(protocols = []) ~host path =
  let optional name = function
    | None ->
        ""
    | Some value ->
        name ^ ": " ^ value ^ "\r\n"
  in
  String.concat ""
    [ "GET " ^ path ^ " HTTP/1.1\r\n"
    ; "Host: " ^ host ^ "\r\n"
    ; "Upgrade: websocket\r\n"
    ; "Connection: Upgrade\r\n"
    ; "Sec-WebSocket-Key: " ^ key ^ "\r\n"
    ; "Sec-WebSocket-Version: 13\r\n"
    ; optional "Origin" origin
    ; optional "Sec-WebSocket-Protocol"
        (if protocols = [] then None else Some (String.concat ", " protocols))
    ; "\r\n" ]

let handshake ?origin ?protocols ~host path c =
  Flow.write_string c.flow (request ?origin ?protocols ~host path) ;
  let status, fields = head c in
  if status <> 101 then raise (Refused status) ;
  if
    List.assoc_opt "sec-websocket-accept" fields
    <> Some (Caravan_websocket.accept_key key)
  then failwith "Ws_client: the Sec-WebSocket-Accept is not correct" ;
  c.protocol <- List.assoc_opt "sec-websocket-protocol" fields

let connect ?origin ?protocols ?host addr path =
  let host =
    match host with
    | Some h ->
        h
    | None ->
        Format.asprintf "%a" Conn_info.pp_addr addr
  in
  let flow = Tcp.connect addr in
  let c = {flow; buf= Bytes.create 4096; pos= 0; len= 0; protocol= None} in
  match handshake ?origin ?protocols ~host path c with
  | () ->
      c
  | exception e ->
      Flow.close flow ; raise e

let protocol c = c.protocol

let opcode = function
  | Text _ ->
      1
  | Binary _ ->
      2
  | Close _ ->
      8
  | Ping _ ->
      9
  | Pong _ ->
      10

let payload = function
  | Text s | Binary s | Ping s | Pong s ->
      s
  | Close (code, reason) ->
      let b = Buffer.create (2 + String.length reason) in
      Buffer.add_uint16_be b code ;
      Buffer.add_string b reason ;
      Buffer.contents b

(* A frame with FIN set, the length in 7, 16 or 64 bits, then the masked payload
   (RFC 6455 §5.2). *)
let send c f =
  let p = payload f in
  let n = String.length p in
  let b = Buffer.create (n + 14) in
  Buffer.add_uint8 b (0x80 lor opcode f) ;
  if n < 126 then Buffer.add_uint8 b (0x80 lor n)
  else if n < 0x10000 then (
    Buffer.add_uint8 b (0x80 lor 126) ;
    Buffer.add_uint16_be b n )
  else (
    Buffer.add_uint8 b (0x80 lor 127) ;
    Buffer.add_int64_be b (Int64.of_int n) ) ;
  Buffer.add_string b mask ;
  String.iteri
    (fun i ch ->
      Buffer.add_uint8 b (Char.code ch lxor Char.code mask.[i land 3]) )
    p ;
  Flow.write_string c.flow (Buffer.contents b)

let recv c =
  let b0 = byte c in
  let b1 = byte c in
  if b0 land 0x80 = 0 then failwith "Ws_client: a fragmented message" ;
  if b1 land 0x80 <> 0 then failwith "Ws_client: a masked frame" ;
  let n =
    match b1 land 0x7f with
    | 126 ->
        String.get_uint16_be (take c 2) 0
    | 127 ->
        Int64.to_int (String.get_int64_be (take c 8) 0)
    | n ->
        n
  in
  let p = take c n in
  match b0 land 0x0f with
  | 1 ->
      Text p
  | 2 ->
      Binary p
  | 8 when n >= 2 ->
      Close (String.get_uint16_be p 0, String.sub p 2 (n - 2))
  | 8 ->
      Close (1005, "")
  | 9 ->
      Ping p
  | 10 ->
      Pong p
  | op ->
      failwith (Printf.sprintf "Ws_client: the opcode %d" op)

let close c = Flow.close c.flow
