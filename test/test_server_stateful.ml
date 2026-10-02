(* A stateful test of HTTP/1.1 connections: generated programs of pipelined
   requests and response reads on several connections, against a model of
   what the server must answer (RFC 9112, RFC 9110). *)

open Windtrap
open Affect
open Affect_http
open Server_support

(* Request kinds. Chunked bodies are HTTP/1.1 only. /ignore also gets a body
   larger than the server's buffer, so that some of it is still unread in the
   kernel when the server drains it or closes over it. *)
let large = String.make 40_000 'x'

let kinds =
  let open List in
  let bodies v10 path =
    [ No_body; Length "data" ]
    @ (if v10 then [] else [ Chunked "data!" ])
    @ if path = "/ignore" then [ Length large ] else []
  in
  concat_map
    (fun v10 ->
       concat_map
         (fun path ->
            concat_map
              (fun conn ->
                 let methods =
                   [ ("GET", No_body); ("HEAD", No_body) ]
                   @ map (fun b -> ("POST", b)) (filter (fun b -> b <> No_body) (bodies v10 path))
                 in
                 map (fun (meth, body) -> { meth; path; v10; conn; body }) methods)
              [ None; Some "close"; Some "keep-alive" ])
         [ "/hello"; "/echo"; "/empty"; "/close"; "/stream"; "/ignore" ])
    [ false; true ]

(* What the server must answer to [k], and whether it then closes. *)
let expected k =
  let head = k.meth = "HEAD" in
  let closes_by_request =
    if k.v10 then k.conn <> Some "keep-alive" else k.conn = Some "close"
  in
  let body_text = match k.body with No_body -> "" | Length s | Chunked s -> s in
  let code, body, closes_by_response =
    match k.path with
    | "/hello" -> (200, "hello", false)
    | "/echo" -> (200, body_text, false)
    | "/empty" -> (204, "", false)
    | "/close" -> (200, "bye", true)
    | "/stream" -> (200, "abcd", k.v10 && not head) (* close-delimited for 1.0 *)
    | _ -> (200, "ignored", false)
  in
  let closes = closes_by_request || closes_by_response in
  (summary code (if head then "" else body) closes, closes)

(* Requests that make the server close over a body still unread in the
   kernel are rare among the kinds, so they get a share of their own. *)
let kind_gen =
  let closes_over_unread k = k.body = Length large && snd (expected k) in
  Gen.frequency
    [ (4, Gen.of_list ~pp:pp_kind kinds);
      (1, Gen.of_list ~pp:pp_kind (List.filter closes_over_unread kinds)) ]

(* The model of a connection. *)
module Model = struct
  type t = {
    mutable owed : string list; (* responses still to read, in order *)
    mutable closing : bool; (* a request that closes the connection was sent *)
    mutable closed : bool; (* the client closed it *)
  }

  let connect () = { owed = []; closing = false; closed = false }

  let send m k =
    cover "HEAD" (k.meth = "HEAD");
    cover "chunked body" (match k.body with Chunked _ -> true | _ -> false);
    cover "HTTP/1.0 kept alive" (k.v10 && k.conn = Some "keep-alive");
    let response, closes = expected k in
    let unread = k.path = "/ignore" && k.body <> No_body in
    cover "unread body drained" (unread && not closes);
    cover "unread body lingered over" (unread && closes);
    m.owed <- m.owed @ [ response ];
    if closes then m.closing <- true

  let receive m =
    cover "two or more responses pipelined" (List.length m.owed >= 2);
    cover "end of a closed connection" (m.owed = [] && m.closing);
    match m.owed with
    | r :: rest -> m.owed <- rest; r
    | [] -> "EOF"

  let close m = m.closed <- true
  let can_send m _ = (not m.closing) && not m.closed
  let can_receive m = (not m.closed) && (m.owed <> [] || m.closing)
  let can_close m = not m.closed
end

(* A real connection, with the kinds sent but not yet answered, to parse HEAD
   responses. *)
module System = struct
  type t = { flow : Flow.t; reader : Buf_reader.t; sent : kind Queue.t }

  let server = ref None

  let connect () =
    let flow = Tcp.connect (Server.local_addr (Option.get !server)) in
    { flow; reader = Buf_reader.of_flow flow; sent = Queue.create () }

  let send c k =
    Queue.push k c.sent;
    Flow.write_string c.flow (serialize k)

  let receive c =
    match Queue.take_opt c.sent with
    | Some k -> read_response ~head:(k.meth = "HEAD") c.reader
    | None -> if Buf_reader.peek_eof c.reader then "EOF" else "unexpected bytes"

  let close c = Flow.close c.flow
end

let conn =
  abstract "c"
    ~pp:(fun ppf (m : Model.t) ->
        Format.fprintf ppf "owed %d%s" (List.length m.owed) (if m.closing then ", closing" else ""))
    ~release:System.close

let commands =
  [ command "connect" (Gen.unit @-> makes conn) Model.connect System.connect;
    command "send" ~pre:Model.can_send (conn ^-> kind_gen @-> returns unit) Model.send System.send;
    command "receive" ~pre:Model.can_receive (conn ^-> returns string) Model.receive System.receive;
    command "close" ~pre:Model.can_close (conn ^-> returns unit) Model.close System.close ]

let stateful_tests =
  group "HTTP/1.1 connections"
    [ stateful ~count:150 ~steps:25 "behave like the model" commands ]

let () =
  let code =
    Affect_unix.Unix.main ~domain_count:1 @@ fun () ->
    let server = Server.start ~config handler in
    System.server := Some server;
    Fun.protect
      ~finally:(fun () -> Server.shutdown server; Server.wait server)
      (fun () -> run "server-stateful" [ stateful_tests ])
  in
  exit code
