(* A chat room over WebSocket. Each text of a peer goes to all peers of the
   room, also to the sender.

   Run dune exec examples/websocket/main.exe, then open http://127.0.0.1:8080/
   in two tabs of a browser. A text that you send in one tab shows in the two
   tabs. When you close the first tab, the other tab shows "user 1 left". When
   you stop the server with Ctrl-C, each tab shows "closed: 1001 server
   shutdown".

   The handshake of a page from another site gets 403. First, set these shell
   variables:

   U='-H Connection:Upgrade -H Upgrade:websocket -H Sec-WebSocket-Version:13'
   K=Sec-WebSocket-Key:dGhlIHNhbXBsZSBub25jZQ==

   curl -i $U -H $K -H Origin:https://evil.example 127.0.0.1:8080/chat

   Optional: websocat (https://github.com/vi/websocat) is a WebSocket client for
   the terminal. It sends no Origin: the server permits a client that is not a
   browser. Type a line to send it:

   websocat --protocol chat ws://127.0.0.1:8080/chat

   The room has no authentication, no limit of peers and no rate limit. For
   production, limit the peers, the messages of each peer, and the connections
   of each address. *)

open Weft
open Caravan
open Caravan_html
module Ws = Caravan_websocket

(* The origins of the pages that can join the room: the page of this server on
   port 8080. Add the origin of each other site whose pages can join, for
   example https://chat.example. A browser sends the cookies of the user with
   the handshake, thus a page of another site must not join.

   By default, Ws.accept permits the origin with the host of the Host field. A
   page of a name that the attacker points at 127.0.0.1 (DNS rebinding) sends
   the same name in Host and in Origin. The fixed list stops it. A CORS policy
   does not apply to WebSocket: the code uses Cors.permits only to examine the
   Origin with the list. *)
let origins =
  Caravan_middleware.Cors.Only ["http://127.0.0.1:8080"; "http://localhost:8080"]

(* The page with the JS client. It adds each message as its own div element,
   with textContent, never as HTML: thus a message cannot run a script in the
   page of another user. A div shows a newline as a space, thus a newline in a
   message does not start a new line. *)
let script =
  {|const log = document.getElementById("log");
const show = (line) => {
  const item = document.createElement("div");
  item.textContent = line;
  log.append(item);
};
const scheme = location.protocol === "https:" ? "wss://" : "ws://";
const ws = new WebSocket(scheme + location.host + "/chat", "chat");
ws.onmessage = (e) => show(e.data);
ws.onclose = (e) => show(`closed: ${e.code} ${e.reason}`);
document.getElementById("form").onsubmit = (e) => {
  e.preventDefault();
  const text = document.getElementById("text");
  ws.send(text.value);
  text.value = "";
};|}

let page =
  list
    [ doctype
    ; El.html
        ~at:[At.lang "en"]
        [ El.head
            [El.meta ~at:[At.v "charset" "utf-8"] (); El.title "caravan chat"]
        ; El.body
            [ El.h1 [txt "Chat"]
            ; El.form
                ~at:[At.id "form"]
                [ El.input ~at:[At.id "text"; At.autofocus] ()
                ; El.button [txt "Send"] ]
            ; El.div ~at:[At.id "log"; At.aria "live" "polite"] []
            ; El.script script ] ] ]

(* A peer of the room. *)
type peer = {name: string; ws: Ws.t}

(* The state of the room: the peers, and the count of all peers that joined. The
   handlers of the peers run at the same time, also on many domains, thus the
   state is in Atomic values. *)
type room = {peers: peer list Atomic.t; count: int Atomic.t}

(* Sets [a] to [f] of its value. A failed compare-and-set does it again. *)
let rec update a f =
  let old = Atomic.get a in
  if not (Atomic.compare_and_set a old (f old)) then update a f

(* Sends [text] to each peer, each send in its own fiber. A peer can close at
   any time: then its send raises Closed. The fiber catches it, thus a closed
   peer does not stop the sender, and the server reports no error.

   The sender waits for the end of all its sends: thus each peer gets the texts
   of one sender in their order. A peer that reads nothing delays the broadcast
   for at most 5 s, the write timeout. Then its connection closes, and the room
   continues without it. *)
let broadcast room text =
  Fiber.scope (fun sw ->
      List.iter
        (fun peer ->
          Fiber.fork sw (fun () ->
              try Ws.send peer.ws (Text text) with Ws.Closed _ -> () ) )
        (Atomic.get room.peers) )

(* Each 30 s, a Ping goes to the peer. Thus a proxy sees traffic, and does not
   close the connection because it is idle. The peer sends a Pong, and Ws.recv
   ignores it. *)
let keepalive ws =
  try
    while true do
      Time.sleep Mono (Span.s 30) ;
      Ws.ping ws ""
    done
  with Ws.Closed _ -> ()

(* Sends each text of the peer to the room. The room takes only text: it ignores
   a binary message. It ends when the connection closes: the peer closed it, it
   sent a message larger than 4 KiB (code 1009), or the server stops. *)
let rec talk room me =
  match Ws.recv me.ws with
  | Text text ->
      broadcast room (me.name ^ ": " ^ text) ;
      talk room me
  | Binary _ ->
      talk room me
  | exception Ws.Closed _ ->
      ()

(* The connection of a peer. The names are "user 1", "user 2" and so on. The
   join text tells the count of peers in the room. When the connection closes,
   the peer leaves the room, and the other peers get the "left" text. At a
   cancel or another exception, the peer also leaves the room. *)
let join room ws =
  let name = Printf.sprintf "user %d" (Atomic.fetch_and_add room.count 1 + 1) in
  let me = {name; ws} in
  update room.peers (List.cons me) ;
  let leave () = update room.peers (List.filter (fun p -> p != me)) in
  Fun.protect ~finally:leave (fun () ->
      let peers = List.length (Atomic.get room.peers) in
      broadcast room (Printf.sprintf "%s joined (%d in the room)" name peers) ;
      Fiber.scope (fun sw ->
          Fiber.fork_daemon sw (fun () -> keepalive ws) ;
          talk room me ) ) ;
  broadcast room (name ^ " left")

(* A client that does not offer the chat subprotocol gets a Close frame with the
   code 1008 (policy violation). *)
let session room ws =
  if Ws.protocol ws = Some "chat" then join room ws
  else Ws.close ~code:1008 ~reason:"use the chat subprotocol" ws

(* GET / gives the page. /chat accepts the handshake of a permitted origin, with
   the chat subprotocol. A message from a peer can have at most 4 KiB. When the
   server stops, Ws.accept sends a Close frame with the code 1001 to each peer.

   A peer that sends no frame in 45 s gets a Close frame with 1001 (the idle
   timeout). A browser answers each Ping of keepalive with a Pong, thus an open
   page stays. ~read_timeout limits each read inside a frame (see
   caravan_websocket.mli). *)
let make () =
  let room = {peers= Atomic.make []; count= Atomic.make 0} in
  fun req ->
    match (Request.meth req, Request.path_only req) with
    | GET, "/" ->
        response page
    | _, "/chat" ->
        Ws.accept
          ~origin:(Caravan_middleware.Cors.permits origins)
          ~protocols:["chat"] ~max_message:4096 ~write_timeout:(Span.s 5)
          ~idle_timeout:(Span.s 45) req (session room)
    | _ ->
        Handler.not_found req
