(* The tests of the websocket example. Each test runs the handler through the
   real server of caravan, in the simulator of weft. The clients are Ws_client
   of examples/support: they send the handshake and the frames of a browser.
   Each run makes a new room. *)

open Windtrap
open Weft
open Caravan
module T = Caravan_testing
module App = Example_websocket.App
module Ws = Example_support.Ws_client

let pp_frame ppf = function
  | Ws.Text s ->
      Format.fprintf ppf "Text %S" s
  | Binary s ->
      Format.fprintf ppf "Binary %S" s
  | Close (code, reason) ->
      Format.fprintf ppf "Close (%d, %S)" code reason
  | Ping s ->
      Format.fprintf ppf "Ping %S" s
  | Pong s ->
      Format.fprintf ppf "Pong %S" s

let frame = Testable.make ~pp:pp_frame ~equal:( = )

let status = Testable.make ~pp:Status.pp ~equal:Status.equal

(* The origin of the page of the example, as a browser sends it. *)
let page_origin = "http://127.0.0.1:8080"

(* [join t] connects a client to the room, as the page does. The server is that
   of [t] by default. *)
let join ?(origin = Some page_origin) ?(protocols = ["chat"]) ?host ?server t =
  let server = Option.value server ~default:(T.server t) in
  Ws.connect ?origin ~protocols ?host (Server.local_addr server) "/chat"

(* The next frame of [c]: it must be a text. *)
let text c =
  match Ws.recv c with Text s -> s | f -> failf "not a text: %a" pp_frame f

(* [texts c n] are the next [n] texts of [c]. *)
let texts c n = List.init n (fun _ -> text c)

(* [contains s sub] is true if [sub] is in [s]. *)
let contains s sub =
  let n = String.length sub in
  let rec at i =
    i + n <= String.length s && (String.sub s i n = sub || at (i + 1))
  in
  at 0

(* [message i] is the text [i] of a sender: 4 KB, so that some texts fill the
   socket buffers of a peer that reads nothing. *)
let message i = Printf.sprintf "%04d %s" i (String.make 4000 '.')

(* [send_all a n] sends the texts 1 to [n] from [a]. *)
let send_all a n =
  for i = 1 to n do
    Ws.send a (Text (message i))
  done

(* [drain t a] reads the frames of [a] in the background, until the end. *)
let drain t a =
  ignore
    (T.background t (fun () ->
         try
           while true do
             ignore (Ws.recv a)
           done
         with End_of_file -> () ) )

(* [room_texts b n ~got ~left] reads the texts of [b] until it got [n] texts of
   user 1 and "user 3 left". It adds each text of user 1 to [got], the last
   first, and it sets [left] at "user 3 left". *)
let room_texts b n ~got ~left =
  while List.length !got < n || not !left do
    match text b with "user 3 left" -> left := true | s -> got := s :: !got
  done

(* [user_1_texts c] reads the frames of [c] to the end of the connection. It is
   the count of the texts of user 1, or [None] if a frame is not a text. *)
let user_1_texts c =
  let rec go k =
    match Ws.recv c with
    | Text s when String.starts_with ~prefix:"user 1: " s ->
        go (k + 1)
    | Text _ ->
        go k
    | _ ->
        None
    | exception End_of_file ->
        Some k
  in
  go 0

let no_errors t = equal int ~msg:"errors" 0 (List.length (T.take_errors t))

let () =
  exit
  @@ Windtrap.run "websocket"
       [ test "GET / gives the page with the JS client" (fun () ->
             let r = T.run (App.make ()) (fun t -> T.get t "/") in
             equal status Status.ok r.status ;
             equal (option string) (Some "text/html; charset=utf-8")
               (Headers.find "content-type" r.headers) ;
             let has = contains r.body in
             is_true ~msg:"new WebSocket" (has "new WebSocket(") ;
             is_true ~msg:"the chat subprotocol" (has "/chat\", \"chat\")") ;
             (* The page adds each message as its own element, with its text,
                never as HTML. *)
             is_true ~msg:"an element" (has "document.createElement(\"div\")") ;
             is_true ~msg:"textContent" (has "item.textContent = line;") ;
             is_true ~msg:"append" (has "log.append(item);") ;
             is_true ~msg:"the log is a div" (has "<div id=\"log\"") ;
             is_false ~msg:"one text for all" (has "log.textContent") ;
             is_false ~msg:"innerHTML" (has "innerHTML") ;
             is_true ~msg:"the close text"
               (has "show(`closed: ${e.code} ${e.reason}`)") )
       ; test "a text of one client goes to the two clients" (fun () ->
             T.run (App.make ()) (fun t ->
                 let a = join t in
                 equal string "user 1 joined (1 in the room)" (text a) ;
                 let b = join t in
                 let joined = "user 2 joined (2 in the room)" in
                 equal string joined (text a) ;
                 equal string joined (text b) ;
                 Ws.send a (Text "hello") ;
                 equal string "user 1: hello" (text a) ;
                 equal string "user 1: hello" (text b) ;
                 Ws.send b (Text "hi") ;
                 equal string "user 2: hi" (text a) ;
                 equal string "user 2: hi" (text b) ;
                 no_errors t ) )
       ; test "the server chooses the chat subprotocol" (fun () ->
             T.run (App.make ()) (fun t ->
                 equal (option string) (Some "chat") (Ws.protocol (join t)) ) )
       ; test "the pages of the server and clients with no Origin can join"
           (fun () ->
             T.run (App.make ()) (fun t ->
                 List.iteri
                   (fun i origin ->
                     let c = join ~origin t in
                     let n = i + 1 in
                     equal string
                       (Printf.sprintf "user %d joined (%d in the room)" n n)
                       (text c) )
                   [ Some "http://127.0.0.1:8080"
                   ; Some "http://localhost:8080"
                   ; None ] ) )
       ; test "an origin that is not permitted gets 403, and no upgrade"
           (fun () ->
             T.run (App.make ()) (fun t ->
                 let a = join t in
                 equal string "user 1 joined (1 in the room)" (text a) ;
                 (* The request of the curl line of the header comment. *)
                 ( match
                     join ~origin:(Some "https://evil.example") ~protocols:[] t
                   with
                 | _ ->
                     fail "the handshake must give 403"
                 | exception Ws.Refused code ->
                     equal int 403 code ) ;
                 (* The handler did not run: no "user 2 joined". *)
                 Ws.send a (Text "anyone?") ;
                 equal string "user 1: anyone?" (text a) ) )
       ; test "a page of a name that points at the server gets 403" (fun () ->
             (* DNS rebinding: Host and Origin have the name of the attacker.
                The default of Ws.accept permits this origin. *)
             T.run (App.make ()) (fun t ->
                 let origin = Some "http://rebind.test" in
                 match join ~origin ~host:"rebind.test" t with
                 | _ ->
                     fail "the handshake must give 403"
                 | exception Ws.Refused code ->
                     equal int 403 code ) )
       ; test "a client with no chat subprotocol gets Close 1008" (fun () ->
             T.run (App.make ()) (fun t ->
                 let c = join ~protocols:[] t in
                 equal (option string) None (Ws.protocol c) ;
                 equal frame
                   (Close (1008, "use the chat subprotocol"))
                   (Ws.recv c) ;
                 no_errors t ) )
       ; test "a close of one client: the other gets \"left\", and no error"
           (fun () ->
             T.run (App.make ()) (fun t ->
                 let a = join t in
                 let b = join t in
                 ignore (texts a 2) ;
                 ignore (texts b 1) ;
                 (* A browser sends 1001 when the user closes the tab. *)
                 Ws.send a (Close (1001, "")) ;
                 equal frame (Close (1001, "")) (Ws.recv a) ;
                 equal string "user 1 left" (text b) ;
                 (* The room has b and the new client. *)
                 let c = join t in
                 equal string "user 3 joined (2 in the room)" (text c) ;
                 no_errors t ) )
       ; test "a client that leaves with no Close frame: \"left\", no error"
           (fun () ->
             T.run (App.make ()) (fun t ->
                 let a = join t in
                 let b = join t in
                 ignore (texts a 2) ;
                 ignore (texts b 1) ;
                 Ws.close a ;
                 equal string "user 1 left" (text b) ;
                 let c = join t in
                 equal string "user 3 joined (2 in the room)" (text c) ;
                 no_errors t ) )
       ; test "a stop of the server: each client gets Close 1001" (fun () ->
             T.run ~stop_on_signals:true (App.make ()) (fun t ->
                 let a = join t in
                 let b = join t in
                 ignore (texts a 2) ;
                 ignore (texts b 1) ;
                 T.signal t Signal.SIGINT ;
                 List.iter
                   (fun c ->
                     equal frame (Close (1001, "server shutdown")) (Ws.recv c) ;
                     (* The reply of the client ends the closing handshake. *)
                     Ws.send c (Close (1001, "")) )
                   [a; b] ;
                 (* The server closes the connections. The "left" texts do not
                    go: the connections close. *)
                 List.iter
                   (fun c -> raises End_of_file (fun () -> ignore (Ws.recv c)))
                   [a; b] ;
                 no_errors t ) )
       ; test "a ping gets a pong" (fun () ->
             T.run (App.make ()) (fun t ->
                 let a = join t in
                 ignore (texts a 1) ;
                 Ws.send a (Ping "are you there?") ;
                 equal frame (Pong "are you there?") (Ws.recv a) ) )
       ; test "the server sends a Ping each 30 s" (fun () ->
             T.run ~manual:true (App.make ()) (fun t ->
                 let a = join t in
                 ignore (texts a 1) ;
                 let got = ref [] in
                 (* The client answers each Ping with a Pong, as a browser does.
                    The room ignores the Pong. *)
                 let reader =
                   T.background t (fun () ->
                       for _ = 1 to 2 do
                         let f = Ws.recv a in
                         got := f :: !got ;
                         Ws.send a (Pong "")
                       done )
                 in
                 T.advance t (Span.s 29) ;
                 equal (list frame) [] !got ;
                 T.advance t (Span.s 1) ;
                 equal (list frame) [Ping ""] !got ;
                 T.advance t (Span.s 30) ;
                 equal (list frame) [Ping ""; Ping ""] !got ;
                 Promise.await reader ;
                 Ws.send a (Text "still here") ;
                 equal string "user 1: still here" (text a) ;
                 no_errors t ) )
       ; test "a peer with no frame in 45 s gets Close 1001, a Pong is a frame"
           (fun () ->
             T.run ~manual:true (App.make ()) (fun t ->
                 let a = join t in
                 let b = join t in
                 ignore (texts a 2) ;
                 ignore (texts b 1) ;
                 (* a answers each Ping, as a browser does. b sends nothing. *)
                 let rec answer () =
                   match Ws.recv a with
                   | Ping s ->
                       Ws.send a (Pong s) ; answer ()
                   | f ->
                       f
                 in
                 let a_next = T.background t answer in
                 let got = ref [] in
                 let reader =
                   T.background t (fun () ->
                       for _ = 1 to 2 do
                         got := Ws.recv b :: !got
                       done )
                 in
                 T.advance t (Span.ms 44_999) ;
                 equal (list frame) [Ping ""] !got ;
                 T.advance t (Span.ms 1) ;
                 Promise.await reader ;
                 equal (list frame) [Close (1001, ""); Ping ""] !got ;
                 equal frame (Text "user 2 left") (Promise.await a_next) ;
                 (* a sent only Pongs, and it stays after 90 s. *)
                 let a_next = T.background t answer in
                 T.advance t (Span.s 90) ;
                 Ws.send a (Text "still here") ;
                 T.settle t ;
                 equal frame (Text "user 1: still here") (Promise.await a_next) ;
                 no_errors t ) )
       ; test "the room ignores a binary message" (fun () ->
             T.run (App.make ()) (fun t ->
                 let a = join t in
                 ignore (texts a 1) ;
                 Ws.send a (Binary "\x00\x01") ;
                 Ws.send a (Text "text") ;
                 equal string "user 1: text" (text a) ) )
       ; test "a message larger than 4 KiB closes with 1009" (fun () ->
             T.run (App.make ()) (fun t ->
                 let a = join t in
                 let b = join t in
                 ignore (texts a 2) ;
                 ignore (texts b 1) ;
                 let max = String.make 4096 'x' in
                 Ws.send a (Text max) ;
                 equal string ("user 1: " ^ max) (text a) ;
                 equal string ("user 1: " ^ max) (text b) ;
                 Ws.send a (Text (max ^ "x")) ;
                 ( match Ws.recv a with
                 | Close (code, _) ->
                     equal int 1009 code
                 | f ->
                     failf "not a Close: %a" pp_frame f ) ;
                 equal string "user 1 left" (text b) ;
                 no_errors t ) )
       ; test "a peer that the server cancels leaves the room" (fun () ->
             let handler = App.make () in
             T.run handler (fun t ->
                 (* A second server with the same room, on the port 81. *)
                 let addr = Net.Tcp ("127.0.0.1", 81) in
                 Fiber.scope (fun sw ->
                     let config = {Config.default with addr} in
                     let other = Server.start ~config sw handler in
                     let a = join ~server:other t in
                     equal string "user 1 joined (1 in the room)" (text a) ;
                     (* T.run has one domain (the default): the server reads no
                        text of a before the abort. Thus the abort resets the
                        connection of a. With more domains, a can get a text
                        before the Close. *)
                     Ws.send a (Text "late") ;
                     (* The abort cancels the handler of a. *)
                     Server.abort other ;
                     Server.wait other ;
                     equal frame (Close (1001, "server shutdown")) (Ws.recv a) ;
                     raises End_of_file (fun () -> ignore (Ws.recv a)) ) ;
                 let b = join t in
                 equal string "user 2 joined (1 in the room)" (text b) ) )
       ; test "a peer that reads nothing delays the room for at most 5 s"
           (fun () ->
             T.run ~manual:true (App.make ()) (fun t ->
                 let a = join t in
                 let b = join t in
                 let c = join t in
                 ignore (texts a 3) ;
                 ignore (texts b 2) ;
                 (* The client c reads nothing. *)
                 let n = 300 and got = ref [] and left = ref false in
                 let room =
                   T.background t (fun () -> room_texts b n ~got ~left)
                 in
                 drain t a ;
                 ignore (T.background t (fun () -> send_all a n)) ;
                 T.advance t (Span.ms 4999) ;
                 is_false ~msg:"c left" !left ;
                 let before = List.length !got in
                 T.advance t (Span.ms 1) ;
                 Promise.await room ;
                 (* b got all the texts of a, in order. *)
                 equal (list string)
                   (List.init n (fun i -> "user 1: " ^ message (i + 1)))
                   (List.rev !got) ;
                 (* The server closed the connection of c, with no Close frame.
                    Before 5 s, b got the text that waits for c: each send has
                    its own fiber. *)
                 match user_1_texts c with
                 | Some k ->
                     is_true ~msg:"b got more than c" (before > k) ;
                     no_errors t
                 | None ->
                     fail "c got a frame that is not a text" ) ) ]
