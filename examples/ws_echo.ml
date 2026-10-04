(* A WebSocket echo server.

   dune exec examples/ws_echo.exe, then open http://localhost:8080/ in a
   browser. The page sends what you type, and the server sends it back. *)

open Caravan
module Ws = Caravan_websocket

let page =
  {|<!doctype html>
<meta charset="utf-8">
<title>caravan WebSocket echo</title>
<form id="form"><input id="text" autofocus> <button>Send</button></form>
<pre id="log"></pre>
<script>
  const log = (line) => { document.getElementById("log").textContent += line + "\n"; };
  const ws = new WebSocket(`ws://${location.host}/echo`);
  ws.onopen = () => log("open");
  ws.onmessage = (e) => log("< " + e.data);
  ws.onclose = (e) => log(`closed ${e.code} ${e.reason}`);
  document.getElementById("form").onsubmit = (e) => {
    e.preventDefault();
    const text = document.getElementById("text");
    ws.send(text.value);
    log("> " + text.value);
    text.value = "";
  };
</script>
|}

let echo c =
  let rec loop () =
    Ws.send c (Ws.recv c);
    loop ()
  in
  loop ()

let handler req =
  match Request.path_only req with
  | "/" -> Response.string ~content_type:"text/html; charset=utf-8" page
  | "/echo" -> Ws.accept req echo
  | _ -> Handler.not_found req

let main () =
  Server.run handler ~on_start:(fun server ->
      Format.printf "Open http://%a/ (Ctrl-C to stop)@." Conn_info.pp_addr
        (Server.local_addr server))

let () = if !Sys.interactive then () else main ()
