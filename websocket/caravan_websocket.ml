open Caravan

let guid = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
let accept_key key = Base64.encode (Sha1.digest (key ^ guid))

type t = Connection.t
type message = Connection.message = Text of string | Binary of string

exception Closed = Connection.Closed

let recv = Connection.recv
let send = Connection.send
let ping = Connection.ping
let close = Connection.close
let protocol = Connection.protocol

(* An origin is "scheme://host[:port]" (RFC 6454 §7). The default check
   permits only the host and the port of the request. *)
let same_origin req origin =
  match (Request.authority req, String.index_opt origin ':') with
  | Some authority, Some i
    when i + 2 < String.length origin && origin.[i + 1] = '/' && origin.[i + 2] = '/' ->
      String.lowercase_ascii (String.sub origin (i + 3) (String.length origin - i - 3))
      = String.lowercase_ascii authority
  | _ -> false

let is_key key = match Base64.decode key with Some k -> String.length k = 16 | None -> false

(* Runs [f] on the connection. If [f] ends, we close the connection. If [f]
   raises, we close with 1011 and give the exception to the server. If the
   server cancels us, we do not close. *)
let run ?protocol ?max_message ?close_timeout ?write_timeout ?read_timeout ?idle_timeout f flow =
  let c = Connection.make ?protocol ?max_message ?close_timeout ?write_timeout ?read_timeout ?idle_timeout flow in
  match f c with
  | () -> Connection.close c
  | exception Closed _ -> ()
  | exception (Affect.Fun.Async.Cancelled as e) -> raise e
  | exception e ->
      let bt = Printexc.get_raw_backtrace () in
      Connection.close ~code:1011 c;
      Printexc.raise_with_backtrace e bt

let accept ?(protocols = []) ?origin ?max_message ?close_timeout ?write_timeout ?read_timeout ?idle_timeout req
    f =
  let h = Request.headers req in
  let refuse ?(status = Status.bad_request) ?headers why =
    Response.string ~status ?headers ("WebSocket handshake: " ^ why ^ "\n")
  in
  let origin_ok () =
    match Headers.find "origin" h with
    | None -> true
    | Some o -> (match origin with Some check -> check o | None -> same_origin req o)
  in
  if not (Method.equal (Request.meth req) Method.GET) then refuse "the method is not GET"
  else if not (Version.equal (Request.version req) Version.Http_1_1)
  then refuse "the version is not HTTP/1.1"
  else if not (Headers.has_token "upgrade" "websocket" h) then refuse "no Upgrade: websocket"
  else if not (Headers.has_token "connection" "upgrade" h) then refuse "no Connection: upgrade"
  else if Headers.find "sec-websocket-version" h <> Some "13"
  then
    refuse ~status:Status.upgrade_required
      ~headers:(Headers.of_list [ ("Sec-WebSocket-Version", "13") ])
      "the version must be 13"
  else
  match Headers.find "sec-websocket-key" h with
  | None -> refuse "no Sec-WebSocket-Key"
  | Some key when not (is_key key) -> refuse "the key is not 16 bytes in base64"
  | Some _ when not (origin_ok ()) -> refuse ~status:Status.forbidden "the origin is not permitted"
  | Some key ->
      let offered = Headers.tokens "sec-websocket-protocol" h in
      let protocol = List.find_opt (fun p -> List.mem p offered) protocols in
      let headers =
        Headers.of_list
          ([ ("Upgrade", "websocket"); ("Sec-WebSocket-Accept", accept_key key) ]
           @ Option.fold ~none:[] ~some:(fun p -> [ ("Sec-WebSocket-Protocol", p) ]) protocol)
      in
      Response.upgrade ~headers
        (run ?protocol ?max_message ?close_timeout ?write_timeout ?read_timeout ?idle_timeout f)

module Private = struct
  module Sha1 = Sha1
  module Base64 = Base64
  module Frame = Frame

  let of_flow ?max_message ?close_timeout ?write_timeout ?read_timeout ?idle_timeout flow =
    Connection.make ?max_message ?close_timeout ?write_timeout ?read_timeout ?idle_timeout flow
end
