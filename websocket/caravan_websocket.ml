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

let without_port port s =
  let suffix = ":" ^ string_of_int port in
  if String.ends_with ~suffix s then
    String.sub s 0 (String.length s - String.length suffix)
  else s

(* An origin is "scheme://host[:port]" (RFC 6454 §7). The default check permits
   only the host and the port of the request. A missing port is the default port
   of the scheme of the origin. An https request does not permit an http origin.
   An http request permits the two schemes, because a proxy can terminate TLS in
   front of the server. *)
let same_origin req origin =
  let origin = String.lowercase_ascii origin in
  match (Request.authority req, String.index_opt origin ':') with
  | Some authority, Some i
    when i + 2 < String.length origin
         && origin.[i + 1] = '/'
         && origin.[i + 2] = '/' -> (
      let host = String.sub origin (i + 3) (String.length origin - i - 3) in
      let same port =
        without_port port host
        = without_port port (String.lowercase_ascii authority)
      in
      match String.sub origin 0 i with
      | "https" ->
          same 443
      | "http" ->
          Request.scheme req <> "https" && same 80
      | _ ->
          false )
  | _ ->
      false

let is_key key =
  match Base64.decode key with Some k -> String.length k = 16 | None -> false

(* Runs [f] on the connection. If [f] ends, we close the connection. If [f]
   raises, we close with 1011 and give the exception to the server. If the
   server cancels us, we do not close. A [Cancelled] from a different call, for
   example a child of [f], is an exception of [f]. A [Closed] is not an
   exception of [f] only if our connection closes: else it comes from a
   different connection. When [stopping] is enabled, we close with 1001 (RFC
   6455 §7.4.1: an endpoint that goes away). *)
let run ?(stopping = Affect.Action.never) ?protocol ?max_message ?close_timeout
    ?write_timeout ?read_timeout ?idle_timeout f flow =
  let open Affect in
  let c =
    Connection.make ?protocol ?max_message ?close_timeout ?write_timeout
      ?read_timeout ?idle_timeout flow
  in
  let watch =
    Fun.Async.call (fun () ->
        Action.invoke stopping ;
        Connection.close ~code:1001 ~reason:"server shutdown" c )
  in
  (* If the stop started a closing handshake, [close] waits for its end and
     sends no second Close frame. Thus the end of [f] does not stop the
     handshake. The connection call waits for the end of [watch]. *)
  let finish ?code () = Connection.close ?code c ; Fun.Async.cancel watch in
  match f c with
  | () ->
      finish ()
  | exception Closed _ when Connection.is_closing c ->
      finish ()
  | exception (Fun.Async.Cancelled as e) when Fun.Async.is_current_cancelled ()
    ->
      (* The cancel goes to [watch] too. *)
      Printexc.raise_with_backtrace e (Printexc.get_raw_backtrace ())
  | exception e ->
      let bt = Printexc.get_raw_backtrace () in
      finish ~code:1011 () ;
      (* The server does not report a Cancelled, thus we give a Failure. *)
      let e =
        match e with
        | Fun.Async.Cancelled ->
            Failure "Caravan_websocket: a call of the handler was cancelled"
        | e ->
            e
      in
      Printexc.raise_with_backtrace e bt

let accept ?(protocols = []) ?origin ?max_message ?close_timeout ?write_timeout
    ?read_timeout ?idle_timeout req f =
  (* The connection starts after the response: we examine the limit at this
     time. *)
  if Option.fold ~none:false ~some:(fun m -> m < 0) max_message then
    invalid_arg "Caravan_websocket.accept: max_message is negative" ;
  let h = Request.headers req in
  let refuse ?(status = Status.bad_request) ?headers why =
    Response.string ~status ?headers ("WebSocket handshake: " ^ why ^ "\n")
  in
  let origin_ok () =
    match Headers.find "origin" h with
    | None ->
        true
    | Some o -> (
      match origin with Some check -> check o | None -> same_origin req o )
  in
  if not (Method.equal (Request.meth req) Method.GET) then
    refuse "the method is not GET"
  else if not (Version.equal (Request.version req) Version.Http_1_1) then
    refuse "the version is not HTTP/1.1"
  else if not (Headers.has_token "upgrade" "websocket" h) then
    refuse "no Upgrade: websocket"
  else if not (Headers.has_token "connection" "upgrade" h) then
    refuse "no Connection: upgrade"
  else if Headers.find "sec-websocket-version" h <> Some "13" then
    refuse ~status:Status.upgrade_required
      ~headers:(Headers.of_list [("Sec-WebSocket-Version", "13")])
      "the version must be 13"
  else
    match Headers.find "sec-websocket-key" h with
    | None ->
        refuse "no Sec-WebSocket-Key"
    | Some key when not (is_key key) ->
        refuse "the key is not 16 bytes in base64"
    | Some _ when not (origin_ok ()) ->
        refuse ~status:Status.forbidden "the origin is not permitted"
    | Some key ->
        let offered = Headers.tokens "sec-websocket-protocol" h in
        let protocol = List.find_opt (fun p -> List.mem p offered) protocols in
        let headers =
          Headers.of_list
            ( [ ("Upgrade", "websocket")
              ; ("Sec-WebSocket-Accept", accept_key key) ]
            @ Option.fold ~none:[]
                ~some:(fun p -> [("Sec-WebSocket-Protocol", p)])
                protocol )
        in
        let stopping = Option.map Conn_info.stopping (Request.conn req) in
        Response.upgrade ~headers
          (run ?stopping ?protocol ?max_message ?close_timeout ?write_timeout
             ?read_timeout ?idle_timeout f )

module Private = struct
  module Sha1 = Sha1
  module Base64 = Base64
  module Frame = Frame
  module Utf8 = Utf8

  let of_flow ?max_message ?close_timeout ?write_timeout ?read_timeout
      ?idle_timeout flow =
    Connection.make ?max_message ?close_timeout ?write_timeout ?read_timeout
      ?idle_timeout flow

  let is_closing = Connection.is_closing

  let run_handler ?stopping ?close_timeout f flow =
    run ?stopping ?close_timeout f flow
end
