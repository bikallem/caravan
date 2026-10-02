open Affect
module Mtime = Affect_unix.Mtime

let limits (c : Config.t) : H1_parse.limits =
  { max_request_line = c.max_request_line; max_header_bytes = c.max_header_bytes;
    max_header_count = c.max_header_count }

(* Network failures end a connection; cancellation must go on. *)
let is_fatal = function Fun.Async.Cancelled -> true | _ -> false

(* The response to a malformed request; the connection closes after it. A
   client that already left makes the write raise Flow.Closed, which ends the
   connection like any other network failure. *)
let error_response w (e : H1_error.t) =
  let status = H1_error.status e in
  let body = Body.of_string (Status.reason status ^ "\n") in
  let r = Response.make ~status ~headers:(Headers.of_list [ ("Content-Type", "text/plain; charset=utf-8") ]) body in
  let framing = H1_write.framing ~meth:Method.GET ~version:Version.Http_1_1 r in
  H1_write.head w ~version:Version.Http_1_1 ~keep_alive:false framing r;
  H1_write.body w framing body;
  `Linger

let request_keep_alive (h : H1_parse.head) =
  match h.version with
  | Version.Http_1_0 -> Headers.has_token "connection" "keep-alive" h.headers
  | _ -> not (Headers.has_token "connection" "close" h.headers)

(* RFC 9110 §10.1.1: 100 Continue only to HTTP/1.1 clients. *)
let expects_continue (h : H1_parse.head) =
  h.version = Version.Http_1_1
  && (match Headers.find "expect" h.headers with
    | Some v -> String.lowercase_ascii v = "100-continue"
    | None -> false)

(* RFC 9112 §9.6: a connection the server ends is closed in stages. Closing
   it over unread input makes the kernel reset it, and a reset can destroy the
   response before the client reads it. So we send our FIN first and read
   until the client closes too, for at most [linger_timeout] and [max_drain]
   bytes. *)
let linger (config : Config.t) flow =
  Flow.set_read_deadline flow
    (Mtime.add_span (Mtime.now ()) config.linger_timeout);
  Flow.shutdown_send flow;
  let buf = Bytes.create 4096 in
  let rec discard left =
    if left > 0
    then match Flow.read flow buf 0 (Int.min left (Bytes.length buf)) with
    | 0 -> ()
    | n -> discard (left - n)
  in
  try discard config.max_drain with Flow.Closed | Flow.Timeout -> ()

let internal_error () =
  Response.string ~status:Status.internal_server_error "Internal Server Error\n"

let serve : Engine.t =
  fun ctx flow handler ->
  let config = ctx.config in
  let limits = limits config in
  let after span () = Mtime.add_span (Mtime.now ()) span in
  (* Reads get a deadline fixed for a whole request head, and renewed before
     each read of a body. Each write gets its own. *)
  let read_deadline = ref (fun () -> None) in
  let r =
    Buf_reader.make ~size:config.buffer_size (fun buf off len ->
        Flow.set_read_deadline flow (!read_deadline ());
        Flow.read flow buf off len)
  in
  let w =
    Buf_writer.make ~size:config.buffer_size (fun buf off len ->
        Flow.set_write_deadline flow (after config.write_timeout ());
        Flow.write flow buf off len)
  in
  let scheme = match Conn_info.scheme ctx.conn with `Http -> "http" | `Https -> "https" in
  let report e = config.on_error e (Printexc.get_raw_backtrace ()) in
  (* Waits at most [idle] for the next request. *)
  let rec next_request ~idle =
    if Buf_reader.buffered r > 0
    then exchange ()
    else
    let input = Flow.readable' flow `Input and stop = Action.map (fun () -> `Stop) ctx.stopping in
    match Action.invoke (Action.choose [ stop; input; Mtime.wait_for' idle `Idle ]) with
    | `Stop | `Idle -> `Close
    | `Input -> if Buf_reader.peek_eof r then `Close else exchange ()
  and exchange () =
    let head_deadline = after config.header_timeout () in
    read_deadline := (fun () -> head_deadline);
    match
      let head = H1_parse.request_head limits r in
      (head, H1_parse.request_framing head, H1_parse.request_target ~scheme head)
    with
    | exception Buf_reader.Eof -> `Close (* the client left in the middle of a request *)
    | exception Flow.Timeout -> error_response w H1_error.Request_timeout
    | exception H1_error.E e -> error_response w e
    | head, framing, target -> (
        read_deadline := after config.read_timeout;
        let committed = ref false and continue_sent = ref false in
        let expect_continue =
          if not (expects_continue head)
          then None
          else
          Some
            (fun () ->
               if not !committed
               then begin
                 continue_sent := true;
                 Buf_writer.write_string w "HTTP/1.1 100 Continue\r\n\r\n";
                 Buf_writer.flush w
               end)
        in
        match H1_body.make ?max_body:config.max_body ?expect_continue ~limits r framing with
        | exception H1_error.E e -> error_response w e
        | body -> respond head target body ~committed ~continue_sent)
  and respond (head : H1_parse.head) (target : H1_parse.target) body ~committed ~continue_sent =
    let req =
      Request.make ~version:head.version ~scheme:target.scheme ?authority:target.authority
        ~headers:head.headers ~body:(H1_body.body body) ~conn:ctx.conn head.meth target.path
    in
    match Fun.Async.get (Fun.Async.call (fun () -> handler req)) with
    | exception H1_error.E e -> error_response w e (* a malformed request body *)
    | exception Body.Truncated -> `Close (* the client left *)
    | exception e when not (is_fatal e) ->
        report e;
        ignore (write ~keep_alive:false head (internal_error ()));
        `Linger
    | response -> (
        match Response.kind response with
        | Response.Upgrade _ ->
            report (Failure "Response.upgrade is not supported yet");
            ignore (write ~keep_alive:false head (internal_error ()));
            `Linger
        | Response.Body _ ->
            let continue_owed =
              expects_continue head && (not !continue_sent) && not (H1_body.is_complete body)
            in
            let keep_alive =
              request_keep_alive head
              && (not (H1_write.wants_close response))
              && (not continue_owed) && not (ctx.is_stopping ())
            in
            committed := true;
            if write ~keep_alive head response && H1_body.drain body ~max:config.max_drain
            then begin
              Fun.Async.yield ();
              next_request ~idle:config.idle_timeout
            end
            else `Linger)
  (* Writes [response]; [true] iff it was written in full and the connection
     can stay open. *)
  and write ~keep_alive (head : H1_parse.head) response =
    let framing = H1_write.framing ~meth:head.meth ~version:head.version response in
    let keep_alive = keep_alive && framing <> H1_write.Close_delimited in
    let date = if config.date_header then Some (Date.now ()) else None in
    H1_write.head w ?date ~version:head.version ~keep_alive framing response;
    match Response.kind response with
    | Response.Upgrade _ -> false
    | Response.Body b -> (
        match H1_write.body w framing b with
        | () -> keep_alive
        | exception ((Flow.Closed | Flow.Timeout) as e) -> raise e
        (* A request body that the response streams was cut short or is
           malformed: the client's fault, not the server's. *)
        | exception (Body.Truncated | H1_error.E _) -> false
        | exception e when not (is_fatal e) -> report e; false)
  in
  try
    match next_request ~idle:config.header_timeout with
    | `Close -> ()
    | `Linger -> linger config flow
  with Flow.Closed | Flow.Timeout -> ()
