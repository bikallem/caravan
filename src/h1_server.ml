open Affect

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
  H1_write.body w framing body

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

let internal_error () =
  Response.string ~status:Status.internal_server_error "Internal Server Error\n"

let serve : Engine.t =
  fun ctx flow handler ->
  let config = ctx.config in
  let limits = limits config in
  let r = Buf_reader.of_flow ~size:config.buffer_size flow in
  let w = Buf_writer.of_flow ~size:config.buffer_size flow in
  let scheme = match Conn_info.scheme ctx.conn with `Http -> "http" | `Https -> "https" in
  let report e = config.on_error e (Printexc.get_raw_backtrace ()) in
  let rec next_request () =
    if Buf_reader.buffered r > 0
    then exchange ()
    else
    let input = Flow.readable' flow `Input and stop = Action.map (fun () -> `Stop) ctx.stopping in
    match Action.invoke (Action.choose [ stop; input ]) with
    | `Stop -> ()
    | `Input -> if not (Buf_reader.peek_eof r) then exchange ()
  and exchange () =
    match
      let head = H1_parse.request_head limits r in
      (head, H1_parse.request_framing head, H1_parse.request_target ~scheme head)
    with
    | exception Buf_reader.Eof -> () (* the client left in the middle of a request *)
    | exception H1_error.E e -> error_response w e
    | head, framing, target -> (
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
    | exception Body.Truncated -> () (* the client left *)
    | exception e when not (is_fatal e) ->
        report e;
        ignore (write ~keep_alive:false head (internal_error ()))
    | response -> (
        match Response.kind response with
        | Response.Upgrade _ ->
            report (Failure "Response.upgrade is not supported yet");
            ignore (write ~keep_alive:false head (internal_error ()))
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
              next_request ()
            end)
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
        | exception e when not (is_fatal e) -> report e; false)
  in
  try next_request () with Flow.Closed | Flow.Timeout -> ()
