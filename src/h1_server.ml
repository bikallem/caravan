open Affect

let limits (c : Config.t) : H1_parse.limits =
  { max_request_line= c.max_request_line
  ; max_header_bytes= c.max_header_bytes
  ; max_header_count= c.max_header_count }

(* Network failures end a connection; cancellation must go on. *)
let is_fatal = function Fun.Async.Cancelled -> true | _ -> false

(* The response to a malformed request; the connection closes after it. A client
   that already left makes the write raise Flow.Closed, which ends the
   connection like any other network failure. *)
let error_response ?date ?(meth = Method.GET) w (e : H1_error.t) =
  let status = H1_error.status e in
  let body = Body.of_string (Status.reason status ^ "\n") in
  let r =
    Response.make ~status
      ~headers:(Headers.of_list [("Content-Type", "text/plain; charset=utf-8")])
      body
  in
  (* A HEAD request gets the head only. *)
  let framing = H1_write.framing ~meth ~version:Version.Http_1_1 r in
  H1_write.head w ?date ~version:Version.Http_1_1 ~keep_alive:false framing r ;
  H1_write.body w framing body ;
  `Linger

let request_keep_alive (h : H1_parse.head) =
  match h.version with
  | Version.Http_1_0 ->
      Headers.has_token "connection" "keep-alive" h.headers
  | _ ->
      not (Headers.has_token "connection" "close" h.headers)

(* RFC 9110 §10.1.1: [100-continue] is the only expectation, and HTTP/1.0
   clients' is ignored. *)
let expectation (h : H1_parse.head) =
  match Headers.tokens "expect" h.headers with
  | [] ->
      `None
  | ts when List.for_all (fun t -> String.lowercase_ascii t = "100-continue") ts
    ->
      if h.version = Version.Http_1_1 then `Continue else `None
  | _ ->
      `Unknown

let expects_continue h = expectation h = `Continue

(* RFC 9112 §9.6: a connection that the server ends closes in stages, for at
   most [linger_timeout] and [max_drain] bytes. *)
let linger (config : Config.t) flow =
  Flow.set_read_deadline flow
    (Time.after (Flow.time flow) config.linger_timeout) ;
  Flow.linger flow ~max:config.max_drain

(* A protocol of an Upgrade field: its name and its version, split at the first
   '/'. *)
let protocol p =
  match String.index_opt p '/' with
  | Some i ->
      (String.sub p 0 i, Some (String.sub p (i + 1) (String.length p - i - 1)))
  | None ->
      (p, None)

(* RFC 9110 §7.8: the names compare without case. RFC 9110 has no rule for the
   versions: they must be the same text, and no version matches only no
   version. *)
let same_protocol a b =
  let name, version = protocol a and name', version' = protocol b in
  String.lowercase_ascii name = String.lowercase_ascii name'
  && version = version'

(* Why [response], an upgrade, cannot answer the request [head], if it cannot:
   RFC 9110 §7.8, §9.3.6 and §15.2.2. *)
let upgrade_error (head : H1_parse.head) response =
  if Status.code (Response.status response) = 101 then
    if head.version <> Version.Http_1_1 then Some "101 to an HTTP/1.0 request"
    else
      let offered = Headers.tokens "upgrade" head.headers in
      if
        not
          ( offered <> []
          && Headers.has_token "connection" "upgrade" head.headers )
      then Some "101 to a request that did not ask for an upgrade"
      else
        match Headers.tokens "upgrade" (Response.headers response) with
        | [] ->
            Some "101 without an Upgrade field"
        | protocols -> (
          (* RFC 9110 §7.8: no switch to a protocol that the client did not
             offer. *)
          match
            List.find_opt
              (fun p -> not (List.exists (same_protocol p) offered))
              protocols
          with
          | Some p ->
              Some ("101 to a protocol that the request did not offer: " ^ p)
          | None ->
              None )
  else if not (Method.equal head.meth Method.CONNECT) then
    Some (Format.asprintf "2xx upgrade to a %a request" Method.pp head.meth)
  else None

(* RFC 9110 §7.8: a 101 to a request that expects 100-continue comes after a
   100, also when the request has no content. A CONNECT request has no content
   (§9.3.6): a tunnel gets the 100 only for a body still owed. *)
let continue_before_switch response body =
  if Status.code (Response.status response) = 101 || H1_body.continue_owed body
  then H1_body.send_continue body

let internal_error () =
  Response.string ~status:Status.internal_server_error "Internal Server Error\n"

let serve : Engine.t =
 fun ctx flow handler ->
  let config = ctx.config in
  let limits = limits config in
  let time = Flow.time flow in
  let after span () = Time.after time span in
  (* Reads get a deadline fixed for a whole request head, and renewed before
     each read of a body. Each write gets its own. *)
  let read_deadline = ref (fun () -> None) in
  let r =
    Buf_reader.make ~size:config.buffer_size ~read_now:(Flow.read_now flow)
      (fun buf off len ->
        Flow.set_read_deadline flow (!read_deadline ()) ;
        Flow.read flow buf off len )
  in
  (* Discarding a request body the handler left unread gets one deadline for all
     of it, as lingering does: renewed per read, a client trickling bytes could
     keep the connection for days. *)
  let drain body =
    let deadline = after config.read_timeout () in
    (read_deadline := fun () -> deadline) ;
    H1_body.drain body ~max:config.max_drain
  in
  let w =
    Buf_writer.make ~size:config.buffer_size (fun buf off len ->
        Flow.set_write_deadline flow (after config.write_timeout ()) ;
        Flow.write flow buf off len )
  in
  let scheme =
    match Conn_info.scheme ctx.conn with `Http -> "http" | `Https -> "https"
  in
  let report e = config.on_error e (Printexc.get_raw_backtrace ()) in
  let date () =
    if config.date_header then Some (Date.of_posix (Time.now config.posix_time))
    else None
  in
  let error ?meth e = error_response ?date:(date ()) ?meth w e in
  (* Waits at most [idle] for the next request. A stopping server takes no new
     request, even one already received. *)
  let rec next_request ~idle =
    if ctx.is_stopping () then
      if Buf_reader.buffered r > 0 then `Linger else `Close
    else if Buf_reader.buffered r > 0 then exchange ()
    else wait_for_input (after idle ())
  (* A wakeup can find nothing to read: readiness can be stale, or a TLS record
     incomplete. Then the wait goes on, to the same deadline; a blocking read
     would ignore both it and a stopping server. *)
  and wait_for_input deadline =
    let input = Flow.readable' flow `Input
    and stop = Action.map (fun () -> `Stop) ctx.stopping in
    match
      Action.invoke (Action.choose [stop; input; Time.at time deadline `Idle])
    with
    | `Stop | `Idle ->
        `Close
    | `Input -> (
      match Buf_reader.fill_now r with
      | `Bytes ->
          exchange ()
      | `Eof ->
          `Close
      | `Would_block ->
          wait_for_input deadline )
  and exchange () =
    let head_deadline = after config.header_timeout () in
    (read_deadline := fun () -> head_deadline) ;
    match H1_parse.request_head limits r with
    | exception Buf_reader.Eof ->
        `Close (* the client left in the middle of a request *)
    | exception Flow.Timeout ->
        error H1_error.Request_timeout
    | exception H1_error.E e ->
        error e
    | head -> (
      match
        let framing = H1_parse.request_framing head in
        let target = H1_parse.request_target ~scheme head in
        if expectation head = `Unknown then
          raise (H1_error.E H1_error.Expectation_failed) ;
        (framing, target)
      with
      | exception H1_error.E e ->
          error ~meth:head.meth e
      | framing, target -> (
          read_deadline := after config.read_timeout ;
          let committed = ref false in
          let expect_continue =
            if not (expects_continue head) then None
            else
              Some
                (fun () ->
                  (* A client that cannot take it left. *)
                  if not !committed then
                    try
                      Buf_writer.write_string w "HTTP/1.1 100 Continue\r\n\r\n" ;
                      Buf_writer.flush w
                    with Flow.Closed | Flow.Timeout -> raise Body.Truncated )
          in
          match
            H1_body.make ?max_body:config.max_body ?expect_continue ~limits r
              framing
          with
          | exception H1_error.E e ->
              error ~meth:head.meth e
          | body ->
              respond head target body ~committed ) )
  and respond (head : H1_parse.head) (target : H1_parse.target) body ~committed
      =
    let req =
      Request.make ~version:head.version ~scheme:target.scheme
        ?authority:target.authority ~headers:head.headers
        ~body:(H1_body.body body) ~conn:ctx.conn head.meth target.path
    in
    match await (Fun.Async.call (fun () -> handler req)) body with
    (* Only the request's own body failing is the client's doing: the same
       exceptions from the handler's other flows, an upstream, are errors. *)
    | exception H1_error.E e when H1_body.failed body ->
        error ~meth:head.meth e
    | exception Body.Truncated when H1_body.failed body ->
        `Close (* the client left *)
    | exception e when not (is_fatal e) ->
        report e ; fail_500 head
    | `Disconnected ->
        `Close
    | `Response response -> (
      match Response.kind response with
      | Response.Upgrade f -> (
        match upgrade_error head response with
        | Some e ->
            report (Failure ("Response.upgrade: " ^ e)) ;
            fail_500 head
        (* A server that stops opens no new upgrade or tunnel (#26), and sends
           no 100 for it. *)
        | None when ctx.is_stopping () ->
            fail_503 head
        (* A body known to be too large to drain gets the 500 with no 100: after
           a 100 the client sends it (RFC 9110 §10.1.1). *)
        | None
          when Option.fold ~none:false
                 ~some:(fun n -> n > config.max_drain)
                 (H1_body.unread body) ->
            fail_500 head
        | None -> (
          match continue_before_switch response body with
          | exception Body.Truncated ->
              `Close (* the client left *)
          (* What follows the request body is the new protocol's. *)
          | () when not (drain body) ->
              fail_500 head
          (* The stop can start during the 100 or the drain. *)
          | () when ctx.is_stopping () ->
              fail_503 head
          | () ->
              (* The writer ignores keep_alive for an upgrade. *)
              ignore
                (write ~committed ~body
                   ~keep_alive:(fun () -> true)
                   head response ) ;
              switch f ) )
      (* RFC 9110 §9.3.6: a 2xx to CONNECT opens a tunnel. *)
      | Response.Body _
        when Method.equal head.meth Method.CONNECT
             && Status.kind (Response.status response) = `Success ->
          report (Failure "a 2xx response to CONNECT must be a Response.upgrade") ;
          fail_500 head
      | Response.Body _ ->
          (* Decided when the head is written, which a streamed response delays
             until its first output: it may read the request body first, and get
             a 100 Continue sent. Whether an unread body can be drained is only
             known once the response is written. *)
          let keep_alive () =
            request_keep_alive head
            && (not (H1_write.wants_close response))
            && (not (H1_body.continue_owed body))
            (* Its framing cannot be trusted any more. *)
            && (not (H1_body.failed body))
            && not (ctx.is_stopping ())
          in
          if write ~committed ~body ~keep_alive head response && drain body then begin
            Fun.Async.yield () ;
            next_request ~idle:config.idle_timeout
          end
          else `Linger )
  (* The handler's response; or [`Disconnected], the exchange cancelled, if the
     client leaves first. The connection is only watched once the request body
     was read: until then the handler may be reading it. Bytes that arrive are a
     pipelined request, kept for later. *)
  and await exchange body =
    let response =
      Action.map (fun r -> `Response r) (Fun.Async.get' exchange)
    in
    let read = Action.map (fun () -> `Read) (H1_body.completed body) in
    (* A wakeup can find nothing to read, as in [wait_for_input]. *)
    let rec watch () =
      match
        Action.invoke (Action.choose [response; Flow.readable' flow `Readable])
      with
      | `Response r ->
          `Response r
      | `Readable -> (
        match Buf_reader.fill_now r with
        | `Bytes ->
            `Response (Fun.Async.get exchange)
        | `Would_block ->
            watch ()
        | `Eof | (exception Flow.Closed) ->
            left () )
    (* Cancelled because the client left, unless we are cancelled too; other
       failures while it unwound are still errors. A response that came anyway
       is sent. *)
    and left () =
      Fun.Async.cancel exchange ;
      match Fun.Async.get exchange with
      | r ->
          `Response r
      | exception Fun.Async.Cancelled
        when not (Fun.Async.is_current_cancelled ()) ->
          `Disconnected
      | exception e when not (is_fatal e) ->
          report e ; `Disconnected
    in
    if not config.cancel_on_disconnect then `Response (Fun.Async.get exchange)
    else
      match Action.invoke (Action.choose [response; read]) with
      | `Response r ->
          `Response r
      | `Read when Buf_reader.buffered r > 0 ->
          `Response (Fun.Async.get exchange)
      | `Read ->
          watch ()
  (* Hands the connection to [f], with the bytes already received first. The new
     protocol sets its own deadlines. *)
  and switch f =
    Flow.set_read_deadline flow None ;
    Flow.set_write_deadline flow None ;
    ( match f (Flow.prepend (Buf_reader.take_buffered r) flow) with
    | () | (exception (Flow.Closed | Flow.Timeout)) ->
        ()
    | exception e when not (is_fatal e) ->
        report e ) ;
    `Close
  (* A 500, and the connection ends. *)
  and fail_500 head =
    ignore (write ~keep_alive:(fun () -> false) head (internal_error ())) ;
    `Linger
  (* No Retry-After: a server that stops does not know when it comes back. *)
  and fail_503 head =
    ignore
      (write
         ~keep_alive:(fun () -> false)
         head
         (Response.string ~status:Status.service_unavailable
            "Service Unavailable\n" ) ) ;
    `Linger
  (* Writes [response]; [true] iff it was written in full and the connection can
     stay open, or switch protocols. The head is written, and [committed] set,
     just before the body's first output; [keep_alive ()] is asked then. Until
     that point a failing body can still be answered properly. *)
  and write ?(committed = ref false) ?body ~keep_alive (head : H1_parse.head)
      response =
    let request_failed () = Option.fold ~none:false ~some:H1_body.failed body in
    let framing =
      H1_write.framing ~meth:head.meth ~version:head.version response
    in
    let kept_alive = ref false in
    let write_head () =
      committed := true ;
      kept_alive := keep_alive () && framing <> H1_write.Close_delimited ;
      H1_write.head w ?date:(date ()) ~version:head.version
        ~keep_alive:!kept_alive framing response
    in
    (* A response cut short after its head. Chunked and Content-Length framings
       show it, but a close-delimited body would look complete after a clean
       close: reset the connection instead. *)
    match Response.kind response with
    | Response.Upgrade _ ->
        write_head () ; Buf_writer.flush w ; true
    | Response.Body b -> (
      match H1_write.body ~before:write_head w framing b with
      | () ->
          !kept_alive
      | exception e -> (
          let bt = Printexc.get_raw_backtrace () in
          (* Cut short after its head, whatever the cause: a failing body, a
             write timeout, a cancellation. *)
          if !committed && framing = H1_write.Close_delimited then
            Flow.reset flow ;
          match e with
          | Flow.Closed | Flow.Timeout | Fun.Async.Cancelled ->
              Printexc.raise_with_backtrace e bt
          (* A request body that the response streams was cut short or is
             malformed: the client's fault, not the server's. *)
          | Body.Truncated when request_failed () ->
              false
          | H1_error.E e when request_failed () ->
              if not !committed then ignore (error ~meth:head.meth e) ;
              false
          | e ->
              config.on_error e bt ;
              if not !committed then ignore (fail_500 head) ;
              false ) )
  in
  try
    match next_request ~idle:config.header_timeout with
    | `Close ->
        ()
    | `Linger ->
        linger config flow
  with Flow.Closed | Flow.Timeout -> ()
