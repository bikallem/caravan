module Cfg = Config
open Affect
open Caravan
open Caravan.Private
module Mtime = Affect_unix.Mtime

(* An error of the request writer that stops the reader. *)
exception Writer_failed of exn * Printexc.raw_backtrace

(* Which ends of the writer stop a read that waits: an exception of the
   user and a wrong length always, a write timeout only before the
   response head. After a cancel or a write error of the network, the
   reader reads what the server sent (RFC 9112 §9.6). *)
let stops phase = function
| Flow.Closed | Fun.Async.Cancelled -> false
| Flow.Timeout -> phase = `Head
| _ -> true

let writer_error (e, bt) =
  match e with
  | Flow.Timeout -> Error.fail (Timeout `Write)
  | H1_write.Length_mismatch ->
      invalid_arg "Caravan_client: the request body length differs from its Content-Length"
  | e -> Printexc.raise_with_backtrace e bt

let at = function
| None -> Action.never
| Some d ->
    let now = Mtime.now () in
    if Mtime.is_earlier now ~than:d then Mtime.wait_for' (Mtime.span now d) `Timeout
    else Action.always `Timeout

let closes (h : H1_parse.response_head) =
  h.version = Version.Http_1_0 || Headers.has_token "connection" "close" h.headers

let body_error e bt =
  match e with
  | Flow.Timeout -> Error.fail (Timeout `Read)
  | Body.Truncated | Flow.Closed -> Error.fail Truncated
  | H1_error.E e -> Error.fail (Bad_response (Format.asprintf "%a" H1_error.pp e))
  | Writer_failed (e, bt) -> writer_error (e, bt)
  | e -> Printexc.raise_with_backtrace e bt

let run (config : Cfg.t) writers (conn : Conn.t) ~on_release req =
  let flow = conn.flow in
  let framing = H1_write.request_framing req in
  (* The caller makes the head, thus a bad request raises here, and the
     connection did not change. *)
  let head =
    let b = Buffer.create 256 in
    let w = Buf_writer.make (fun buf off len -> Buffer.add_subbytes b buf off len) in
    match H1_write.request_head w framing req; Buf_writer.flush w with
    | () -> Buffer.contents b
    | exception e -> on_release conn ~reusable:true; raise e
  in
  let sent = Cell.Once.make () in
  let job =
    Writers.job (fun () ->
        let w =
          Buf_writer.make (fun buf off len ->
              Flow.set_write_deadline flow (Mtime.add_span (Mtime.now ()) config.Cfg.write_timeout);
              Flow.write flow buf off len)
        in
        Buf_writer.write_string w head;
        H1_write.body w framing (Request.body req);
        Cell.Once.try_set sent (Mtime.now ()))
  in
  (* One read function for the exchange: only the reader waits for input,
     and it also sees the end of the request and the end of the writer. *)
  let phase = ref `Head and sent_at = ref None and writer_seen = ref false in
  let read buf off len =
    let start = Mtime.now () in
    let rec loop () =
      Fun.Async.check_cancellation ();
      match Flow.read_now flow buf off len with
      | Some n -> n
      | None -> (
          let deadline =
            match !phase with
            | `Head -> Option.bind !sent_at (fun t -> Mtime.add_span t config.response_timeout)
            | `Body -> Mtime.add_span start config.read_timeout
          in
          let events = [ Flow.readable' flow `Readable; at deadline ] in
          let events =
            if !sent_at = None then Action.map (fun t -> `Sent t) (Cell.Once.get' sent) :: events
            else events
          in
          let events =
            if !writer_seen then events
            else Action.map (fun r -> `Writer r) (Cell.Once.get' job.result) :: events
          in
          match Action.invoke (Action.choose events) with
          | `Readable -> loop ()
          | `Timeout -> raise Flow.Timeout
          | `Sent t -> sent_at := Some t; loop ()
          | `Writer (Ok ()) -> writer_seen := true; loop ()
          | `Writer (Error (e, bt)) ->
              writer_seen := true;
              if stops !phase e then raise (Writer_failed (e, bt)) else loop ())
    in
    loop ()
  in
  let r = Buf_reader.make ~read_now:(Flow.read_now flow) read in
  let response = ref None and response_framing = ref H1_parse.Until_close in
  let reusable () =
    match !response with
    | None -> false
    | Some (h : H1_parse.response_head) ->
        (* [closes] is true for HTTP/1.0 (D6). *)
        (not (closes h))
        && (not (H1_write.request_closes req))
        && !response_framing <> H1_parse.Until_close
        && Buf_reader.buffered r = 0
  in
  (* The release runs one time. At a correct end of the body it waits for
     the writer: a server that keeps the connection reads the rest of the
     request. Else it cancels the writer first. It gives the error of the
     user, if the writer ended with one. *)
  let released = Atomic.make false in
  let release ~ended =
    if Writers.is_self job
    then invalid_arg "Caravan_client: a request body producer used its own response";
    if Atomic.exchange released true
    then None
    else begin
      let result =
        if Atomic.get job.taken
        then Some (Writers.await ~cancel:((not ended) || not (reusable ())) job)
        else None
      in
      let writer_ok = match result with Some (Ok ()) -> true | _ -> false in
      on_release conn ~reusable:(ended && writer_ok && reusable ());
      match result with
      | Some (Error ((e, _) as err)) when stops `Body e -> Some err
      | _ -> None
    end
  in
  (try Writers.start writers job
   with e ->
     let bt = Printexc.get_raw_backtrace () in
     ignore (release ~ended:false);
     Printexc.raise_with_backtrace e bt);
  let limits = Cfg.limits config in
  (* RFC 9110 §15.2: interim responses other than 101 are skipped. *)
  let rec final n =
    let h = H1_parse.response_head limits r in
    let code = Status.code h.status in
    if code >= 200 || code = 101 then h
    else if n = 10 then raise (H1_error.E (Bad_request "more than 10 interim responses"))
    else final (n + 1)
  in
  match
    let h = final 0 in
    (h, H1_parse.response_framing ~meth:(Request.meth req) h)
  with
  | exception e -> (
      let bt = Printexc.get_raw_backtrace () in
      ignore (release ~ended:false);
      match e with
      | Buf_reader.Eof | Flow.Closed ->
          if Buf_reader.consumed r = 0 && Buf_reader.buffered r = 0 then Error.fail Closed
          else Error.fail (Bad_response "the connection ended in a response head")
      | Flow.Timeout -> Error.fail (Timeout `Response)
      | H1_error.E (Not_implemented s) -> Error.fail (Unsupported s)
      | H1_error.E e -> Error.fail (Bad_response (Format.asprintf "%a" H1_error.pp e))
      | Writer_failed (e, bt) -> writer_error (e, bt)
      | e -> Printexc.raise_with_backtrace e bt)
  | _, Tunnel ->
      ignore (release ~ended:false);
      Error.fail (Unsupported "protocol switch")
  | h, fr ->
      response := Some h;
      response_framing := fr;
      phase := `Body;
      (* RFC 9112 §9.6: the server closes, thus the rest of the request
         is not necessary. The reader still reads the response. *)
      if closes h
      then
        (* The job is taken at this time. A cancel of a call that ended
           has no effect. *)
        Fun.Async.mask_cancellation (fun () -> Fun.Async.cancel (Cell.Once.get job.handle));
      let inner =
        match fr with
        | No_body -> Body.empty
        | Fixed n -> H1_body.fixed r n
        | Chunked -> H1_body.chunked ~limits r
        | Until_close | Tunnel -> H1_body.until_close r
      in
      (* An error of the user in the writer comes out at the end of the
         body, or at Body.close. *)
      let pending = ref None in
      let finish ~ended = match release ~ended with Some err -> pending := Some err | None -> () in
      let raise_pending () =
        match !pending with
        | Some err -> pending := None; writer_error err
        | None -> ()
      in
      let read buf off len =
        match Body.read inner buf off len with
        | 0 -> finish ~ended:true; raise_pending (); 0
        | n -> n
        | exception e ->
            let bt = Printexc.get_raw_backtrace () in
            finish ~ended:false;
            (match e with Writer_failed _ -> pending := None | _ -> ());
            body_error e bt
      in
      (* After the release, [finish] does nothing. *)
      let close () = finish ~ended:false; raise_pending () in
      if fr = No_body || fr = Fixed 0 then finish ~ended:true;
      let body =
        Body.of_source ?length:(Body.length inner) ~trailers:(fun () -> Body.trailers inner) ~close read
      in
      Response.make ~status:h.status ~headers:h.headers body
