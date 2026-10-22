module Cfg = Config
open Weft
open Caravan
open Caravan.Private

(* An error of the request writer that stops the reader. *)
exception Writer_failed of exn * Printexc.raw_backtrace

(* A write error of this connection, in the writer. A producer can raise
   Flow.Closed of a different flow: only this wrapper means the network. *)
exception Net of exn

(* Which ends of the writer stop a read that waits: an exception of the user and
   a wrong length always, a write timeout only before the response head. After a
   cancel or a write error of the network, the reader reads what the server sent
   (RFC 9112 §9.6). *)
let stops phase = function
  | Net Flow.Timeout ->
      phase = `Head
  | Net _ | Writers.Job_cancelled ->
      false
  | _ ->
      true

let writer_error (e, bt) =
  match e with
  | Net Flow.Timeout ->
      Error.fail (Timeout `Write)
  | H1_write.Length_mismatch ->
      invalid_arg
        "Caravan_client: the request body length differs from its \
         Content-Length"
  | Fiber.Cancelled _ ->
      (* The cancel of a different scope: the caller is not cancelled. *)
      failwith "Caravan_client: the request body producer raised Cancelled"
  | e ->
      Printexc.raise_with_backtrace e bt

(* Runs [f], then closes [body]. An exception of [f] has priority over an
   exception of the close. Unlike Stdlib.Fun.protect, no exception gets the
   Finally_raised wrapper: a Cancelled of the close stays Cancelled, and the
   caller gets the exception of the user. *)
let closing body f =
  match f () with
  | v ->
      Body.close body ; v
  | exception e ->
      let bt = Printexc.get_raw_backtrace () in
      (try Body.close body with _ -> ()) ;
      Printexc.raise_with_backtrace e bt

let closes (h : H1_parse.response_head) =
  h.version = Version.Http_1_0
  || Headers.has_token "connection" "close" h.headers

let body_error e bt =
  match e with
  | Flow.Timeout ->
      Error.fail (Timeout `Read)
  | Body.Truncated | Flow.Closed ->
      Error.fail Truncated
  | H1_error.E e ->
      Error.fail (Bad_response (Format.asprintf "%a" H1_error.pp e))
  | Writer_failed (e, bt) ->
      writer_error (e, bt)
  | e ->
      Printexc.raise_with_backtrace e bt

let run (config : Cfg.t) writers (conn : Conn.t) ~on_release req =
  let flow = conn.flow in
  let framing = H1_write.request_framing req in
  let inner = Request.body req in
  (* A bad request raises in the caller, and the connection did not change. For
     a request with no body on the wire, the close of the body is its only user
     code: it runs here, in the fiber of the caller. *)
  ( match
      H1_write.check_request req ;
      if framing = H1_write.No_body then Body.close inner
    with
  | () ->
      ()
  | exception e -> (
      let bt = Printexc.get_raw_backtrace () in
      on_release conn ~reusable:true ;
      match e with
      | Fiber.Cancelled _ when not (Cancel.is_own e) ->
          failwith
            "Caravan_client: the close of the request body raised Cancelled"
      | e ->
          Printexc.raise_with_backtrace e bt ) ) ;
  let sent = Ivar.create () in
  (* [produced] is set when the user code of the body ends: the producer of a
     writer body, or the reads and the close of a source. After that, the job
     only writes to the network, with [write_timeout]. A body of length 0 with
     [Content-Length: 0] still runs its producer, or one read. *)
  let produced = Atomic.make (framing = H1_write.No_body) in
  let job =
    Writers.job (fun () ->
        (* One timeout for each write that waits, set one time for the job. *)
        Flow.set_write_timeout flow (Some config.Cfg.write_timeout) ;
        let w =
          Buf_writer.make ~buffer:conn.write_buf (fun buf off len ->
              try Flow.write flow buf off len
              with (Flow.Closed | Flow.Timeout | Net.Error _) as e ->
                raise (Net e) )
        in
        (* The body closes in all cases, also when a write of the head fails. A
           second close does nothing. *)
        closing inner (fun () ->
            H1_write.request_head w framing req ;
            let body =
              if framing = H1_write.No_body then inner
              else begin
                (* The head goes before the user code of the body runs: a
                   producer can wait for the response before its first write. *)
                Buf_writer.flush w ;
                (* The close of a source is user code too, thus it runs before
                   the flag. *)
                Body.of_writer ?length:(Body.length inner) (fun bw ->
                    closing inner (fun () -> Body.write_to inner bw) ;
                    Atomic.set produced true )
              end
            in
            H1_write.body w framing body ) ;
        ignore (Ivar.try_fill sent (Time.now Mono)) )
  in
  (* One read function for the exchange: only the reader waits for input, and it
     also sees the end of the request and the end of the writer. *)
  let phase = ref `Head and sent_at = ref None and writer_seen = ref false in
  (* The read deadline of a call starts at its first wait: the clock is read
     only on the wait path, one time for each call. A wake by [`Sent], [`Writer]
     or stale readiness keeps it. *)
  let read buf off len =
    let start = ref None in
    let rec loop () =
      Cancel.check () ;
      match Flow.read_now flow buf off len with
      | Some n ->
          n
      | None -> (
          let deadline =
            match !phase with
            | `Head ->
                Option.bind !sent_at (fun t ->
                    Time.add Mono t config.response_timeout )
            | `Body ->
                let t =
                  match !start with
                  | Some t ->
                      t
                  | None ->
                      let t = Time.now Mono in
                      start := Some t ;
                      t
                in
                Time.add Mono t config.read_timeout
          in
          let events =
            [ wrap (Flow.readable flow) (fun () -> `Readable)
            ; wrap (Deadline.at deadline) (fun () -> `Timeout) ]
          in
          let events =
            if !sent_at = None then
              wrap (Ivar.read sent) (fun t -> `Sent t) :: events
            else events
          in
          let events =
            if !writer_seen then events
            else wrap (Writers.result job) (fun r -> `Writer r) :: events
          in
          match sync (choose events) with
          | `Readable ->
              loop ()
          | `Timeout ->
              raise Flow.Timeout
          | `Sent t ->
              sent_at := Some t ;
              loop ()
          | `Writer r -> (
              writer_seen := true ;
              (* A writer that ends before [sent], for example with a write
                 error, starts the response deadline too. *)
              if !sent_at = None then sent_at := Some (Time.now Mono) ;
              match r with
              | Error (e, bt) when stops !phase e ->
                  raise (Writer_failed (e, bt))
              | Ok () | Error _ ->
                  loop () ) )
    in
    loop ()
  in
  let r =
    Buf_reader.make ~buffer:conn.read_buf ~read_now:(Flow.read_now flow) read
  in
  let response = ref None and response_framing = ref H1_parse.Until_close in
  let reusable () =
    match !response with
    | None ->
        false
    | Some (h : H1_parse.response_head) ->
        (* [closes] is true for HTTP/1.0 (D6). *)
        (not (closes h))
        && (not (H1_write.request_closes req))
        && !response_framing <> H1_parse.Until_close
        && Buf_reader.buffered r = 0
  in
  (* The release runs one time. It gives the error of the user, if the writer
     ended with one: - If the connection can stay, the release waits for the
     writer only when the user code of the body ended. Else the writer gives the
     connection back at its end, in its own fiber: the producer is user code,
     and the caller must not wait for it. - If the connection cannot stay, the
     writer stops first. *)
  let released = Atomic.make false in
  let release ~ended =
    if Writers.is_self job then begin
      (* A wait for its own end never ends. *)
      if not (Atomic.exchange released true) then
        on_release conn ~reusable:false ;
      invalid_arg
        "Caravan_client: a request body producer used its own response"
    end ;
    if Atomic.exchange released true then None
    else if not (Writers.is_started job) then (
      on_release conn ~reusable:false ;
      None )
    else begin
      let keep = ended && reusable () in
      let ok = function Ok () -> true | Error _ -> false in
      let user_error = function
        | Some (Error ((e, _) as err)) when stops `Body e ->
            Some err
        | _ ->
            None
      in
      if keep && not (Atomic.get produced) then
        user_error
          (Writers.when_done job (fun r -> on_release conn ~reusable:(ok r)))
      else begin
        (* After [produced], only network writes remain, each with
           [write_timeout]. A cancel of the caller stops this wait too: then the
           writer stops, and the connection closes. *)
        let r =
          if not keep then Writers.await ~cancel:true job
          else
            match sync (Writers.result job) with
            | r ->
                r
            | exception (Fiber.Cancelled _ as e) ->
                let bt = Printexc.get_raw_backtrace () in
                ignore (Writers.await ~cancel:true job) ;
                on_release conn ~reusable:false ;
                Printexc.raise_with_backtrace e bt
        in
        on_release conn ~reusable:(keep && ok r) ;
        user_error (Some r)
      end
    end
  in
  ( try Writers.start writers job
    with e ->
      let bt = Printexc.get_raw_backtrace () in
      ignore (release ~ended:false) ;
      (* The job never runs, thus the body closes here. *)
      ( try Body.close inner with _ -> () ) ;
      Printexc.raise_with_backtrace e bt ) ;
  let limits = Cfg.limits config in
  (* RFC 9110 §15.2: interim responses other than 101 are skipped. *)
  let rec final n =
    let h = H1_parse.response_head limits r in
    let code = Status.code h.status in
    if code >= 200 || code = 101 then h
    else if n = 10 then
      raise (H1_error.E (Bad_request "more than 10 interim responses"))
    else final (n + 1)
  in
  match
    let h = final 0 in
    (h, H1_parse.response_framing ~meth:(Request.meth req) h)
  with
  | exception e -> (
      let bt = Printexc.get_raw_backtrace () in
      ignore (release ~ended:false) ;
      match e with
      | Buf_reader.Eof | Flow.Closed ->
          if Buf_reader.consumed r = 0 && Buf_reader.buffered r = 0 then
            Error.fail Closed
          else
            Error.fail (Bad_response "the connection ended in a response head")
      | Flow.Timeout ->
          Error.fail (Timeout `Response)
      | H1_error.E (Not_implemented s) ->
          Error.fail (Unsupported s)
      | H1_error.E e ->
          Error.fail (Bad_response (Format.asprintf "%a" H1_error.pp e))
      | Writer_failed (e, bt) ->
          writer_error (e, bt)
      | e ->
          Printexc.raise_with_backtrace e bt )
  | _, Tunnel ->
      ignore (release ~ended:false) ;
      Error.fail (Unsupported "protocol switch")
  | h, fr ->
      response := Some h ;
      response_framing := fr ;
      phase := `Body ;
      (* RFC 9112 §9.6: the server closes, thus the rest of the request is not
         necessary. The reader still reads the response. *)
      if closes h then
        (* The job started at this time. A cancel of a fiber that ended has no
           effect, and the cancel does no sync. *)
        Writers.cancel job ;
      let inner =
        match fr with
        | No_body ->
            Body.empty
        | Fixed n ->
            H1_body.fixed r n
        | Chunked ->
            H1_body.chunked ~limits r
        | Until_close | Tunnel ->
            H1_body.until_close r
      in
      (* An error of the user in the writer comes out at the end of the body, or
         at Body.close. *)
      let pending = ref None in
      let finish ~ended =
        match release ~ended with Some err -> pending := Some err | None -> ()
      in
      let raise_pending () =
        match !pending with
        | Some err ->
            pending := None ;
            writer_error err
        | None ->
            ()
      in
      let read buf off len =
        match Body.read inner buf off len with
        | 0 ->
            finish ~ended:true ; raise_pending () ; 0
        | n ->
            n
        | exception e ->
            let bt = Printexc.get_raw_backtrace () in
            finish ~ended:false ;
            (match e with Writer_failed _ -> pending := None | _ -> ()) ;
            body_error e bt
      in
      (* After the release, [finish] does nothing. *)
      let close () = finish ~ended:false ; raise_pending () in
      if fr = No_body || fr = Fixed 0 then finish ~ended:true ;
      let body =
        Body.of_source ?length:(Body.length inner)
          ~trailers:(fun () -> Body.trailers inner)
          ~close read
      in
      Response.make ~status:h.status ~headers:h.headers body
