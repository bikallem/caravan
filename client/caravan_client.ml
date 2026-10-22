module Config = Config
module Request = Caravan.Request
module Response = Caravan.Response
module Body = Caravan.Body
module Headers = Caravan.Headers
module Method = Caravan.Method

type error = Error.t =
  | Connect of string
  | Timeout of [`Connect | `Response | `Read | `Write]
  | Closed
  | Truncated
  | Bad_response of string
  | Unsupported of string

exception Error = Error.Error

open Weft
module Cancel = Caravan.Private.Cancel

type t = {config: Config.t; writers: Writers.t; pool: Pool.t}

(* Half of [idle_timeout], and 10 ms or more: a short [idle_timeout] does not
   make a busy loop. *)
let reap_interval (config : Config.t) =
  let h = Span.of_ns (Int64.div (Span.to_ns config.idle_timeout) 2L) in
  if Span.compare h (Span.ms 10) < 0 then Span.ms 10 else h

(* The reaper closes the idle connections that are too old. It is a daemon of
   the scope of the writers: [close] cancels it, and the end of the jobs ends
   it. *)
let make ?(config = Config.default) sw =
  let pool = Pool.make config in
  (* A cancel of [sw] is a close too. *)
  let writers = Writers.make ~on_stop:(fun () -> Pool.close pool) sw in
  let half = reap_interval config in
  Fiber.fork_daemon ~name:"caravan.client.reaper" (Writers.scope writers)
    (fun () ->
      while true do
        Time.sleep Mono half ; Pool.reap pool
      done ) ;
  {config; writers; pool}

let close t = Writers.stop t.writers ; Pool.close t.pool

let run ?config f =
  Fiber.scope (fun s ->
      match make ?config s with
      | exception Fiber.Scope_closed ->
          (* The scope [s] is new, so only a cancel of the caller closes it. *)
          Cancel.scope_closed ()
      | t -> (
          (* The request bodies that still go get [close_grace]. A cancel of the
             caller ends the grace at once. *)
          let finish () =
            Writers.finish t.writers ~grace:t.config.close_grace ;
            close t ;
            Writers.wait t.writers
          in
          match f t with
          | v ->
              finish () ; Cancel.check () ; v
          | exception e ->
              let bt = Printexc.get_raw_backtrace () in
              finish () ;
              Printexc.raise_with_backtrace e bt ) )

(* [host[:port]], with an IPv6 host in brackets. An empty port is 80. *)
let host_port authority =
  let invalid () =
    invalid_arg ("Caravan_client: invalid authority " ^ authority)
  in
  let port s =
    if s = "" then 80
    else
      match int_of_string_opt s with
      | Some p
        when p >= 1 && p <= 65535
             && String.for_all (fun c -> c >= '0' && c <= '9') s ->
          p
      | _ ->
          invalid ()
  in
  if String.starts_with ~prefix:"[" authority then
    match String.index_opt authority ']' with
    | None ->
        invalid ()
    | Some i ->
        let host = String.sub authority 0 (i + 1)
        and rest =
          String.sub authority (i + 1) (String.length authority - i - 1)
        in
        if rest = "" then (host, 80)
        else if rest.[0] = ':' then
          (host, port (String.sub rest 1 (String.length rest - 1)))
        else invalid ()
  else
    match String.rindex_opt authority ':' with
    | None ->
        if authority = "" then invalid () else (authority, 80)
    | Some i ->
        let host = String.sub authority 0 i in
        if host = "" then invalid () ;
        ( host
        , port (String.sub authority (i + 1) (String.length authority - i - 1))
        )

(* RFC 9110 §9.2.2. *)
let idempotent m =
  List.exists (Method.equal m) Method.[GET; HEAD; PUT; DELETE; OPTIONS; TRACE]

(* A closed client refuses a request. A cancel of the caller can close the
   client at the same time. For example, the daemon of the writers calls
   [Pool.close] on a different domain, or a different fiber calls [close]. Then
   the caller gets [Fiber.Cancelled], not the refusal. [Cancel.check] is a sync,
   thus it is outside the lock of the pool. *)
let checkout ?reuse t ~host ~port =
  match
    if Writers.is_stopping t.writers then Error.refuse_closed () ;
    Pool.checkout ?reuse t.pool ~host ~port
  with
  | c ->
      c
  | exception (Invalid_argument _ as e) ->
      let bt = Printexc.get_raw_backtrace () in
      Cancel.check () ;
      Printexc.raise_with_backtrace e bt

let request t req =
  (* If [request] raises before the exchange, the body of the request closes
     here. A second close does nothing. *)
  let host, port, c, kind =
    try
      if Request.scheme req <> "http" then
        Error.fail (Unsupported ("the scheme " ^ Request.scheme req)) ;
      let authority =
        match Request.authority req with
        | Some a ->
            a
        | None ->
            invalid_arg "Caravan_client.request: the request has no authority"
      in
      let host, port = host_port authority in
      (* A bad request, and a request during the grace of [run], raise before a
         connect. *)
      Caravan.Private.H1_write.check_request req ;
      let c, kind = checkout t ~host ~port in
      (host, port, c, kind)
    with e ->
      let bt = Printexc.get_raw_backtrace () in
      (try Body.close (Request.body req) with _ -> ()) ;
      Printexc.raise_with_backtrace e bt
  in
  let release c ~reusable = Pool.release t.pool c ~reusable in
  (* A server can close an idle connection while the request goes. One retry on
     a new connection is safe for an idempotent request without a body, when no
     byte of a response came on a reused connection (RFC 9112 §9.3.1). The retry
     gets a new empty body, because the first attempt closed the body of the
     request. *)
  match Exchange.run t.config t.writers c ~on_release:release req with
  | resp ->
      resp
  | exception (Error.Error Closed as e) ->
      let bt = Printexc.get_raw_backtrace () in
      if
        kind = `Reused
        && idempotent (Request.meth req)
        && Body.length (Request.body req) = Some 0
        && not (Writers.is_stopping t.writers)
      then begin
        let c, _ = checkout ~reuse:false t ~host ~port in
        Exchange.run t.config t.writers c ~on_release:release
          (Request.with_body Body.empty req)
      end
      else Printexc.raise_with_backtrace e bt

let body resp =
  match Response.kind resp with
  | Response.Body b ->
      b
  | Response.Upgrade _ ->
      Body.empty

let with_response t req f =
  let resp = request t req in
  match f resp with
  | v ->
      Body.close (body resp) ;
      v
  | exception e ->
      let bt = Printexc.get_raw_backtrace () in
      (try Body.close (body resp) with _ -> ()) ;
      Printexc.raise_with_backtrace e bt

(* http://host[:port][/path][?query][#fragment] *)
let parse_url url =
  let invalid () = invalid_arg ("Caravan_client: invalid URL " ^ url) in
  let i =
    match String.index_opt url ':' with Some i -> i | None -> invalid ()
  in
  let scheme = String.lowercase_ascii (String.sub url 0 i) in
  let rest = String.sub url (i + 1) (String.length url - i - 1) in
  if not (String.starts_with ~prefix:"//" rest) then invalid () ;
  if scheme <> "http" then Error.fail (Unsupported ("the scheme " ^ scheme)) ;
  let rest = String.sub rest 2 (String.length rest - 2) in
  let rest =
    match String.index_opt rest '#' with
    | Some j ->
        String.sub rest 0 j
    | None ->
        rest
  in
  let cut =
    match (String.index_from_opt rest 0 '/', String.index_opt rest '?') with
    | Some a, Some b ->
        Int.min a b
    | Some a, None | None, Some a ->
        a
    | None, None ->
        String.length rest
  in
  let authority = String.sub rest 0 cut
  and path = String.sub rest cut (String.length rest - cut) in
  if authority = "" || String.contains authority '@' then invalid () ;
  let host, port = host_port authority in
  let authority =
    if port = 80 then host else Printf.sprintf "%s:%d" host port
  in
  let path =
    if path = "" then "/" else if path.[0] = '?' then "/" ^ path else path
  in
  (authority, path)

let get ?(headers = Headers.empty) t url =
  let authority, path = parse_url url in
  request t (Request.make ~authority ~headers Method.GET path)

module Private = struct
  module Conn = Conn
  module Writers = Writers
  module Exchange = Exchange
  module Resolver = Resolver
  module Connect = Connect
  module Pool = Pool

  let parse_url = parse_url

  let reap_interval = reap_interval
end
