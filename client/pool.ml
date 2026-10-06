module Cfg = Config
module Mtime = Affect_unix.Mtime
open Caravan

(* The idle connections of each (host, port), the newest first, and the
   connections in use. A Mutex protects them, and no critical section
   suspends the fiber: the connect and the checks of an idle connection
   happen outside the lock. *)
type t = {
  config : Cfg.t;
  now : unit -> Mtime.t;
  connect : host:string -> port:int -> Flow.t;
  lock : Mutex.t;
  idle : (string * int, Conn.t list) Hashtbl.t;
  in_use : (int, Conn.t) Hashtbl.t;
  mutable closed : bool;
}

let make ?(now = Mtime.now) ?connect config =
  let connect =
    match connect with Some c -> c | None -> fun ~host ~port -> Connect.connect config ~host ~port
  in
  { config; now; connect; lock = Mutex.create (); idle = Hashtbl.create 8; in_use = Hashtbl.create 8;
    closed = false }

let refuse () = invalid_arg "Caravan_client: the client is closed"

(* The newest idle connection of [key], now in use. *)
let take t key =
  Mutex.protect t.lock (fun () ->
      if t.closed then refuse ();
      match Hashtbl.find_opt t.idle key with
      | Some (c :: rest) ->
          if rest = [] then Hashtbl.remove t.idle key else Hashtbl.replace t.idle key rest;
          Hashtbl.replace t.in_use c.Conn.id c;
          Some c
      | _ -> None)

(* An idle connection is stale when it is too old, or when its server
   closed it, reset it, or sent bytes that no request asked for. *)
let too_old t (c : Conn.t) =
  match Mtime.add_span c.released_at t.config.idle_timeout with
  | Some d -> not (Mtime.is_earlier (t.now ()) ~than:d)
  | None -> false

let stale t c =
  too_old t c
  || match Flow.read_now c.flow (Bytes.create 1) 0 1 with
     | None -> false
     | Some _ -> true
     | exception Flow.Closed -> true

let discard t (c : Conn.t) =
  Mutex.protect t.lock (fun () -> Hashtbl.remove t.in_use c.id);
  Flow.close c.flow

let is_closed t = Mutex.protect t.lock (fun () -> t.closed)

let rec checkout ?(reuse = true) t ~host ~port =
  let key = (String.lowercase_ascii host, port) in
  match if reuse then take t key else if is_closed t then refuse () else None with
  | Some c when stale t c -> discard t c; checkout ~reuse t ~host ~port
  | Some c -> (c, `Reused)
  | None ->
      let c = Conn.make ~host:(fst key) ~port (t.connect ~host ~port) in
      let added =
        Mutex.protect t.lock (fun () ->
            if t.closed then false else (Hashtbl.replace t.in_use c.id c; true))
      in
      if added then (c, `New) else (Flow.close c.flow; refuse ())

let release t (c : Conn.t) ~reusable =
  let kept =
    Mutex.protect t.lock (fun () ->
        Hashtbl.remove t.in_use c.id;
        let key = (c.host, c.port) in
        let idle = Option.value ~default:[] (Hashtbl.find_opt t.idle key) in
        if reusable && (not t.closed) && List.length idle < t.config.max_idle_per_host
        then begin
          c.released_at <- t.now ();
          Hashtbl.replace t.idle key (c :: idle);
          true
        end
        else false)
  in
  if not kept then Flow.close c.flow

(* Closes the idle connections that are older than [idle_timeout]. Thus
   a host that the client does not use again keeps no socket open. *)
let reap t =
  let old = ref [] in
  Mutex.protect t.lock (fun () ->
      Hashtbl.filter_map_inplace
        (fun _ cs ->
           let o, keep = List.partition (too_old t) cs in
           old := o @ !old;
           if keep = [] then None else Some keep)
        t.idle);
  List.iter (fun (c : Conn.t) -> Flow.close c.flow) !old

let idle_hosts t = Mutex.protect t.lock (fun () -> Hashtbl.length t.idle)

(* The connections in use close too: their exchanges then fail. *)
let close t =
  let all =
    Mutex.protect t.lock (fun () ->
        t.closed <- true;
        let all =
          Hashtbl.fold (fun _ cs acc -> cs @ acc) t.idle (Hashtbl.fold (fun _ c acc -> c :: acc) t.in_use [])
        in
        Hashtbl.reset t.idle;
        Hashtbl.reset t.in_use;
        all)
  in
  List.iter (fun (c : Conn.t) -> Flow.close c.flow) all
