open Affect

exception Timeout

exception Closed

type t =
  { time: Time.mono
  ; read: bytes -> int -> int -> int
  ; read_now: bytes -> int -> int -> int option
  ; write: bytes -> int -> int -> unit
  ; readable: unit Action.t
  ; shutdown_send: unit -> unit
  ; close: unit -> unit
  ; reset: unit -> unit
  ; set_read_deadline: Affect_unix.Mtime.t option -> unit
  ; set_write_deadline: Affect_unix.Mtime.t option -> unit
  ; closed: bool Atomic.t }

let make ~time ~read ~read_now ~write ~readable ~shutdown_send ~close
    ?(reset = close) ~set_read_deadline ~set_write_deadline () =
  { time
  ; read
  ; read_now
  ; write
  ; readable
  ; shutdown_send
  ; close
  ; reset
  ; set_read_deadline
  ; set_write_deadline
  ; closed= Atomic.make false }

let time flow = flow.time

let check_range fn buf off len =
  if off < 0 || len < 0 || off > Bytes.length buf - len then
    invalid_arg (Printf.sprintf "Flow.%s: invalid range" fn)

let check_read fn flow buf off len =
  check_range fn buf off len ;
  if len = 0 then invalid_arg (Printf.sprintf "Flow.%s: empty read" fn) ;
  if Atomic.get flow.closed then raise Closed

let read flow buf off len =
  check_read "read" flow buf off len ;
  flow.read buf off len

let read_now flow buf off len =
  check_read "read_now" flow buf off len ;
  flow.read_now buf off len

let readable' flow tag = Action.map (fun () -> tag) flow.readable

let write flow buf off len =
  check_range "write" buf off len ;
  if Atomic.get flow.closed then raise Closed ;
  if len > 0 then flow.write buf off len

let write_string flow s =
  write flow (Bytes.unsafe_of_string s) 0 (String.length s)

let shutdown_send flow =
  if not (Atomic.get flow.closed) then flow.shutdown_send ()

let set_read_deadline flow d = flow.set_read_deadline d

let set_write_deadline flow d = flow.set_write_deadline d

let close flow = if not (Atomic.exchange flow.closed true) then flow.close ()

let reset flow = if not (Atomic.exchange flow.closed true) then flow.reset ()

let linger flow ~max =
  let buf = Bytes.create 4096 in
  let rec discard left =
    if left > 0 then
      match read flow buf 0 (Int.min left (Bytes.length buf)) with
      | 0 ->
          ()
      | n ->
          discard (left - n)
  in
  (* A different fiber can close [flow] at any time. *)
  try shutdown_send flow ; discard max with Closed | Timeout -> ()

let prepend s flow =
  if s = "" then flow
  else begin
    let pos = ref 0 in
    let remaining () = String.length s - !pos in
    let take buf off len =
      let n = Int.min len (remaining ()) in
      Bytes.blit_string s !pos buf off n ;
      pos := !pos + n ;
      n
    in
    let read buf off len =
      if remaining () = 0 then read flow buf off len else take buf off len
    in
    let read_now buf off len =
      if remaining () = 0 then read_now flow buf off len
      else Some (take buf off len)
    in
    let readable =
      Action.guard (fun () ->
          if remaining () > 0 then Action.always () else flow.readable )
    in
    make ~time:flow.time ~read ~read_now ~write:(write flow) ~readable
      ~shutdown_send:(fun () -> shutdown_send flow)
      ~close:(fun () -> close flow)
      ~reset:(fun () -> reset flow)
      ~set_read_deadline:(set_read_deadline flow)
      ~set_write_deadline:(set_write_deadline flow) ()
  end

let of_string ?(chunk = max_int) input output =
  if chunk <= 0 then invalid_arg "Flow.of_string: chunk must be positive" ;
  let pos = ref 0 in
  let read buf off len =
    let n = Int.min (Int.min len chunk) (String.length input - !pos) in
    Bytes.blit_string input !pos buf off n ;
    pos := !pos + n ;
    n
  in
  let write buf off len = Buffer.add_subbytes output buf off len in
  let ignore_deadline (_ : Affect_unix.Mtime.t option) = () in
  make ~time:Time.mono ~read
    ~read_now:(fun buf off len -> Some (read buf off len))
    ~write ~readable:(Action.always ()) ~shutdown_send:ignore ~close:ignore
    ~set_read_deadline:ignore_deadline ~set_write_deadline:ignore_deadline ()
