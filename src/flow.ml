open Affect

exception Timeout
exception Closed

type t = {
  read : bytes -> int -> int -> int;
  write : bytes -> int -> int -> unit;
  readable : unit Action.t;
  shutdown_send : unit -> unit;
  close : unit -> unit;
  set_read_deadline : Affect_unix.Mtime.t option -> unit;
  set_write_deadline : Affect_unix.Mtime.t option -> unit;
  closed : bool Atomic.t;
}

let make ~read ~write ~readable ~shutdown_send ~close ~set_read_deadline
    ~set_write_deadline =
  { read; write; readable; shutdown_send; close; set_read_deadline;
    set_write_deadline; closed = Atomic.make false }

let check_range fn buf off len =
  if off < 0 || len < 0 || off > Bytes.length buf - len
  then invalid_arg (Printf.sprintf "Flow.%s: invalid range" fn)

let read flow buf off len =
  check_range "read" buf off len;
  if len = 0 then invalid_arg "Flow.read: empty read";
  if Atomic.get flow.closed then raise Closed;
  flow.read buf off len

let readable' flow tag = Action.map (fun () -> tag) flow.readable

let write flow buf off len =
  check_range "write" buf off len;
  if Atomic.get flow.closed then raise Closed;
  if len > 0 then flow.write buf off len

let write_string flow s =
  write flow (Bytes.unsafe_of_string s) 0 (String.length s)

let shutdown_send flow = if not (Atomic.get flow.closed) then flow.shutdown_send ()
let set_read_deadline flow d = flow.set_read_deadline d
let set_write_deadline flow d = flow.set_write_deadline d
let close flow = if not (Atomic.exchange flow.closed true) then flow.close ()

let prepend s flow =
  if s = ""
  then flow
  else begin
    let pos = ref 0 in
    let remaining () = String.length s - !pos in
    let read buf off len =
      if remaining () = 0
      then read flow buf off len
      else begin
        let n = Int.min len (remaining ()) in
        Bytes.blit_string s !pos buf off n;
        pos := !pos + n;
        n
      end
    in
    let readable =
      Action.guard (fun () ->
          if remaining () > 0 then Action.always () else flow.readable)
    in
    make ~read ~write:(write flow) ~readable
      ~shutdown_send:(fun () -> shutdown_send flow)
      ~close:(fun () -> close flow)
      ~set_read_deadline:(set_read_deadline flow)
      ~set_write_deadline:(set_write_deadline flow)
  end

let of_string ?(chunk = max_int) input output =
  if chunk <= 0 then invalid_arg "Flow.of_string: chunk must be positive";
  let pos = ref 0 in
  let read buf off len =
    let n = Int.min (Int.min len chunk) (String.length input - !pos) in
    Bytes.blit_string input !pos buf off n;
    pos := !pos + n;
    n
  in
  let write buf off len = Buffer.add_subbytes output buf off len in
  let ignore_deadline (_ : Affect_unix.Mtime.t option) = () in
  make ~read ~write ~readable:(Action.always ()) ~shutdown_send:ignore
    ~close:ignore ~set_read_deadline:ignore_deadline
    ~set_write_deadline:ignore_deadline
