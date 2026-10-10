(* Signals are blocked in the calling thread, and so in the domains and threads
   it creates later. A system thread takes them with sigwait and writes, per
   signal, its index in [signals] to a pipe that affect can wait on. Once
   released, or from the third signal on, the thread hands the signals it takes
   back to the system's default action: the thread, unlike affect, cannot be
   held up by a handler that blocks its domain. *)

type t =
  { signals: int list
  ; read: Unix.file_descr
  ; active: bool Atomic.t
  ; count: int Atomic.t (* signals taken since installed *)
  ; write: Unix.file_descr
  ; pid: int (* the process of the relay thread *) }

let lock = Mutex.create ()

let installed = ref None

let index_of s signals =
  let rec find i = function
    | [] ->
        0
    | x :: rest ->
        if x = s then i else find (i + 1) rest
  in
  find 0 signals

let rec drain fd =
  match Unix.read fd (Bytes.create 64) 0 64 with
  | 0 ->
      ()
  | _ ->
      drain fd
  | exception Unix.Unix_error ((EAGAIN | EWOULDBLOCK | EINTR), _, _) ->
      ()

let start signals =
  ignore (Thread.sigmask Unix.SIG_BLOCK signals) ;
  let read, write = Unix.pipe ~cloexec:true () in
  Unix.set_nonblock read ;
  let active = Atomic.make true and count = Atomic.make 0 in
  let rec relay () =
    let s = Thread.wait_signal signals in
    if Atomic.get active && Atomic.fetch_and_add count 1 < 2 then begin
      ignore
        (Unix.write_substring write
           (String.make 1 (Char.chr (index_of s signals)))
           0 1 ) ;
      relay ()
    end
    else begin
      ignore (Thread.sigmask Unix.SIG_UNBLOCK [s]) ;
      Unix.kill (Unix.getpid ()) s ;
      ignore (Thread.sigmask Unix.SIG_BLOCK [s]) ;
      relay ()
    end
  in
  ignore (Thread.create relay ()) ;
  {signals; read; write; active; count; pid= Unix.getpid ()}

let install ?(signals = [Sys.sigint; Sys.sigterm]) () =
  Mutex.protect lock
  @@ fun () ->
  match !installed with
  (* A fork keeps only the thread that forks: the relay of [t] is not in this
     process, and its pipe is also the pipe of the parent. The signals of [t]
     that the child does not install get their action again. *)
  | Some t when t.pid <> Unix.getpid () ->
      let close fd = try Unix.close fd with Unix.Unix_error _ -> () in
      close t.read ;
      close t.write ;
      installed := None ;
      ignore
        (Thread.sigmask Unix.SIG_UNBLOCK
           (List.filter (fun s -> not (List.mem s signals)) t.signals) ) ;
      let t = start signals in
      installed := Some t ;
      t
  | Some t when t.signals <> signals ->
      invalid_arg "Stop_signal.install: already installed with other signals"
  | Some t ->
      if not (Atomic.get t.active) then begin
        ignore (Thread.sigmask Unix.SIG_BLOCK signals) ;
        drain t.read ;
        Atomic.set t.count 0 ;
        Atomic.set t.active true
      end ;
      t
  | None ->
      let t = start signals in
      installed := Some t ;
      t

(* The value of the parent is not valid in a child after a fork: its pipe is
   also the pipe of the parent. *)
let check t name =
  if t.pid <> Unix.getpid () then
    invalid_arg ("Stop_signal." ^ name ^ ": a value of another process")

let release t =
  check t "release" ;
  Mutex.protect lock
  @@ fun () ->
  if Atomic.exchange t.active false then begin
    drain t.read ;
    ignore (Thread.sigmask Unix.SIG_UNBLOCK t.signals)
  end

let wait t =
  check t "wait" ;
  let rec loop () =
    Affect.Action.invoke (Affect_unix.Unix.wait_readable t.read ()) ;
    let b = Bytes.create 1 in
    match Unix.read t.read b 0 1 with
    | _ ->
        List.nth t.signals (Char.code (Bytes.get b 0))
    | exception Unix.Unix_error ((EAGAIN | EWOULDBLOCK | EINTR), _, _) ->
        loop ()
  in
  loop ()
