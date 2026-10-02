(* Signals are blocked in the calling thread, and so in the domains and
   threads it creates later. A system thread takes them with sigwait and
   writes a byte per signal to a pipe that affect can wait on. *)

type t = { signals : int list; read : Unix.file_descr }

let lock = Mutex.create ()
let installed = ref None

let install ?(signals = [ Sys.sigint; Sys.sigterm ]) () =
  Mutex.protect lock @@ fun () ->
  match !installed with
  | Some t when t.signals = signals -> t
  | Some _ -> invalid_arg "Stop_signal.install: already installed with other signals"
  | None ->
      ignore (Thread.sigmask Unix.SIG_BLOCK signals);
      let read, write = Unix.pipe ~cloexec:true () in
      Unix.set_nonblock read;
      let rec relay () =
        ignore (Thread.wait_signal signals);
        ignore (Unix.write_substring write "s" 0 1);
        relay ()
      in
      ignore (Thread.create relay ());
      let t = { signals; read } in
      installed := Some t;
      t

let rec wait t =
  Affect.Action.invoke (Affect_unix.Unix.wait_readable t.read ());
  match Unix.read t.read (Bytes.create 1) 0 1 with
  | _ -> ()
  | exception Unix.Unix_error ((EAGAIN | EWOULDBLOCK | EINTR), _, _) -> wait t
