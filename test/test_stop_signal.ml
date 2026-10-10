(* Stop_signal with signals sent to the test process. It is a suite of its own:
   the fork tests need a process that never started a domain (Unix.fork refuses
   after a domain), and test_net installs other signals. An OCaml handler for
   the two signals makes a signal that the relay does not take harmless: it only
   counts. *)

open Windtrap
open Caravan

let signals = [Sys.sigusr1; Sys.sigusr2]

let handled = Atomic.make 0

let () =
  List.iter
    (fun s ->
      Sys.set_signal s (Sys.Signal_handle (fun _ -> Atomic.incr handled)) )
    signals

(* One domain: with no other domain, the process can fork in a later test. *)
let main1 f = Affect_unix.Unix.main ~domain_count:1 f

(* The signal that [wait] gives within 2 s. *)
let next t = Server_support.within_2s (fun () -> Stop_signal.wait t)

(* Runs [child] in a child process, and gives its exit status. The child ends
   with _exit, so that no at_exit function of the parent runs. A child that does
   not end in 5 s gets SIGKILL. *)
let in_child child =
  Stdlib.flush_all () ;
  match Unix.fork () with
  | 0 ->
      let code = try child () with _ -> 2 in
      Unix._exit code
  | pid ->
      let deadline = Unix.gettimeofday () +. 5. in
      let rec reap () =
        match Unix.waitpid [WNOHANG] pid with
        | 0, _ ->
            if Unix.gettimeofday () > deadline then begin
              Unix.kill pid Sys.sigkill ;
              let rec dead () =
                try snd (Unix.waitpid [] pid)
                with Unix.Unix_error (EINTR, _, _) -> dead ()
              in
              dead ()
            end
            else begin
              Unix.sleepf 0.01 ; reap ()
            end
        | _, status ->
            status
        | exception Unix.Unix_error (EINTR, _, _) ->
            reap ()
      in
      reap ()

(* In a child, install [signals] and wait for a signal that the child sends to
   itself: 0 if wait gives it. *)
let child_waits ?(signals = signals) () =
  let t = Stop_signal.install ~signals () in
  main1
  @@ fun () ->
  Unix.kill (Unix.getpid ()) Sys.sigusr2 ;
  if next t = Sys.sigusr2 then 0 else 1

let fork_tests =
  group "After a fork"
    [ test "install in the child starts a new relay" (fun () ->
          let t = Stop_signal.install ~signals () in
          let status =
            Stdlib.Fun.protect
              ~finally:(fun () -> Stop_signal.release t)
              (fun () -> in_child child_waits)
          in
          is_true ~msg:"the child got its signal" (status = Unix.WEXITED 0) )
    ; test "install in the child after a release in the parent" (fun () ->
          Stop_signal.release (Stop_signal.install ~signals ()) ;
          is_true ~msg:"the child got its signal"
            (in_child child_waits = Unix.WEXITED 0) )
    ; test "install in the child with other signals" (fun () ->
          let t = Stop_signal.install ~signals () in
          let status =
            Stdlib.Fun.protect
              ~finally:(fun () -> Stop_signal.release t)
              (fun () -> in_child (child_waits ~signals:[Sys.sigusr2]))
          in
          is_true ~msg:"the child got its signal" (status = Unix.WEXITED 0) )
    ; (* The child installs only SIGUSR2: SIGUSR1 goes to its handler. *)
      test "in the child, the other signals of the parent get their action"
        (fun () ->
          let t = Stop_signal.install ~signals () in
          let child () =
            ignore (Stop_signal.install ~signals:[Sys.sigusr2] ()) ;
            let before = Atomic.get handled in
            Unix.kill (Unix.getpid ()) Sys.sigusr1 ;
            let deadline = Unix.gettimeofday () +. 2. in
            let rec got () =
              if Atomic.get handled > before then 0
              else if Unix.gettimeofday () > deadline then 1
              else begin
                Unix.sleepf 0.001 ; got ()
              end
            in
            got ()
          in
          let status =
            Stdlib.Fun.protect
              ~finally:(fun () -> Stop_signal.release t)
              (fun () -> in_child child)
          in
          is_true ~msg:"the handler of the child got SIGUSR1"
            (status = Unix.WEXITED 0) )
    ; test "in the child, the value of the parent raises" (fun () ->
          let t = Stop_signal.install ~signals () in
          let child () =
            ignore (Stop_signal.install ~signals ()) ;
            let raises f =
              match f () with
              | _ ->
                  false
              | exception Invalid_argument _ ->
                  true
            in
            if
              raises (fun () -> Stop_signal.release t)
              && raises (fun () -> Stop_signal.wait t)
            then 0
            else 1
          in
          let status =
            Stdlib.Fun.protect
              ~finally:(fun () -> Stop_signal.release t)
              (fun () -> in_child child)
          in
          is_true ~msg:"release and wait raised" (status = Unix.WEXITED 0) ) ]

let () = exit (run "stop-signal" [fork_tests])
