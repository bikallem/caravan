(* The printers of caravan.testing, with no other printer for the exceptions of
   Weft_sim in the program. *)

open Windtrap

(* A reference to the module, thus the linker keeps it and its printers. *)
let () = ignore Caravan_testing.authority

let tests =
  group "Printers"
    [ test "the exceptions of Weft_sim" (fun () ->
          expect
            (String.concat "\n"
               (List.map Printexc.to_string
                  [ Weft_sim.Deadlock ["a"; "b"]
                  ; Weft_sim.Livelock ["a"]
                  ; Weft_sim.Lost_wakeup ["b"]
                  ; Weft_sim.Unawaited ["c"]
                  ; Weft_sim.Model_error "d" ] ) )
          @@ __POS_OF__
               {|Weft_sim.Deadlock: a; b
Weft_sim.Livelock: a
Weft_sim.Lost_wakeup: b
Weft_sim.Unawaited: c
Weft_sim.Model_error: d|} )
    ; test "the names of the signals of Killed" (fun () ->
          let names =
            Weft.Signal.
              [ (SIGHUP, "SIGHUP")
              ; (SIGINT, "SIGINT")
              ; (SIGQUIT, "SIGQUIT")
              ; (SIGILL, "SIGILL")
              ; (SIGABRT, "SIGABRT")
              ; (SIGFPE, "SIGFPE")
              ; (SIGKILL, "SIGKILL")
              ; (SIGSEGV, "SIGSEGV")
              ; (SIGBUS, "SIGBUS")
              ; (SIGPIPE, "SIGPIPE")
              ; (SIGALRM, "SIGALRM")
              ; (SIGTERM, "SIGTERM")
              ; (SIGUSR1, "SIGUSR1")
              ; (SIGUSR2, "SIGUSR2")
              ; (SIGCHLD, "SIGCHLD")
              ; (SIGCONT, "SIGCONT")
              ; (SIGSTOP, "SIGSTOP")
              ; (SIGTSTP, "SIGTSTP")
              ; (SIGWINCH, "SIGWINCH")
              ; (Other_signal 40, "signal 40") ]
          in
          equal (list string)
            (List.map (fun (_, n) -> "Weft_sim.Killed: " ^ n) names)
            (List.map
               (fun (s, _) -> Printexc.to_string (Weft_sim.Killed s))
               names ) ) ]

let () = exit (run "printers" [tests])
