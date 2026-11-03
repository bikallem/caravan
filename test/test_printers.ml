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
    ]

let () = exit (run "printers" [tests])
