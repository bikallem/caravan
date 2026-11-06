(* The tests of the shared code of the tests of the examples. *)

open Windtrap
module Page = Example_support.Page

let () =
  exit
  @@ Windtrap.run "support"
       [ test "grep_runs: the first n characters of a run, then the next n"
           (fun () ->
             equal (list string) ["ab"; "cd"] (Page.grep_runs 2 "abcde") )
       ; test "grep_runs: a run shorter than n gives nothing" (fun () ->
             equal (list string) [] (Page.grep_runs 3 "ab cd") ;
             equal (list string) ["abc"] (Page.grep_runs 3 "ab abc") )
       ; test "grep_runs: only base64url characters make a run" (fun () ->
             equal (list string) ["bc"; "_d"; "-e"]
               (Page.grep_runs 2 "a.bc_d-e+f") ) ]
