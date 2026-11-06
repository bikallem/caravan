(* Error_page gives a page to the client, and the report goes to stderr. *)
let () = Example_common.serve (Example_compress.App.make ~report:prerr_endline)
