(* Serves examples/html/htmx.min.js, if it is there. Run the example from the
   root of the repository. *)
let file = "examples/html/htmx.min.js"

let htmx =
  if Sys.file_exists file then
    Some (In_channel.with_open_bin file In_channel.input_all)
  else (
    print_endline ("No " ^ file ^ ": the page has no htmx.") ;
    None )

let () = Example_common.serve (Example_html.App.make ~htmx)
