(* The outbox of the example prints each mail, then flushes stdout. The queue is
   a service of the server: it starts before the server, and it stops after the
   server. *)

let () =
  let app =
    Example_jobs.App.make
      ~outbox:(fun m -> print_string m ; flush stdout)
      ~report:prerr_endline
  in
  Example_common.serve ~services:[Caravan_jobs.service app.queue] app.handler
