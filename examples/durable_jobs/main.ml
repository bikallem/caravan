(* The outbox of the example prints each mail, then flushes stdout. The services
   start in the list order (the queue, then Recurring) and stop in the reverse
   order: Recurring stops before its queue. *)

module App = Example_durable_jobs.App

let () =
  let store = Example_durable_jobs.Store.create () in
  let app =
    App.make ~store
      ~outbox:(fun m -> print_string m ; flush stdout)
      ~log:print_endline ~report:prerr_endline
  in
  Example_common.serve
    ~services:
      [ Caravan_jobs.Durable.service app.queue
      ; Caravan_jobs.Recurring.service app.recurring ]
    app.handler
