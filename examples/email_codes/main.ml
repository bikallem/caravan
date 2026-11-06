(* The outbox of this program prints each mail on the standard output. It is a
   stand-in for development only: it sends no mail. The queue is a service of
   the server: it starts before the server, and it stops after the server. *)

let dev_outbox mail =
  print_string
    ("--- dev outbox: this mail is not sent, it is only printed ---\n" ^ mail) ;
  flush stdout

let () =
  (* The generator of the OS: safe on many domains. *)
  Mirage_crypto_rng_unix.use_default () ;
  let key = Example_dev.Dev_key.session () in
  let app =
    Example_email_codes.App.make ~key ~outbox:dev_outbox ~report:prerr_endline
      ~users:Example_email_codes.App.demo_users
  in
  Example_common.serve ~host:"localhost"
    ~services:[Caravan_jobs.service app.queue]
    app.handler
