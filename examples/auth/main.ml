let () =
  (* The generator of the OS: safe on many domains. *)
  Mirage_crypto_rng_unix.use_default () ;
  let key = Example_dev.Dev_key.session () in
  Example_common.serve ~host:"localhost"
    (Example_auth.App.make ~key ~report:prerr_endline
       ~users:Example_auth.App.demo_user )
