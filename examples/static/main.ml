let () =
  let root =
    Option.value
      (Sys.getenv_opt "STATIC_ROOT")
      ~default:"examples/static/public"
  in
  Example_common.serve (Example_static.App.make ~root)
