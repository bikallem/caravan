let () =
  let app = Example_hello.App.make ~log:print_endline in
  Example_common.serve ~config:Example_hello.App.config ~services:[app.reporter]
    app.handler
