let () = Example_common.serve (Example_middleware.App.make ~log:print_endline)
