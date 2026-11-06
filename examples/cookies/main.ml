let () =
  Example_common.serve ~host:"localhost"
    (Example_cookies.App.make ~log:print_endline)
