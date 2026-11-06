(* The code that all examples share. *)

let url ~scheme ~host (addr : Weft.Net.addr) =
  match addr with
  | Tcp (_, port) ->
      Printf.sprintf "%s://%s:%d" scheme host port
  | addr ->
      Format.asprintf "%a" Caravan.Conn_info.pp_addr addr

let serve ?(config = Caravan.Config.default) ?services ?engine
    ?(scheme = "http") ?(host = "127.0.0.1") ?(port = 8080) handler =
  let config = {config with addr= Tcp ("127.0.0.1", port)} in
  Caravan.Server.run ~config ?engine ?services
    (module Weft_os)
    handler
    ~on_start:(fun server ->
      Format.printf "Listening on %s (Ctrl-C to stop)@."
        (url ~scheme ~host (Caravan.Server.local_addr server)) )
