(* Server.run for the signal tests: prints its port, then "started" when a
   request to /slow (300ms) or /stuck (10s) starts, in CRLF lines. *)

open Caravan
module Mtime = Affect_unix.Mtime

let handler req =
  let pause span =
    print_string "started\r\n";
    flush stdout;
    Mtime.wait_for span;
    Response.string "done"
  in
  match Request.path_only req with
  | "/slow" -> pause Mtime.Span.(300 * ms)
  | "/stuck" -> pause Mtime.Span.(10 * s)
  | _ -> Response.string "hello"

let () =
  let config =
    { Config.default with addr = Unix.ADDR_INET (Unix.inet_addr_loopback, 0); date_header = false }
  in
  Server.run ~config ~domain_count:2 handler ~on_start:(fun server ->
      match Server.local_addr server with
      | Unix.ADDR_INET (_, port) -> Printf.printf "%d\r\n%!" port
      | Unix.ADDR_UNIX _ -> assert false)
