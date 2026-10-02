module Mtime = Affect_unix.Mtime

type t = {
  addr : Unix.sockaddr;
  backlog : int;
  max_connections : int;
  buffer_size : int;
  max_request_line : int;
  max_header_bytes : int;
  max_header_count : int;
  max_body : int option;
  max_drain : int;
  header_timeout : Mtime.Span.t;
  idle_timeout : Mtime.Span.t;
  read_timeout : Mtime.Span.t;
  write_timeout : Mtime.Span.t;
  linger_timeout : Mtime.Span.t;
  shutdown_grace : Mtime.Span.t;
  date_header : bool;
  on_error : exn -> Printexc.raw_backtrace -> unit;
}

let print_error exn bt =
  Printf.eprintf "affect-http: %s\n%s%!" (Printexc.to_string exn)
    (Printexc.raw_backtrace_to_string bt)

let default =
  {
    addr = Unix.ADDR_INET (Unix.inet_addr_loopback, 8080);
    backlog = 1024;
    max_connections = 1024;
    buffer_size = 4096;
    max_request_line = 8192;
    max_header_bytes = 16384;
    max_header_count = 100;
    max_body = None;
    max_drain = 65536;
    header_timeout = Mtime.Span.(10 * s);
    idle_timeout = Mtime.Span.(30 * s);
    read_timeout = Mtime.Span.(30 * s);
    write_timeout = Mtime.Span.(30 * s);
    linger_timeout = Mtime.Span.(2 * s);
    shutdown_grace = Mtime.Span.(10 * s);
    date_header = true;
    on_error = print_error;
  }
