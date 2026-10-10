open Affect
open Caravan

type output = {written: string; fin: bool; reset: bool}

let loopback port = Unix.ADDR_INET (Unix.inet_addr_loopback, port)

let conn = Conn_info.make ~id:1 ~peer:(loopback 50000) ~local:(loopback 8080) ()

let serve_one ctx handler input =
  let out = Buffer.create 512 and fin = ref false and reset = ref false in
  let inner = Flow.of_string input out in
  let ended what =
    if !fin || !reset then failwith ("the server wrote after it " ^ what)
  in
  let flow =
    Flow.make ~time:(Flow.time inner)
      ~read:(fun buf off len -> Flow.read inner buf off len)
      ~read_now:(fun buf off len -> Flow.read_now inner buf off len)
      ~write:(fun buf off len ->
        ended "ended the connection" ;
        Flow.write inner buf off len )
      ~readable:(Flow.readable' inner ())
      ~shutdown_send:(fun () -> fin := true)
      ~close:ignore
      ~reset:(fun () -> reset := true)
      ~set_read_deadline:ignore ~set_write_deadline:ignore ()
  in
  Private.H1_server.serve ctx flow handler ;
  {written= Buffer.contents out; fin= !fin; reset= !reset}

let serve ~config handler inputs =
  let config = {config with Config.cancel_on_disconnect= false} in
  let ctx =
    {Engine.config; conn; stopping= Action.never; is_stopping= (fun () -> false)}
  in
  Affect_unix.Unix.main ~domain_count:1 (fun () ->
      List.map (serve_one ctx handler) inputs )
