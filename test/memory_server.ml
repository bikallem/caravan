open Weft
open Caravan

type output = {written: string; fin: bool; reset: bool}

let conn =
  Conn_info.make ~id:1
    ~peer:(Net.Tcp ("127.0.0.1", 50000))
    ~local:(Net.Tcp ("127.0.0.1", 8080))
    ()

let serve_one ctx handler input =
  let out = Buffer.create 512 and fin = ref false and reset = ref false in
  let inner = Flow.of_string input out in
  let ended what =
    if !fin || !reset then failwith ("the server wrote after it " ^ what)
  in
  let flow =
    Flow.make
      ~read:(fun buf off len -> Flow.read inner buf off len)
      ~read_now:(fun buf off len -> Flow.read_now inner buf off len)
      ~write:(fun buf off len ->
        ended "ended the connection" ;
        Flow.write inner buf off len )
      ~readable:(Flow.readable inner)
      ~shutdown_send:(fun () -> fin := true)
      ~close:ignore
      ~reset:(fun () -> reset := true)
      ~set_read_deadline:ignore ~set_write_deadline:ignore ()
  in
  Private.H1_server.serve ctx flow handler ;
  {written= Buffer.contents out; fin= !fin; reset= !reset}

(* The time of the Date fields of the conformance tests: Sun, 06 Nov 1994
   08:49:37 GMT. The simulator starts at a different time. *)
let date = Time.posix_of_unix_s 784111777.

let serve ~config handler inputs =
  let config = {config with Config.cancel_on_disconnect= false} in
  let ctx = {Engine.config; conn; is_stopping= (fun () -> false)} in
  Sim_support.run (fun sim ->
      Weft_sim.set_posix sim date ;
      List.map (serve_one ctx handler) inputs )
