open Weft

(* weft runs at most 62 domains. *)
let default_domains () = min 62 (Domain.recommended_domain_count ())

let run ?config ?engine ?(domains = default_domains ()) ?(on_start = ignore)
    handler =
  Weft_linux.run ~domains ~signals:[Signal.SIGINT; Signal.SIGTERM] (fun () ->
      Fiber.scope (fun sw ->
          let s = Caravan.Server.start ?config ?engine sw handler in
          Fiber.fork_daemon ~name:"caravan.signals" sw (fun () ->
              Caravan.Server.stop_on_signals s ) ;
          on_start s ;
          Caravan.Server.wait s ) )
