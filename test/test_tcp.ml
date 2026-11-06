(* The tests of Tcp and Flow.linger on the simulator of weft. Tcp is
   Weft_flow.Tcp, and weft tests it (test/flow of weft). These tests are those
   that weft does not have, and the tests of linger, which is in caravan. Each
   test runs in Sim_support: 1 seed and 1 domain. *)

open Windtrap
open Weft
open Caravan

let with_flows = Server_support.with_flows

let read_all = Server_support.read_all

let read_exactly = Server_support.read_exactly

let read_one = Server_support.read_one

(* [waiting_read sim s flow] starts a read of 1 byte of [flow] in [s], and makes
   sure that the read waits after a settle. *)
let waiting_read sim s flow =
  let reader = Sim_support.call ~name:"reader" s (fun () -> read_one flow) in
  is_false ~msg:"the read waits" (Sim_support.ended sim reader) ;
  reader

(* [byte_before_deadline sim ~client flow] makes a read of [flow] wait until a
   byte of [client] comes 1 ns before the read deadline. Then a second read ends
   at the deadline. *)
let byte_before_deadline sim ~client flow =
  let open Sim_support in
  let t0 = Time.now Mono in
  let l = Span.s 11 in
  Flow.set_read_deadline flow (Some (t0 +: l)) ;
  Fiber.scope
  @@ fun s ->
  let first = waiting_read sim s flow in
  advance_to sim (just_before (t0 +: l)) ;
  is_false ~msg:"no end before the byte" (ended sim first) ;
  Flow.write_string client "x" ;
  is_true ~msg:"the byte ends the read" (ended sim first) ;
  equal int 1 (await first) ;
  let second = call s (fun () -> read_one flow) in
  raises Flow.Timeout (fun () -> ends_at sim ~t0 l second)

(* [each_way sim] sends 4 MiB from each side to the other at the same time. *)
let each_way sim =
  let open Sim_support in
  with_flows sim
  @@ fun ~server ~client ->
  let data = String.init (4 * 1024 * 1024) (fun i -> Char.chr (i land 0xff)) in
  Fiber.scope
  @@ fun s ->
  let send flow =
    call s (fun () ->
        Flow.write_string flow data ;
        Flow.shutdown_send flow )
  in
  let s1 = send client and s2 = send server in
  let r1 = call s (fun () -> read_all server) in
  let r2 = call s (fun () -> read_all client) in
  await s1 ;
  await s2 ;
  is_true ~msg:"the server got it" (String.equal data (await r1)) ;
  is_true ~msg:"the client got it" (String.equal data (await r2))

let tcp_tests =
  let open Sim_support in
  group "Tcp"
    [ test "4 MiB each way at once" (fun () -> run each_way)
    ; test "a wrapper flow gets the deadline of its inner flow" (fun () ->
          manual
          @@ fun sim ->
          with_flows sim
          @@ fun ~server ~client ->
          let wrapper = (Server_support.counted server).flow in
          byte_before_deadline sim ~client wrapper )
    ; test "a deadline removed from a read that waits stops it no more"
        (fun () ->
          manual
          @@ fun sim ->
          with_flows sim
          @@ fun ~server ~client ->
          let t0 = Time.now Mono in
          Flow.set_read_deadline server (Some (t0 +: Span.s 5)) ;
          Fiber.scope
          @@ fun s ->
          let reader = call s (fun () -> read_exactly server 4) in
          is_false ~msg:"the read waits" (ended sim reader) ;
          Flow.set_read_deadline server None ;
          advance sim (Span.s 10) ;
          is_false ~msg:"no end" (ended sim reader) ;
          Flow.write_string client "late" ;
          equal string "late" (await reader) ) ]

let linger_tests =
  let open Sim_support in
  group "Tcp linger"
    [ (* A close with unread input sends a reset to the peer. The linger reads
         that input until the end of the stream of the peer. In the simulator, a
         reset after the shutdown of the linger gives the peer the end of the
         stream. Thus the test also makes sure that the linger waits for the end
         of the stream. *)
      test "linger, then close, gives the peer the end of the stream" (fun () ->
          manual
          @@ fun sim ->
          with_flows sim
          @@ fun ~server ~client ->
          Flow.write_string client "req" ;
          let t0 = Time.now Mono in
          Flow.set_read_deadline server (Some (t0 +: Span.s 2)) ;
          Fiber.scope (fun s ->
              let linger = call s (fun () -> Flow.linger server ~max:100) in
              is_false ~msg:"the linger waits for the end of the stream"
                (ended sim linger) ;
              Flow.shutdown_send client ;
              (* No advance: the time is t0. *)
              is_true ~msg:"the linger ends before the deadline"
                (ended sim linger) ;
              await linger ) ;
          Flow.close server ;
          equal int ~msg:"the end of the stream" 0 (read_one client) )
    ; test "linger ends at the read deadline" (fun () ->
          manual
          @@ fun sim ->
          with_flows sim
          @@ fun ~server ~client:_ ->
          let t0 = Time.now Mono in
          Flow.set_read_deadline server (Some (t0 +: Span.s 2)) ;
          Fiber.scope
          @@ fun s ->
          (* The client sends nothing and does not close. *)
          let linger = call s (fun () -> Flow.linger server ~max:100) in
          ends_at sim ~t0 (Span.s 2) linger )
    ; test "linger ignores the read timeout of the flow" (fun () ->
          manual
          @@ fun sim ->
          with_flows sim
          @@ fun ~server ~client:_ ->
          let t0 = Time.now Mono in
          Flow.set_read_timeout server (Some (Span.s 1)) ;
          Flow.set_read_deadline server (Some (t0 +: Span.s 5)) ;
          Fiber.scope
          @@ fun s ->
          let linger = call s (fun () -> Flow.linger server ~max:100) in
          ends_at sim ~t0 (Span.s 5) linger ) ]

let () = exit (Windtrap.run "tcp" [tcp_tests; linger_tests])
