(* The tests of caravan.tls, in the simulator. Each test sets a new Fortuna
   generator with a fixed seed first, thus a run is the same for a seed. The
   simulator runs on one OS thread, thus Fortuna is correct here. *)

open Windtrap
open Caravan
open Weft
module T = Caravan_testing
module S = Caravan_tls

let seed () =
  Mirage_crypto_rng.set_default_generator
    (Mirage_crypto_rng.create ~seed:"caravan tls tests"
       (module Mirage_crypto_rng.Fortuna) )

(* The time of the simulator in the tests: 2025-06-15. The certificates are
   valid from 2020 to 2040. *)
let t0 = 1_750_000_000.

let posix_now () = Ptime.of_float_s (Time.posix_to_unix_s (Time.now Posix))

(* [setup ?server_alpn ?client_alpn ?peer_name ()] seeds the generator and gives
   a server and a client configuration. *)
let setup ?server_alpn ?client_alpn ?peer_name () =
  seed () ;
  Tls_support.configs ?server_alpn ?client_alpn ?peer_name ~time:posix_now ()

let server_addr = Net.Tcp ("127.0.0.1", 80)

let raises_closed ?msg f =
  raises_match ?msg (function Flow.Closed -> true | _ -> false) f

(* [run ?server_alpn ?client_alpn handler f] serves [handler] over TLS in the
   simulator at [t0], and gives [f] the client configuration. *)
let run ?server_alpn ?client_alpn handler f =
  let scfg, ccfg = setup ?server_alpn ?client_alpn () in
  T.run ~engine:(S.engine scfg Server.http1) handler (fun t ->
      Weft_sim.set_posix (T.sim t) (Time.posix_of_unix_s t0) ;
      f t ccfg )

(* [read_all ?strict flow] reads [flow] to its end. With [~strict:false]
   (default), [Flow.Closed] is the end: for a TCP flow that the server resets.
   With [~strict:true], a TLS flow must end with close_notify. *)
let read_all ?(strict = false) flow =
  let b = Buffer.create 4096 and buf = Bytes.create 4096 in
  let rec go () =
    match Flow.read flow buf 0 4096 with
    | 0 ->
        Buffer.contents b
    | n ->
        Buffer.add_subbytes b buf 0 n ;
        go ()
    | exception Flow.Closed when not strict ->
        Buffer.contents b
  in
  go ()

(* [exchange ccfg text] sends [text] on a new TLS connection and reads the
   answer to the end. *)
let exchange ccfg text =
  let flow, _ = S.client ccfg (Tcp.connect server_addr) in
  Fun.protect ~finally:(fun () -> Flow.close flow)
  @@ fun () ->
  Flow.write_string flow text ;
  read_all ~strict:true flow

let get path =
  Printf.sprintf
    "GET %s HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n" path

(* [body_of response] is the text after the head. *)
let body_of r =
  let rec find i =
    if i + 4 > String.length r then fail "no end of the head"
    else if String.sub r i 4 = "\r\n\r\n" then i + 4
    else find (i + 1)
  in
  let i = find 0 in
  String.sub r i (String.length r - i)

(* A handler that tells the scheme and the ALPN of its request. *)
let info req =
  let alpn =
    Option.value ~default:"none" (Option.bind (Request.conn req) Conn_info.alpn)
  in
  Response.string (Request.scheme req ^ " " ^ alpn)

let count sub s =
  let n = String.length sub in
  let rec go i acc =
    if i + n > String.length s then acc
    else if String.sub s i n = sub then go (i + n) (acc + 1)
    else go (i + 1) acc
  in
  go 0 0

let no_errors t =
  equal (list string) []
    (List.map (fun (e, _) -> Printexc.to_string e) (T.take_errors t))

let elapsed start = Span.to_ns (Time.diff Mono (Time.now Mono) start)

(* {1 A raw client of ocaml-tls} *)

(* [raw_handshake ?with_finished tcp ccfg] does the handshake of a client with
   Tls.Engine on [tcp], and gives the state. [with_finished tls] gives the state
   and the records to send in the same TCP write as the Finished message. *)
let raw_handshake ?with_finished tcp ccfg =
  let buf = Bytes.create 65536 in
  let tls, hello = Tls.Engine.client ccfg in
  Flow.write_string tcp hello ;
  let rec go tls =
    if not (Tls.Engine.handshake_in_progress tls) then tls
    else
      let n = Flow.read tcp buf 0 65536 in
      match Tls.Engine.handle_tls tls (Bytes.sub_string buf 0 n) with
      | Ok (tls, _, `Response r, _) -> (
        match (r, with_finished) with
        | Some fin, Some f when not (Tls.Engine.handshake_in_progress tls) ->
            let tls, more = f tls in
            Flow.write_string tcp (fin ^ more) ;
            tls
        | _ ->
            Option.iter (Flow.write_string tcp) r ;
            go tls )
      | Error (f, _) ->
          fail (Tls.Engine.string_of_failure f)
  in
  go tls

(* [raw_read ?on_data tcp tls] reads [tcp] with Tls.Engine to the end, and gives
   the plaintext. [on_data tls n] runs after each data, with the count of the
   bytes so far, and gives the next state. *)
let raw_read ?(on_data = fun tls _ -> tls) tcp tls =
  let buf = Bytes.create 65536 and out = Buffer.create 4096 in
  let rec go tls =
    match Flow.read tcp buf 0 65536 with
    | 0 ->
        ()
    | n -> (
      match Tls.Engine.handle_tls tls (Bytes.sub_string buf 0 n) with
      | Ok (tls, eof, `Response r, `Data d) ->
          Option.iter (Flow.write_string tcp) r ;
          Option.iter (Buffer.add_string out) d ;
          let tls = on_data tls (Buffer.length out) in
          if eof = None then go tls
      | Error (f, _) ->
          fail (Tls.Engine.string_of_failure f) )
  in
  go tls ; Buffer.contents out

let send tcp tls data =
  let tls, out = Option.get (Tls.Engine.send_application_data tls [data]) in
  Flow.write_string tcp out ; tls

let engine_tests =
  group "The engine"
    [ test "a GET over TLS, with the scheme https" (fun () ->
          run info (fun t ccfg ->
              let r = exchange ccfg (get "/") in
              is_true ~msg:r (String.starts_with ~prefix:"HTTP/1.1 200" r) ;
              equal string "https none" (body_of r) ;
              no_errors t ) )
    ; test "ALPN: the protocol of the session goes to Conn_info" (fun () ->
          run ~client_alpn:["h2"; "http/1.1"] info (fun _ ccfg ->
              equal string "https http/1.1" (body_of (exchange ccfg (get "/"))) ) )
    ; test "bodies of 1 MiB in the two directions" (fun () ->
          let data =
            String.init (1024 * 1024) (fun i -> Char.chr (i mod 251))
          in
          let echo req = Response.string (Body.to_string (Request.body req)) in
          run echo (fun _ ccfg ->
              let r =
                exchange ccfg
                  (Printf.sprintf
                     "POST / HTTP/1.1\r\n\
                      Host: localhost\r\n\
                      Connection: close\r\n\
                      Content-Length: %d\r\n\
                      \r\n\
                      %s"
                     (String.length data) data )
              in
              is_true ~msg:"the bytes" (String.equal data (body_of r)) ) )
    ; test "three requests on one connection" (fun () ->
          run info (fun _ ccfg ->
              let one = "GET / HTTP/1.1\r\nHost: localhost\r\n\r\n" in
              equal int 3
                (count "HTTP/1.1 200" (exchange ccfg (one ^ one ^ get "/"))) ) )
    ; test "the request in the same TCP write as the Finished message"
        (fun () ->
          run info (fun _ ccfg ->
              let tcp = Tcp.connect server_addr in
              Fun.protect ~finally:(fun () -> Flow.close tcp)
              @@ fun () ->
              let with_finished tls =
                Option.get (Tls.Engine.send_application_data tls [get "/"])
              in
              let tls = raw_handshake ~with_finished tcp ccfg in
              equal string "https none" (body_of (raw_read tcp tls)) ) )
    ; test "a handshake that fails or ends early is not reported" (fun () ->
          run info (fun t _ ->
              (* A handshake record with a bad body: the server sends an
                 alert. *)
              let tcp = Tcp.connect server_addr in
              Flow.write_string tcp "\x16\x03\x01\x00\x05hello" ;
              ignore (read_all tcp) ;
              Flow.close tcp ;
              (* A client that closes at once. *)
              Flow.close (Tcp.connect server_addr) ;
              (* The header of a long record: the deadline of the head ends
                 it. *)
              let tcp = Tcp.connect server_addr in
              Flow.write_string tcp "GET / HTTP/1.1\r\n\r\n" ;
              let start = Time.now Mono in
              ignore (read_all tcp) ;
              Flow.close tcp ;
              is_true ~msg:"the deadline of the head"
                (elapsed start >= Span.to_ns Config.default.header_timeout) ;
              T.settle t ;
              no_errors t ) )
    ; test "a handshake of more than 64 KiB ends" (fun () ->
          run info (fun t ccfg ->
              (* A ClientHello, then ChangeCipherSpec records: ocaml-tls accepts
                 each one, thus only the limit ends the handshake. *)
              let tcp = Tcp.connect server_addr in
              let _, hello = Tls.Engine.client ccfg in
              Flow.write_string tcp hello ;
              let ccs =
                String.concat ""
                  (List.init 2000 (fun _ -> "\x14\x03\x03\x00\x01\x01"))
              in
              let start = Time.now Mono in
              ( match
                  for _ = 1 to 10 do
                    Flow.write_string tcp ccs
                  done
                with
              | () ->
                  ()
              | exception Flow.Closed ->
                  () ) ;
              ignore (read_all tcp) ;
              Flow.close tcp ;
              is_true ~msg:"before the deadline"
                (elapsed start < Span.to_ns (Span.s 1)) ;
              T.settle t ;
              no_errors t ) )
    ; test "an idle connection ends with close_notify" (fun () ->
          run info (fun _ ccfg ->
              let flow, _ = S.client ccfg (Tcp.connect server_addr) in
              Fun.protect ~finally:(fun () -> Flow.close flow)
              @@ fun () ->
              Flow.write_string flow "GET / HTTP/1.1\r\nHost: localhost\r\n\r\n" ;
              (* The server closes after the idle timeout: the read gives 0, not
                 Closed. *)
              let r = read_all ~strict:true flow in
              equal string "https none" (body_of r) ) )
    ; test "a client that leaves with no close_notify is not reported"
        (fun () ->
          run info (fun t ccfg ->
              let tcp = Tcp.connect server_addr in
              let flow, _ = S.client ccfg tcp in
              Flow.write_string flow "GET / HTTP/1.1\r\nHost: localhost\r\n" ;
              Flow.close tcp ;
              T.settle t ;
              no_errors t ) )
    ; test "a stop of the server ends a slow handshake" (fun () ->
          run info (fun t _ ->
              let tcp = Tcp.connect server_addr in
              let start = Time.now Mono in
              let reader = T.background t (fun () -> ignore (read_all tcp)) in
              Time.sleep Mono (Span.ms 100) ;
              Server.shutdown (T.server t) ;
              Promise.await reader ;
              Flow.close tcp ;
              is_true ~msg:"less than 1 s"
                (elapsed start < Span.to_ns (Span.s 1)) ) )
    ; test "the client refuses a certificate for another name" (fun () ->
          let peer_name =
            Domain_name.(host_exn (of_string_exn "example.org"))
          in
          let scfg, _ = setup () in
          let _, ccfg = setup ~peer_name () in
          T.run ~engine:(S.engine scfg Server.http1) info (fun t ->
              Weft_sim.set_posix (T.sim t) (Time.posix_of_unix_s t0) ;
              raises_match
                (function S.Handshake_error _ -> true | _ -> false)
                (fun () -> ignore (exchange ccfg (get "/"))) ) )
    ; test "the fds after 20 connections" (fun () ->
          run info (fun t ccfg ->
              ignore (exchange ccfg (get "/")) ;
              T.settle t ;
              let n = Weft_sim.open_fds (T.sim t) in
              for _ = 1 to 20 do
                ignore (exchange ccfg (get "/"))
              done ;
              T.settle t ;
              equal int n (Weft_sim.open_fds (T.sim t)) ) )
    ; test "engine refuses h2, TLS 1.0 and a missing generator" (fun () ->
          let is_invalid = function Invalid_argument _ -> true | _ -> false in
          let scfg, _ = setup ~server_alpn:["h2"; "http/1.1"] () in
          raises_match is_invalid (fun () ->
              ignore (S.engine scfg Server.http1 : Engine.t) ) ;
          let old_tls =
            Result.get_ok
              (Tls.Config.server ~version:(`TLS_1_0, `TLS_1_3)
                 ~certificates:
                   (let _, leaf, key = Tls_support.pki () in
                    `Single ([leaf], key) )
                 () )
          in
          raises_match is_invalid (fun () ->
              ignore (S.engine old_tls Server.http1 : Engine.t) ) ;
          let scfg, _ = setup () in
          Mirage_crypto_rng.unset_default_generator () ;
          Fun.protect ~finally:seed
          @@ fun () ->
          raises_match is_invalid (fun () ->
              ignore (S.engine scfg Server.http1 : Engine.t) ) ) ]

(* {1 The flow, with no HTTP} *)

(* [fake_tcp ?readable ?write tcp] is [tcp] with a [readable] and a [write] of
   the test. *)
let fake_tcp ?readable ?write tcp =
  Flow.make ~read:(Flow.read tcp) ~read_now:(Flow.read_now tcp)
    ~write:(Option.value write ~default:(Flow.write tcp))
    ~readable:(Option.value readable ~default:(Flow.readable tcp))
    ~shutdown_send:(fun () -> Flow.shutdown_send tcp)
    ~close:(fun () -> Flow.close tcp)
    ~set_read_deadline:(Flow.set_read_deadline tcp)
    ~set_write_deadline:(Flow.set_write_deadline tcp)
    ()

let listen () = Net.listen (Net.Tcp ("server", 443))

let connect () = Tcp.connect (Net.Tcp ("server", 443))

(* [accept_one s scfg l] runs [S.server] on the next connection of [l], and
   gives the text of its error, or ["closed"] for a reset of the peer. *)
let accept_one ?(pause = Span.zero) s scfg l =
  Fiber.fork_promise s (fun () ->
      let c, _ = Net.accept l in
      let tcp = Tcp.flow c in
      Time.sleep Mono pause ;
      Fun.protect ~finally:(fun () -> Flow.close tcp)
      @@ fun () ->
      match S.server scfg tcp with
      | _ ->
          "a session"
      | exception S.Handshake_error m ->
          m
      | exception Flow.Closed ->
          "closed" )

(* [pair ?domains ?wrap ?swrap f] runs [f server client client_tcp] with the TLS
   flows of one connection in the simulator. [wrap] and [swrap] change the TCP
   flows of the client and of the server. *)
let pair ?domains ?(wrap = Fun.id) ?(swrap = Fun.id) f =
  let scfg, ccfg = setup () in
  Sim_support.run ?domains (fun sim ->
      Weft_sim.set_posix sim (Time.posix_of_unix_s t0) ;
      let l = listen () in
      Fiber.scope (fun s ->
          let server =
            Fiber.fork_promise s (fun () ->
                let c, _ = Net.accept l in
                fst (S.server scfg (swrap (Tcp.flow c))) )
          in
          let tcp = wrap (connect ()) in
          let client, _ = S.client ccfg tcp in
          let server = Promise.await server in
          Fun.protect
            ~finally:(fun () ->
              Flow.close client ; Flow.close server ; Net.close l )
            (fun () -> f server client tcp) ) )

let read_string flow n =
  let buf = Bytes.create n in
  Bytes.sub_string buf 0 (Flow.read flow buf 0 n)

(* An application data record with a bad MAC. *)
let bad_record = "\x17\x03\x03\x00\x20" ^ String.make 32 'x'

let ready flow =
  sync
    (choose
       [ wrap (Flow.readable flow) (fun () -> true)
       ; wrap (Time.after Mono (Span.s 1)) (fun () -> false) ] )

(* [read_n flow n] reads [n] bytes of [flow]. *)
let read_n flow size =
  let b = Buffer.create size and buf = Bytes.create 65536 in
  while Buffer.length b < size do
    let n = Flow.read flow buf 0 65536 in
    if n = 0 then fail "an early end" ;
    Buffer.add_subbytes b buf 0 n
  done ;
  Buffer.contents b

let flow_tests =
  group "The flow"
    [ test "read_now gives None for a part of a record" (fun () ->
          (* After [split := true], a write of 10 bytes or more goes in two
             parts, with 1 s between them. *)
          let split = ref false in
          let wrap tcp =
            fake_tcp tcp ~write:(fun buf off len ->
                if len < 10 || not !split then Flow.write tcp buf off len
                else (
                  Flow.write tcp buf off (len / 2) ;
                  Time.sleep Mono (Span.s 1) ;
                  Flow.write tcp buf (off + (len / 2)) (len - (len / 2)) ) )
          in
          pair ~wrap (fun server client _ ->
              split := true ;
              (* No byte came: read_now does not wait. *)
              equal (option int) None
                (Flow.read_now server (Bytes.create 16) 0 16) ;
              Fiber.scope (fun s ->
                  Fiber.fork s (fun () -> Flow.write_string client "hello") ;
                  (* At 0.5 s, half of the record came: no data yet. A read_now
                     that waits gives the data at 1 s. *)
                  Time.sleep Mono (Span.ms 500) ;
                  equal (option int) None
                    (Flow.read_now server (Bytes.create 16) 0 16) ;
                  equal string "hello" (read_string server 16) ) ) )
    ; test "shutdown_send sends close_notify: the peer reads 0" (fun () ->
          pair (fun server client _ ->
              Flow.write_string server "bye" ;
              Flow.shutdown_send server ;
              (* The data and close_notify can come in one TCP read. *)
              Time.sleep Mono (Span.ms 100) ;
              equal string "bye" (read_string client 16) ;
              equal int 0 (Flow.read client (Bytes.create 1) 0 1) ;
              equal (option int) (Some 0)
                (Flow.read_now client (Bytes.create 1) 0 1) ) )
    ; test "a TCP end with no close_notify raises Closed" (fun () ->
          pair (fun server client tcp ->
              Flow.shutdown_send tcp ;
              raises_closed (fun () -> ignore (read_string server 16)) ;
              (* Each later read raises again. *)
              raises_closed (fun () -> ignore (read_string server 16)) ;
              (* The peer can still read: a write works. *)
              Flow.write_string server "still" ;
              equal string "still" (read_string client 16) ) )
    ; test "a TCP end with no close_notify, seen by read_now" (fun () ->
          pair (fun server _ tcp ->
              Flow.shutdown_send tcp ;
              Time.sleep Mono (Span.ms 100) ;
              raises_closed (fun () ->
                  ignore (Flow.read_now server (Bytes.create 4) 0 4) ) ) )
    ; test "a bad record after the handshake raises Closed" (fun () ->
          pair (fun server client tcp ->
              (* The plaintext before the bad record comes first. *)
              Flow.write_string client "ok" ;
              Time.sleep Mono (Span.ms 100) ;
              Flow.write_string tcp bad_record ;
              Time.sleep Mono (Span.ms 100) ;
              equal string "ok" (read_string server 16) ;
              raises_closed (fun () -> ignore (read_string server 16)) ) )
    ; test "a record header with no body raises Closed" (fun () ->
          (* An empty record is 5 bytes: the split must give it to the session,
             which refuses it. *)
          pair (fun server _ tcp ->
              Flow.write_string tcp "\x17\x03\x03\x00\x00" ;
              raises_closed (fun () -> ignore (read_string server 16)) ) )
    ; test "a record header of 64 KiB fails at once" (fun () ->
          (* ocaml-tls refuses a record of more than 18 KiB, thus the flow gives
             the header to it at once and does not wait for 64 KiB. *)
          pair (fun server _ tcp ->
              Flow.write_string tcp "\x17\x03\x03\xff\xff" ;
              raises_closed (fun () -> ignore (read_string server 16)) ) ;
          (* A data record, then the header, in one TCP write. *)
          let scfg, ccfg = setup () in
          Sim_support.run (fun sim ->
              Weft_sim.set_posix sim (Time.posix_of_unix_s t0) ;
              let l = listen () in
              Fiber.scope (fun s ->
                  let server =
                    Fiber.fork_promise s (fun () ->
                        let c, _ = Net.accept l in
                        fst (S.server scfg (Tcp.flow c)) )
                  in
                  let tcp = connect () in
                  let tls = raw_handshake tcp ccfg in
                  let server = Promise.await server in
                  let _, out =
                    Option.get (Tls.Engine.send_application_data tls ["x"])
                  in
                  Flow.write_string tcp (out ^ "\x17\x03\x03\xff\xff") ;
                  equal string "x" (read_string server 16) ;
                  raises_closed (fun () -> ignore (read_string server 16)) ;
                  Flow.close tcp ;
                  Flow.close server ;
                  Net.close l ) ) )
    ; test "KeyUpdates in the write of the Finished message count" (fun () ->
          (* [updates n] sends the Finished message, [n] KeyUpdates and "x" in
             one TCP write: the records after the handshake go to the budget of
             the session. *)
          let scfg, ccfg = setup () in
          let updates n =
            Sim_support.run (fun sim ->
                Weft_sim.set_posix sim (Time.posix_of_unix_s t0) ;
                let l = listen () in
                Fiber.scope (fun s ->
                    let server =
                      Fiber.fork_promise s (fun () ->
                          let c, _ = Net.accept l in
                          let tcp = Tcp.flow c in
                          Fun.protect ~finally:(fun () -> Flow.close tcp)
                          @@ fun () ->
                          match S.server scfg tcp with
                          | f, _ ->
                              read_string f 16
                          | exception S.Handshake_error m ->
                              m )
                    in
                    let tcp = connect () in
                    let with_finished tls =
                      let rec go tls n acc =
                        if n = 0 then
                          let tls, out =
                            Option.get
                              (Tls.Engine.send_application_data tls ["x"])
                          in
                          (tls, String.concat "" (List.rev (out :: acc)))
                        else
                          match Tls.Engine.key_update ~request:false tls with
                          | Ok (tls, out) ->
                              go tls (n - 1) (out :: acc)
                          | Error f ->
                              fail (Tls.Engine.string_of_failure f)
                      in
                      go tls n []
                    in
                    ignore (raw_handshake ~with_finished tcp ccfg) ;
                    let r = Promise.await server in
                    Flow.close tcp ; Net.close l ; r ) )
          in
          equal string "x" (updates 64) ;
          equal string "too many records with no plaintext" (updates 65) )
    ; test "records after close_notify, and no empty record" (fun () ->
          let scfg, ccfg = setup () in
          Sim_support.run (fun sim ->
              Weft_sim.set_posix sim (Time.posix_of_unix_s t0) ;
              let l = listen () in
              Fiber.scope (fun s ->
                  let server =
                    Fiber.fork_promise s (fun () ->
                        let c, _ = Net.accept l in
                        fst (S.server scfg (Tcp.flow c)) )
                  in
                  let tcp = connect () in
                  let tls = raw_handshake tcp ccfg in
                  let server = Promise.await server in
                  (* "a", close_notify and "b" in one TCP write: the flow
                     ignores "b". *)
                  let tls, a =
                    Option.get (Tls.Engine.send_application_data tls ["a"])
                  in
                  let tls, close = Tls.Engine.send_close_notify tls in
                  let _, b =
                    match Tls.Engine.send_application_data tls ["b"] with
                    | Some x ->
                        x
                    | None ->
                        (* ocaml-tls can refuse data after its close_notify:
                           then the record of "a" again stands for "b". *)
                        (tls, a)
                  in
                  Flow.write_string tcp (a ^ close ^ b) ;
                  equal string "a" (read_string server 16) ;
                  equal int 0 (Flow.read server (Bytes.create 4) 0 4) ;
                  (* A write of 16 KiB makes no empty record: ocaml-tls 2.1.3
                     adds one to a write of exactly 16384 bytes. An empty record
                     of TLS 1.3 has 17 bytes: the type and the tag. *)
                  Flow.write_string server (String.make 16384 'w') ;
                  Flow.shutdown_send server ;
                  let wire = read_all tcp in
                  let rec lengths i acc =
                    if i + 5 > String.length wire then acc
                    else
                      let len =
                        (Char.code wire.[i + 3] lsl 8)
                        lor Char.code wire.[i + 4]
                      in
                      lengths (i + 5 + len) (len :: acc)
                  in
                  is_false ~msg:"an empty record" (List.mem 17 (lengths 0 [])) ;
                  Flow.close tcp ;
                  Flow.close server ;
                  Net.close l ) ) )
    ; test "shutdown_send ends the TCP stream when close_notify fails"
        (fun () ->
          let fail_write = ref false in
          let wrap tcp =
            fake_tcp tcp ~write:(fun buf off len ->
                if !fail_write then raise Flow.Timeout
                else Flow.write tcp buf off len )
          in
          pair ~wrap (fun server client _ ->
              fail_write := true ;
              raises_match
                (function Flow.Timeout -> true | _ -> false)
                (fun () -> Flow.shutdown_send client) ;
              (* The TCP end came, with no close_notify. *)
              raises_closed (fun () -> ignore (read_string server 16)) ) )
    ; test "readable: buffered data, a failed flow, and no busy loop" (fun () ->
          (* After [deaf := true], the TCP flow of the server is never readable:
             then only the buffer and the state of the TLS flow make it
             readable. *)
          let deaf = ref false in
          let swrap tcp =
            fake_tcp tcp
              ~readable:
                (guard (fun () -> if !deaf then never else Flow.readable tcp))
          in
          pair ~swrap (fun server client tcp ->
              (* A wait for data that comes at 1 s: readiness can be stale, but
                 a readable that is always ready makes a busy loop. *)
              Fiber.scope (fun s ->
                  Fiber.fork s (fun () ->
                      Time.sleep Mono (Span.s 1) ;
                      Flow.write_string client "z" ) ;
                  let rec wait n =
                    if n > 100 then fail "a busy loop on readable" ;
                    sync (Flow.readable server) ;
                    match Flow.read_now server (Bytes.create 4) 0 4 with
                    | Some _ ->
                        ()
                    | None ->
                        wait (n + 1)
                  in
                  wait 0 ) ;
              Flow.write_string client "abcdef" ;
              equal string "ab" (read_string server 2) ;
              deaf := true ;
              is_true ~msg:"buffered" (ready server) ;
              equal string "cdef" (read_string server 16) ;
              Flow.write_string tcp bad_record ;
              raises_closed (fun () -> ignore (read_string server 16)) ;
              is_true ~msg:"failed" (ready server) ) )
    ; test "a write that fails in a record fails the flow" (fun () ->
          (* After [fail_write := true], the next write sends half of its bytes,
             then raises Timeout. *)
          let fail_write = ref false in
          let wrap tcp =
            fake_tcp tcp ~write:(fun buf off len ->
                if !fail_write then (
                  fail_write := false ;
                  Flow.write tcp buf off (len / 2) ;
                  raise Flow.Timeout )
                else Flow.write tcp buf off len )
          in
          pair ~wrap (fun _ client _ ->
              fail_write := true ;
              raises_match
                (function Flow.Timeout -> true | _ -> false)
                (fun () -> Flow.write_string client "first") ;
              (* Half of a record went out: no record can follow it. *)
              raises_closed (fun () -> Flow.write_string client "second") ) )
    ; test "a write after close_notify, and after a failed write" (fun () ->
          pair (fun server client tcp ->
              Flow.shutdown_send server ;
              raises_closed (fun () -> Flow.write_string server "late") ;
              (* A write of the client on a closed TCP flow fails it. *)
              Flow.close tcp ;
              raises_closed (fun () -> Flow.write_string client "x") ;
              raises_closed (fun () -> ignore (read_string client 16)) ;
              (* shutdown_send of a failed flow raises nothing. *)
              Flow.shutdown_send client ) )
    ; test "a reader never waits for a TCP write, on 4 domains" (fun () ->
          (* The client sends 1 MiB, and reads only after that. The server reads
             in one fiber, and sends 1 MiB in another one. The socket of the
             simulator holds 64 KiB: the write of the server waits until the
             client reads. If the reader of the server waited for that write,
             the upload of the client would never end. *)
          let size = 1024 * 1024 in
          let data = String.init size (fun i -> Char.chr (i mod 253)) in
          pair ~domains:4 (fun server client _ ->
              Fiber.scope (fun s ->
                  let got =
                    Fiber.fork_promise s (fun () -> read_n server size)
                  in
                  Fiber.fork s (fun () -> Flow.write_string server data) ;
                  Flow.write_string client data ;
                  is_true ~msg:"client" (String.equal data (read_n client size)) ;
                  is_true ~msg:"server" (String.equal data (Promise.await got)) ) ) )
    ; test "a peer that leaves or stops in the handshake" (fun () ->
          let scfg, ccfg = setup () in
          Sim_support.run (fun sim ->
              Weft_sim.set_posix sim (Time.posix_of_unix_s t0) ;
              let l = listen () in
              Fiber.scope (fun s ->
                  let server = accept_one s scfg l in
                  Flow.close (connect ()) ;
                  equal string "the peer closed the connection"
                    (Promise.await server) ;
                  (* A close_notify alert in place of a ClientHello. *)
                  let server = accept_one s scfg l in
                  let tcp = connect () in
                  Flow.write_string tcp "\x15\x03\x03\x00\x02\x01\x00" ;
                  equal string "the peer closed the session"
                    (Promise.await server) ;
                  Flow.close tcp ;
                  (* A ClientHello, then a plain alert: TLS 1.3 refuses it. *)
                  let server = accept_one s scfg l in
                  let _, hello = Tls.Engine.client ccfg in
                  let tcp = connect () in
                  Flow.write_string tcp (hello ^ "\x15\x03\x03\x00\x02\x01\x00") ;
                  is_true ~msg:"an alert after the hello"
                    (Promise.await server <> "a session") ;
                  Flow.close tcp ;
                  (* A bad record, then a reset: the alert of the server finds
                     no peer. *)
                  let server = accept_one ~pause:(Span.ms 100) s scfg l in
                  let tcp = connect () in
                  Flow.write_string tcp "\x16\x03\x01\x00\x05hello" ;
                  Flow.reset tcp ;
                  is_true ~msg:"garbage" (Promise.await server <> "a session") ;
                  Net.close l ) ) )
    ; test "a KeyUpdate of the peer during a response" (fun () ->
          (* A raw client asks for a KeyUpdate in the middle of a 1 MiB
             response. The server must send its KeyUpdate before its next data:
             if not, the client cannot read the data. *)
          let scfg, ccfg = setup () in
          let size = 1024 * 1024 in
          Sim_support.run (fun sim ->
              Weft_sim.set_posix sim (Time.posix_of_unix_s t0) ;
              let l = listen () in
              Fiber.scope (fun s ->
                  let server =
                    Fiber.fork_promise s (fun () ->
                        let c, _ = Net.accept l in
                        let f, _ = S.server scfg (Tcp.flow c) in
                        ignore (read_string f 16) ;
                        (* A reader fiber gives the KeyUpdate to the session
                           while the server writes. *)
                        Fiber.scope (fun s ->
                            Fiber.fork s (fun () ->
                                match read_string f 16 with
                                | _ ->
                                    ()
                                | exception Flow.Closed ->
                                    () ) ;
                            for _ = 1 to 64 do
                              Flow.write_string f (String.make (size / 64) 'k') ;
                              Time.sleep Mono (Span.ms 1)
                            done ;
                            Flow.shutdown_send f ) ;
                        f )
                  in
                  let tcp = connect () in
                  let tls = send tcp (raw_handshake tcp ccfg) "go" in
                  let updated = ref false in
                  let on_data tls got =
                    if got > size / 4 && not !updated then (
                      updated := true ;
                      match Tls.Engine.key_update ~request:true tls with
                      | Ok (tls, out) ->
                          Flow.write_string tcp out ; tls
                      | Error f ->
                          fail (Tls.Engine.string_of_failure f) )
                    else tls
                  in
                  let got = raw_read ~on_data tcp tls in
                  is_true ~msg:"a KeyUpdate" !updated ;
                  equal int size (String.length got) ;
                  Flow.close tcp ;
                  Flow.close (Promise.await server) ;
                  Net.close l ) ) )
    ; test "records with no plaintext have a budget" (fun () ->
          (* 64 KeyUpdates pass. More than the budget fail the flow: ocaml-tls
             keeps memory for each one. *)
          let scfg, ccfg = setup () in
          Sim_support.run (fun sim ->
              Weft_sim.set_posix sim (Time.posix_of_unix_s t0) ;
              let l = listen () in
              Fiber.scope (fun s ->
                  let server =
                    Fiber.fork_promise s (fun () ->
                        let c, _ = Net.accept l in
                        fst (S.server scfg (Tcp.flow c)) )
                  in
                  let tcp = connect () in
                  let tls = ref (raw_handshake tcp ccfg) in
                  let server = Promise.await server in
                  let updates n =
                    for _ = 1 to n do
                      match Tls.Engine.key_update ~request:false !tls with
                      | Ok (t, out) ->
                          tls := t ;
                          Flow.write_string tcp out
                      | Error f ->
                          fail (Tls.Engine.string_of_failure f)
                    done
                  in
                  updates 64 ;
                  tls := send tcp !tls "a" ;
                  equal string "a" (read_string server 16) ;
                  (* With "a", 32 KiB of plaintext give 2 more. 32767 bytes:
                     ocaml-tls adds an empty record to 32768. *)
                  let data = String.make 32767 'p' in
                  tls := send tcp !tls data ;
                  equal int 32767 (String.length (read_n server 32767)) ;
                  updates 2 ;
                  tls := send tcp !tls "b" ;
                  equal string "b" (read_string server 16) ;
                  updates 1 ;
                  tls := send tcp !tls "c" ;
                  raises_closed (fun () -> ignore (read_string server 16)) ;
                  Flow.close tcp ;
                  Flow.close server ;
                  Net.close l ) ) ) ]

let () = exit (Windtrap.run "tls" [engine_tests; flow_tests])
