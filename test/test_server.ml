(* Integration tests: the server over real sockets, used as clients do. *)

open Windtrap
open Affect
open Affect_http
open Server_support

let connect server =
  let flow = Tcp.connect (Server.local_addr server) in
  (flow, Buf_reader.of_flow flow)

let get ?(conn = None) path = serialize { meth = "GET"; path; v10 = false; conn; body = No_body }

let integration_tests =
  group "Server over sockets"
    [ test "a burst of pipelined requests in one write" (fun () ->
          with_server @@ fun server ->
          let flow, r = connect server in
          Flow.write_string flow (get "/hello" ^ get "/empty" ^ get ~conn:(Some "close") "/stream");
          let responses = List.init 3 (fun _ -> read_response ~head:false r) in
          equal (list string) [ {|200 "hello"|}; {|204 ""|}; {|200 "abcd" close|} ] responses;
          Flow.close flow);
      test "a chunked upload in small delayed pieces is echoed" (fun () ->
          with_server @@ fun server ->
          let flow, r = connect server in
          let data = String.init 3000 (fun i -> Char.chr (65 + (i mod 26))) in
          Flow.write_string flow "POST /echo HTTP/1.1\r\nHost: x\r\nTransfer-Encoding: chunked\r\n\r\n";
          let rec send i =
            if i < String.length data
            then begin
              let n = Int.min 97 (String.length data - i) in
              let chunk = Printf.sprintf "%x\r\n%s\r\n" n (String.sub data i n) in
              (* Split each chunk across writes too. *)
              let half = String.length chunk / 2 in
              Flow.write_string flow (String.sub chunk 0 half);
              Mtime.wait_for Mtime.Span.(1 * ms);
              Flow.write_string flow (String.sub chunk half (String.length chunk - half));
              send (i + n)
            end
          in
          send 0;
          Flow.write_string flow "0\r\n\r\n";
          equal string (summary 200 data false) (read_response ~head:false r);
          Flow.close flow);
      test "8 MiB echoed while it uploads" (fun () ->
          with_server @@ fun server ->
          let flow, r = connect server in
          let data = String.init (8 * 1024 * 1024) (fun i -> Char.chr (i land 0xff)) in
          let upload =
            Fun.Async.call (fun () ->
                Flow.write_string flow
                  (Printf.sprintf "POST /echo HTTP/1.1\r\nHost: x\r\nContent-Length: %d\r\n\r\n"
                     (String.length data));
                Flow.write_string flow data)
          in
          let response = read_response ~head:false r in
          Fun.Async.get upload;
          is_true ~msg:"echoed exactly" (String.equal response (summary 200 data false));
          Flow.close flow);
      test "a streamed response arrives as it is produced" (fun () ->
          with_server @@ fun server ->
          let flow, r = connect server in
          Flow.write_string flow (get "/drip");
          let clock = Mtime.counter () in
          let rec header () = if Buf_reader.read_line r ~max:1000 <> "" then header () in
          header ();
          (* Chunks: size line, data line. Note when each data byte arrives. *)
          let arrivals =
            List.init 3 (fun _ ->
                ignore (Buf_reader.read_line r ~max:100);
                let data = Buf_reader.read_line r ~max:100 in
                (data, Mtime.Span.to_float_ns (Mtime.count clock) /. 1e6))
          in
          equal (list string) [ "1"; "2"; "3" ] (List.map fst arrivals);
          let times = List.map snd arrivals in
          is_true ~msg:(String.concat ", " (List.map (Printf.sprintf "%.0fms") times))
            (List.nth times 1 -. List.nth times 0 > 60. && List.nth times 2 -. List.nth times 1 > 60.);
          Flow.close flow);
      test "clients that leave mid-request do not disturb others" (fun () ->
          with_server @@ fun server ->
          let a, _ = connect server in
          Flow.write_string a "GET /hello HTTP/1.1\r\nHo";
          Flow.close a;
          let b, _ = connect server in
          Flow.write_string b "POST /echo HTTP/1.1\r\nHost: x\r\nContent-Length: 10\r\n\r\nabc";
          Flow.close b;
          let c, r = connect server in
          Flow.write_string c (get "/hello");
          equal string {|200 "hello"|} (read_response ~head:false r);
          Flow.close c) ]

let () = exit (run "server" [ integration_tests ])
