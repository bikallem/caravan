(* The tests of caravan.middleware. Most tests call a middleware with a request
   on a Conn_info with a chosen peer, with no server. The tests of real
   connections use Caravan_testing. *)

open Windtrap
open Caravan
module F = Caravan_middleware.Forwarded
module Id = Caravan_middleware.Request_id
module T = Caravan_testing

let is_invalid_arg = function Invalid_argument _ -> true | _ -> false

let show_node = function
  | F.Ip a ->
      "Ip " ^ a
  | Unix ->
      "Unix"
  | Unknown ->
      "Unknown"

let conn peer =
  Conn_info.make ~id:7 ~peer ~local:(Weft.Net.Tcp ("10.0.0.9", 80)) ()

let tcp ip = Weft.Net.Tcp (ip, 4000)

let xff = "X-Forwarded-For"

let xfp = "X-Forwarded-Proto"

let xfh = "X-Forwarded-Host"

let show_client req = Option.fold ~none:"None" ~some:show_node (F.client req)

(* [seen ~peer ~connected ~meth ~authority mw fields] is what a handler behind
   [mw] sees: the client, the scheme and the authority. The request has a
   connection from [peer] if [connected] (the default). *)
let seen ?(peer = tcp "10.0.0.1") ?(connected = true) ?(meth = Method.GET)
    ?scheme ?authority mw fields =
  let conn = if connected then Some (conn peer) else None in
  let req =
    Request.make ?conn ?scheme ?authority ~headers:(Headers.of_list fields) meth
      "/"
  in
  let out = ref "" in
  ignore
    (mw
       (fun req ->
         out :=
           String.concat " "
             [ show_client req
             ; Request.scheme req
             ; Option.value ~default:"-" (Request.authority req) ] ;
         Response.empty () )
       req ) ;
  !out

let trusted = F.trust_ips ["10.0.0.1"; "10.0.0.2"]

let forwarded_tests =
  group "Forwarded"
    [ test "a peer that is not trusted changes nothing" (fun () ->
          equal string "Ip 10.0.0.5 http a.example"
            (seen ~peer:(tcp "10.0.0.5") ~authority:"a.example"
               (F.middleware ~host:true ~trusted ())
               [(xff, "1.2.3.4"); (xfp, "https"); (xfh, "evil.example")] ) )
    ; test "a trusted peer gives the client, the scheme and the host" (fun () ->
          equal string "Ip 1.2.3.4 https a.example"
            (seen
               (F.middleware ~host:true ~trusted ())
               [(xff, "1.2.3.4"); (xfp, "https"); (xfh, "a.example")] ) )
    ; test "X-Forwarded-Proto http" (fun () ->
          equal string "Ip 1.2.3.4 http -"
            (seen ~scheme:"https" (F.middleware ~trusted ())
               [(xff, "1.2.3.4"); (xfp, "HTTP")] ) )
    ; test "X-Forwarded-Host is off by default" (fun () ->
          equal string "Ip 1.2.3.4 https orig"
            (seen ~authority:"orig" (F.middleware ~trusted ())
               [(xff, "1.2.3.4"); (xfp, "https"); (xfh, "evil.example")] ) )
    ; test "the value of a client is to the left" (fun () ->
          let fields = [(xff, "6.6.6.6, 1.2.3.4")] in
          equal string "Ip 1.2.3.4 http -"
            (seen (F.middleware ~trusted ()) fields) ;
          (* 1.2.3.4 is not trusted, thus the walk stops there. *)
          equal string "Ip 1.2.3.4 http -"
            (seen (F.middleware ~limit:2 ~trusted ()) fields) )
    ; test "the limit of hops" (fun () ->
          let fields = [(xff, "1.2.3.4, 10.0.0.1")] in
          equal string "Ip 1.2.3.4 http -"
            (seen ~peer:(tcp "10.0.0.2")
               (F.middleware ~limit:2 ~trusted ())
               fields ) ;
          equal string "Ip 10.0.0.1 http -"
            (seen ~peer:(tcp "10.0.0.2") (F.middleware ~trusted ()) fields) )
    ; test "the values align from the right" (fun () ->
          let mw = F.middleware ~limit:2 ~trusted () in
          (* The proxy 10.0.0.1 got https from the client. 10.0.0.2 has no proto
             value: it keeps https. *)
          equal string "Ip 1.2.3.4 https -"
            (seen ~peer:(tcp "10.0.0.2") mw
               [(xff, "1.2.3.4, 10.0.0.1"); (xfp, "https")] ) ;
          (* The empty proto of 1.2.3.4 makes the client Unknown. *)
          equal string "Unknown https -"
            (seen ~peer:(tcp "10.0.0.2") mw
               [(xff, "1.2.3.4, 10.0.0.1"); (xfp, ", https")] ) )
    ; test "a later element keeps or replaces the host" (fun () ->
          let mw = F.middleware ~host:true ~limit:2 ~trusted () in
          let peer = tcp "10.0.0.2" in
          let fors = (xff, "1.2.3.4, 10.0.0.1") in
          equal string "Ip 1.2.3.4 http a.example"
            (seen ~peer mw [fors; (xfh, "a.example")]) ;
          equal string "Ip 1.2.3.4 http b.example"
            (seen ~peer mw [fors; (xfh, "b.example, a.example")]) )
    ; test "a bad value is Unknown, not the peer" (fun () ->
          equal string "Unknown http -"
            (seen (F.middleware ~trusted ()) [(xff, "garbage")]) ;
          equal string "Unknown http -"
            (seen
               (F.middleware ~limit:2
                  ~trusted:(F.trust_ips ["10.0.0.1"; "1.2.3.4"])
                  () )
               [(xff, "garbage, 1.2.3.4")] ) )
    ; test "unknown stops the walk, also when each node is trusted" (fun () ->
          equal string "Unknown http -"
            (seen
               (F.middleware ~limit:5 ~trusted:(fun _ -> true) ())
               [(xff, "6.6.6.6, unknown, 1.2.3.4")] ) )
    ; test "a bad proto or host gives Unknown, never the trusted proxy"
        (fun () ->
          let mw = F.middleware ~host:true ~limit:2 ~trusted () in
          let peer = tcp "10.0.0.2" in
          let fors = (xff, "1.2.3.4, 10.0.0.1") in
          equal string "Unknown https -"
            (seen ~peer mw [fors; (xfp, "ftp, https")]) ;
          equal string "Unknown http b.example"
            (seen ~peer mw [fors; (xfh, "evil.com/x, b.example")]) ;
          equal string "Unknown http b.example"
            (seen ~peer mw [fors; (xfh, "a@b, b.example")]) ;
          (* The first element: a proxy that sets only X-Forwarded-For, and a
             client that sends its own X-Forwarded-Proto. *)
          let mw = F.middleware ~host:true ~trusted () in
          List.iter
            (fun f ->
              equal string "Unknown http -" (seen mw [(xff, "1.2.3.4"); f]) )
            [(xfp, "x"); (xfp, ""); (xfh, "")] )
    ; test "the normal form of the nodes" (fun () ->
          List.iter
            (fun (v, expected) ->
              equal ~msg:v string expected
                (seen (F.middleware ~trusted ()) [(xff, v)]) )
            [ ("[2001:DB8::1]:4711", "Ip 2001:db8::1 http -")
            ; ("2001:db8::1", "Ip 2001:db8::1 http -")
            ; ("192.0.2.43:80", "Ip 192.0.2.43 http -")
            ; ("::ffff:10.0.0.3", "Ip 10.0.0.3 http -")
            ; ("[::1]:x", "Unknown http -")
            ; ("1.2.3.256", "Unknown http -")
            ; ("", "Unknown http -")
            ; ("1.2.3.255", "Ip 1.2.3.255 http -")
            ; ("::1", "Ip ::1 http -")
            ; ("1.2.3.4:x", "Unknown http -")
            ; ("[::1", "Unknown http -")
            ; ( "0000:0000:0000:0000:0000:abcd:255.255.255.255"
              , "Ip ::abcd:ffff:ffff http -" )
            ; (":::", "Unknown http -")
            ; ("::::::::::", "Unknown http -")
            ; ("1.2.3.4:80:90", "Unknown http -")
            ; ("1.2.3.4::", "Unknown http -")
            ; ("::ffff:1.2.3.4:80", "Unknown http -")
            ; ("fe80::1%eth0", "Unknown http -")
            ; ("[fe80::1%25eth0]:1", "Unknown http -")
            ; ("1.2.3.04", "Unknown http -")
            ; ("0.0.0.0", "Ip 0.0.0.0 http -")
            ; ("[::FFFF:1.2.3.4]:80", "Ip 1.2.3.4 http -")
            ; ("2001:DB8:0:0:0:0:0:1", "Ip 2001:db8::1 http -")
            ; ("2001:db8:0:0:1:0:0:1", "Ip 2001:db8::1:0:0:1 http -")
            ; ("1:0:2:3:4:5:6:7", "Ip 1:0:2:3:4:5:6:7 http -")
            ; ("1:2:3:4:5:6:7:8", "Ip 1:2:3:4:5:6:7:8 http -")
            ; ("1:2:3:4:5:6:7:8:9", "Unknown http -")
            ; ("1:2:3:4:5:6:7::8", "Unknown http -")
            ; ("12345::1", "Unknown http -")
            ; ("1:2:3:4:5:6:7:", "Unknown http -")
            ; ("1::3:4:5:6:7:8", "Ip 1:0:3:4:5:6:7:8 http -")
            ; ("::g", "Unknown http -")
            ; ( "0000:0000:0000:0000:0000:abcd:255.255.255.2555"
              , "Unknown http -" ) ] ;
          equal string "Ip 1.2.3.4 http -"
            (seen ~peer:(tcp "::FFFF:10.0.0.1") (F.middleware ~trusted ())
               [(xff, "1.2.3.4")] ) )
    ; test "a trusted peer with no X-Forwarded-For changes nothing" (fun () ->
          equal string "Ip 10.0.0.1 http -"
            (seen (F.middleware ~trusted ()) [(xfp, "https")]) )
    ; test "trust_ips trusts only IP nodes" (fun () ->
          is_false (F.trust_ips ["10.0.0.1"] F.Unix) ;
          is_false (F.trust_ips ["10.0.0.1"] F.Unknown) ;
          is_true (F.trust_ips ["::FFFF:10.0.0.1"] (F.Ip "10.0.0.1")) ;
          is_true (F.trust_ips ["2001:db8:0::1"] (F.Ip "2001:db8::1")) )
    ; test "trusted never gets Unknown" (fun () ->
          let unknown = ref false in
          let trusted n =
            if n = F.Unknown then unknown := true ;
            true
          in
          ignore
            (seen
               (F.middleware ~limit:3 ~trusted ())
               [(xff, "1.2.3.4, x, 5.6.7.8")] ) ;
          ignore
            (seen
               ~peer:(Weft.Net.Tcp ("", 0))
               (F.middleware ~trusted ())
               [(xff, "1.2.3.4")] ) ;
          is_false !unknown )
    ; test "the middleware does not change the fields" (fun () ->
          let fields = [(xff, "1.2.3.4"); (xfp, "https"); (xfh, "a.example")] in
          let req =
            Request.make
              ~conn:(conn (tcp "10.0.0.1"))
              ~headers:(Headers.of_list fields) Method.GET "/"
          in
          let got = ref [] in
          ignore
            (F.middleware ~host:true ~trusted ()
               (fun req ->
                 got := Headers.to_list (Request.headers req) ;
                 Response.empty () )
               req ) ;
          equal (list (pair string string)) fields !got )
    ; test "two field lines join in order" (fun () ->
          let fields = [(xff, "1.2.3.4"); (xff, "10.0.0.1")] in
          let peer = tcp "10.0.0.2" in
          equal string "Ip 10.0.0.1 http -"
            (seen ~peer (F.middleware ~trusted ()) fields) ;
          equal string "Ip 1.2.3.4 http -"
            (seen ~peer (F.middleware ~limit:2 ~trusted ()) fields) )
    ; test "a Unix socket peer and the empty peer" (fun () ->
          equal string "Ip 1.2.3.4 http -"
            (seen ~peer:(Weft.Net.Unix "/run/s")
               (F.middleware ~trusted:(( = ) F.Unix) ())
               [(xff, "1.2.3.4")] ) ;
          equal string "Unknown http -"
            (seen
               ~peer:(Weft.Net.Tcp ("", 0))
               (F.middleware ~trusted:(fun _ -> true) ())
               [(xff, "1.2.3.4")] ) )
    ; test "CONNECT keeps its authority" (fun () ->
          equal string "Ip 1.2.3.4 http t.example:443"
            (seen ~meth:Method.CONNECT ~authority:"t.example:443"
               (F.middleware ~host:true ~trusted ())
               [(xff, "1.2.3.4"); (xfh, "a.example")] ) )
    ; test "a request with no connection, and a bad limit" (fun () ->
          equal string "None http -"
            (seen ~connected:false
               (F.middleware ~trusted:(fun _ -> true) ())
               [(xff, "1.2.3.4")] ) ;
          raises_match is_invalid_arg (fun () ->
              F.middleware ~limit:0 ~trusted () ) )
    ; test "through a real connection of the simulator" (fun () ->
          (* The simulator gives the client the peer address "client". *)
          let trusted = function F.Ip "client" -> true | _ -> false in
          let handler =
            Handler.compose
              [F.middleware ~trusted ()]
              (fun req -> Response.string (show_client req))
          in
          let r =
            T.run handler (fun t ->
                T.fetch t
                  (Request.make
                     ~headers:(Headers.of_list [(xff, "1.2.3.4")])
                     Method.GET "/" ) )
          in
          equal string "Ip 1.2.3.4" r.body ) ]

let run_id mw ?(upgrade = false) fields =
  let req =
    Request.make
      ~conn:(conn (tcp "10.0.0.1"))
      ~headers:(Headers.of_list fields) Method.GET "/"
  in
  let saw = ref ("", "") in
  let r =
    mw
      (fun req ->
        saw :=
          ( Option.value ~default:"none" (Id.find req)
          , Option.value ~default:"none"
              (Headers.find "x-request-id" (Request.headers req)) ) ;
        if upgrade then Response.upgrade ignore else Response.empty () )
      req
  in
  let found, field = !saw in
  if found <> field then
    fail "the attribute and the field of the request differ" ;
  (found, Headers.find "x-request-id" (Response.headers r))

let request_id_tests =
  group "Request_id"
    [ test "a new ID from the counter" (fun () ->
          let mw = Id.middleware () in
          equal (pair string (option string)) ("0", Some "0") (run_id mw []) ;
          equal (pair string (option string)) ("1", Some "1") (run_id mw []) )
    ; test "the clean form of an incoming ID" (fun () ->
          let mw = Id.middleware () in
          let id v = fst (run_id mw [("X-Request-Id", v)]) in
          equal string "ab-1_Z@x" (id "ab-1_Z@x") ;
          equal string "abscript" (id "a b<script>") ;
          equal string (String.make 255 'a') (id (String.make 300 'a')) ;
          equal string "0" (id "!!!") )
    ; test "accept" (fun () ->
          let mw = Id.middleware ~accept:(fun _ -> false) () in
          equal string "0" (fst (run_id mw [("X-Request-Id", "abc")])) )
    ; test "the field" (fun () ->
          let mw = Id.middleware ~field:"X-Trace" () in
          let r =
            mw (fun _ -> Response.empty ()) (Request.make Method.GET "/")
          in
          equal (option string) (Some "0")
            (Headers.find "x-trace" (Response.headers r)) ;
          let r =
            mw
              (fun req ->
                Response.string (Option.value ~default:"none" (Id.find req)) )
              (Request.make
                 ~headers:(Headers.of_list [("X-Trace", "abc")])
                 Method.GET "/" )
          in
          equal (option string) (Some "abc")
            (Headers.find "x-trace" (Response.headers r)) ;
          raises_match is_invalid_arg (fun () ->
              Id.middleware ~field:"bad name" () ) )
    ; test "the clean form of make" (fun () ->
          let mw = Id.middleware ~make:(fun _ -> "a\r\nb") () in
          equal string "ab" (fst (run_id mw [])) ;
          let mw = Id.middleware ~make:(fun _ -> "") () in
          equal string "0" (fst (run_id mw [])) ;
          equal string "1" (fst (run_id mw [])) )
    ; test "an upgrade response has the field" (fun () ->
          equal (option string) (Some "0")
            (snd (run_id (Id.middleware ()) ~upgrade:true [])) )
    ; test "each middleware has its own counter" (fun () ->
          let a = Id.middleware () and b = Id.middleware () in
          ignore (run_id a []) ;
          equal string "0" (fst (run_id b [])) )
    ; test "100 requests at the same time on 4 domains" (fun () ->
          let handler =
            Handler.compose
              [Id.middleware ()]
              (fun req -> Response.string (Option.get (Id.find req)))
          in
          let bodies =
            T.run ~domains:4 handler (fun t ->
                Weft.Fiber.scope (fun s ->
                    List.init 100 (fun _ ->
                        Weft.Fiber.fork_promise s (fun () -> (T.get t "/").body) )
                    |> List.map Weft.Promise.await ) )
          in
          equal int 100 (List.length (List.sort_uniq compare bodies)) ) ]

let () = exit (Windtrap.run "middleware" [forwarded_tests; request_id_tests])
