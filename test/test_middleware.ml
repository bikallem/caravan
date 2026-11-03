(* The tests of caravan.middleware. Most tests call a middleware with a request
   on a Conn_info with a chosen peer, with no server. The tests of real
   connections use Caravan_testing. *)

open Windtrap
open Caravan
module F = Caravan_middleware.Forwarded
module Id = Caravan_middleware.Request_id
module T = Caravan_testing

let status = Testable.make ~pp:Status.pp ~equal:Status.equal

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

(* Access_log and Error_page *)

module L = Caravan_middleware.Access_log
module P = Caravan_middleware.Error_page

let show_outcome = function
  | L.Complete ->
      "Complete"
  | Incomplete ->
      "Incomplete"
  | Failed e ->
      "Failed " ^ Printexc.to_string e
  | Cancelled ->
      "Cancelled"
  | Not_sent ->
      "Not_sent"
  | Upgraded ->
      "Upgraded"

(* [show e] is the status, the bytes and the outcome of [e]. *)
let show (e : L.entry) =
  Printf.sprintf "%s %d %s"
    (Option.fold ~none:"-"
       ~some:(fun s -> string_of_int (Status.code s))
       e.status )
    e.bytes (show_outcome e.outcome)

(* [logger ()] is a [log] that keeps the entries, and fills [logged] at each
   entry. *)
type logger =
  {log: L.entry -> unit; entries: unit -> L.entry list; logged: unit Weft.Ivar.t}

let logger () =
  let all = Atomic.make [] and logged = Weft.Ivar.create () in
  let rec push e =
    let l = Atomic.get all in
    if not (Atomic.compare_and_set all l (e :: l)) then push e
  in
  { log=
      (fun e ->
        push e ;
        ignore (Weft.Ivar.try_fill logged ()) )
  ; entries= (fun () -> List.rev (Atomic.get all))
  ; logged }

let shown l = List.map show (l.entries ())

let with_log ?(outer = []) ?(inner = []) l handler =
  Handler.compose (outer @ [L.middleware ~log:l.log ()] @ inner) handler

let reply s _ = Response.string s

(* The client error of a response that the server cuts short. *)
let tolerant f = try ignore (f ()) with Caravan_client.Error _ -> ()

let failed_with prefix = function
  | [e] ->
      String.starts_with ~prefix e
  | _ ->
      false

let errors_are names t =
  equal (list string) names
    (List.map (fun (e, _) -> Printexc.to_string e) (T.take_errors t))

(* [waits ~cleanup waiting] is a handler that fills [waiting], then sleeps for 1
   h. With [cleanup], a cancel of the sleep raises [Failure cleanup]. *)
let waits ?cleanup waiting _ =
  ignore (Weft.Ivar.try_fill waiting ()) ;
  match Weft.Time.sleep Mono (Weft.Span.s 3600) with
  | () ->
      Response.empty ()
  | exception (Weft.Fiber.Cancelled _ as e) -> (
    match cleanup with Some m -> failwith m | None -> raise e )

(* [leave_after waiting t] starts a request ([T.get t "/"] by default), waits
   for [waiting], then cancels the request: the client closes the connection. *)
let leave_after ?(request = fun t -> ignore (T.get t "/")) waiting t =
  Weft.Fiber.scope (fun s ->
      let p = Weft.Fiber.fork_promise s (fun () -> request t) in
      Weft.sync (Weft.Ivar.read waiting) ;
      Weft.Promise.cancel p )

(* A POST with a chunked body of 10 bytes, more than [max_body = 4]. *)
let too_large () =
  Request.make
    ~body:(Body.of_writer (fun w -> Body.Writer.write_string w "0123456789"))
    Method.POST "/"

let small_body = {Config.default with max_body= Some 4}

let access_log_tests =
  group "Access_log"
    [ test "a GET" (fun () ->
          let l = logger () in
          let slow _ =
            Weft.Time.sleep Mono (Weft.Span.s 2) ;
            Response.string "hello"
          in
          ignore (T.run (with_log l slow) (fun t -> T.get t "/a?q=1")) ;
          match l.entries () with
          | [e] ->
              equal string "200 5 Complete" (show e) ;
              equal string "/a?q=1" e.target ;
              is_true (Weft.Span.compare e.duration (Weft.Span.s 2) >= 0)
          | _ ->
              fail "one entry" )
    ; test "a POST that reads its body" (fun () ->
          let l = logger () in
          let echo req = Response.string (Body.to_string (Request.body req)) in
          ignore
            (T.run (with_log l echo) (fun t ->
                 T.fetch t
                   (Request.make ~body:(Body.of_string "abc") Method.POST "/") )
            ) ;
          equal (list string) ["200 3 Complete"] (shown l) )
    ; test "a response that an outer middleware drops" (fun () ->
          let l = logger () in
          let drop : Handler.middleware =
           fun next req ->
            ( match Response.kind (next req) with
            | Response.Body b ->
                Body.close b
            | Upgrade _ ->
                () ) ;
            Response.string "other"
          in
          ignore
            (T.run
               (with_log ~outer:[drop] l (reply "hi"))
               (fun t -> T.get t "/") ) ;
          equal (list string) ["200 0 Not_sent"] (shown l) )
    ; test "a body that the stop of the server cancels" (fun () ->
          let l = logger () in
          let h _ =
            Response.stream (fun w ->
                Body.Writer.write_string w "x" ;
                Body.Writer.flush w ;
                Weft.Time.sleep Mono (Weft.Span.s 3600) )
          in
          T.run (with_log l h) (fun t ->
              ignore (T.request t (Request.make Method.GET "/")) ) ;
          match l.entries () with
          | [e] ->
              equal string "Cancelled" (show_outcome e.outcome)
          | _ ->
              fail "one entry" )
    ; test "a request body that is too large after the head" (fun () ->
          let l = logger () in
          let config = {Config.default with max_body= Some 12} in
          let h req = Response.make (Request.body req) in
          T.run ~config (with_log l h) (fun t ->
              tolerant (fun () ->
                  T.fetch t
                    (Request.make
                       ~body:
                         (Body.of_writer (fun w ->
                              Body.Writer.write_string w "ab" ;
                              Body.Writer.flush w ;
                              Weft.Time.sleep Mono (Weft.Span.ms 10) ;
                              Body.Writer.write_string w "cdefgh" ) )
                       Method.POST "/" ) ) ) ;
          is_true
            ~msg:(String.concat ";" (shown l))
            (failed_with "200 2 Failed" (shown l)) )
    ; test "a response that streams a request body that is cut short" (fun () ->
          let l = logger () in
          let req =
            Request.make
              ~body:(Body.of_source (fun _ _ _ -> raise Body.Truncated))
              Method.POST "/"
          in
          Weft_sim.run (fun _ ->
              let r =
                L.middleware ~log:l.log ()
                  (fun req -> Response.make (Request.body req))
                  req
              in
              match Response.kind r with
              | Response.Body b ->
                  raises Body.Truncated (fun () -> ignore (Body.to_string b))
              | Upgrade _ ->
                  fail "an upgrade" ) ;
          (* No byte went: the server sends nothing. *)
          equal (list string) ["- 0 Cancelled"] (shown l) )
    ; test "a request body that is truncated" (fun () ->
          let l = logger () and seen = ref 0 in
          let req () =
            Request.make
              ~body:(Body.of_source (fun _ _ _ -> raise Body.Truncated))
              Method.POST "/"
          in
          let read req = Response.string (Body.to_string (Request.body req)) in
          Weft_sim.run (fun _ ->
              raises Body.Truncated (fun () ->
                  L.middleware ~log:l.log () read (req ()) ) ;
              raises Body.Truncated (fun () ->
                  P.middleware ~report:(fun _ _ _ -> incr seen) () read (req ()) ) ) ;
          equal (list string) ["- 0 Cancelled"] (shown l) ;
          equal int 0 !seen )
    ; test "responses with no body" (fun () ->
          let l = logger () in
          let h req =
            match Request.path req with
            | "/204" ->
                Response.string ~status:Status.no_content "x"
            | "/205" ->
                Response.string ~status:(Status.v 205) "x"
            | "/304" ->
                Response.string ~status:Status.not_modified "x"
            | _ ->
                Response.string "hello"
          in
          T.run (with_log l h) (fun t ->
              ignore (T.fetch t (Request.make Method.HEAD "/")) ;
              List.iter (fun p -> ignore (T.get t p)) ["/204"; "/205"; "/304"] ) ;
          equal (list string)
            [ "200 0 Complete"
            ; "204 0 Complete"
            ; "205 0 Complete"
            ; "304 0 Complete" ]
            (shown l) )
    ; test "a 2xx body to CONNECT" (fun () ->
          let l = logger () in
          let h req =
            match Headers.find "x-status" (Request.headers req) with
            | Some "204" ->
                Response.string ~status:Status.no_content "x"
            | _ ->
                Response.string "x"
          in
          T.run (with_log l h) (fun t ->
              List.iter
                (fun st ->
                  tolerant (fun () ->
                      T.fetch t
                        (Request.make ~authority:"127.0.0.1:80"
                           ~headers:(Headers.of_list [("X-Status", st)])
                           Method.CONNECT "" ) ) )
                ["200"; "204"] ;
              equal int 2 (List.length (T.take_errors t)) ) ;
          equal (list string) ["500 0 Not_sent"; "500 0 Not_sent"] (shown l) )
    ; test "a body shorter than its length" (fun () ->
          let l = logger () in
          let h _ =
            Response.stream ~length:5 (fun w ->
                Body.Writer.write_string w "abc" )
          in
          T.run (with_log l h) (fun t ->
              tolerant (fun () -> T.get t "/") ;
              errors_are
                ["response body length differs from its Content-Length"]
                t ) ;
          equal (list string) ["200 3 Incomplete"] (shown l) )
    ; test "a streamed body that raises" (fun () ->
          let l = logger () in
          let h _ =
            Response.stream (fun w ->
                Body.Writer.write_string w "abc" ;
                Body.Writer.flush w ;
                failwith "body" )
          in
          T.run (with_log l h) (fun t ->
              tolerant (fun () -> T.get t "/") ;
              errors_are [{|Failure("body")|}] t ) ;
          equal (list string) [{|200 3 Failed Failure("body")|}] (shown l) ;
          (* Before the first byte, the server sends its 500, but the entry has
             the status of the handler. *)
          let l = logger () in
          T.run
            (with_log l (fun _ -> Response.stream (fun _ -> failwith "early")))
            (fun t ->
              equal status Status.internal_server_error (T.get t "/").status ;
              errors_are [{|Failure("early")|}] t ) ;
          equal (list string) [{|200 0 Failed Failure("early")|}] (shown l) )
    ; test "a handler that raises" (fun () ->
          let l = logger () in
          let r =
            T.run
              (with_log l (fun _ -> failwith "x"))
              (fun t ->
                let r = T.get t "/" in
                errors_are [{|Failure("x")|}] t ;
                r )
          in
          equal status Status.internal_server_error r.status ;
          equal (list string) [{|500 0 Failed Failure("x")|}] (shown l) )
    ; test "a request body that is too large" (fun () ->
          let l = logger () in
          let config = small_body in
          let h req = Response.string (Body.to_string (Request.body req)) in
          let r =
            T.run ~config (with_log l h) (fun t -> T.fetch t (too_large ()))
          in
          equal status Status.content_too_large r.status ;
          is_true
            ~msg:(String.concat ";" (shown l))
            (failed_with "413 0 Failed" (shown l)) )
    ; test "Body.Truncated of the handler, with a fine request body" (fun () ->
          let l = logger () in
          T.run
            (with_log l (fun _ -> raise Body.Truncated))
            (fun t ->
              equal status Status.internal_server_error (T.get t "/").status ;
              errors_are ["Caravan__Body.Truncated"] t ) ;
          is_true (failed_with "500 0 Failed" (shown l)) )
    ; test "the client leaves before the response" (fun () ->
          let l = logger () and waiting = Weft.Ivar.create () in
          T.run
            (with_log l (waits waiting))
            (fun t ->
              leave_after waiting t ;
              Weft.sync (Weft.Ivar.read l.logged) ;
              errors_are [] t ) ;
          equal (list string) ["- 0 Cancelled"] (shown l) )
    ; test "the client leaves during the body" (fun () ->
          let l = logger () in
          let started = Weft.Ivar.create () in
          let h _ =
            Response.stream (fun w ->
                let rec loop () =
                  Body.Writer.write_string w "x" ;
                  Body.Writer.flush w ;
                  ignore (Weft.Ivar.try_fill started ()) ;
                  Weft.Time.sleep Mono (Weft.Span.ms 10) ;
                  loop ()
                in
                loop () )
          in
          T.run (with_log l h) (fun t ->
              leave_after started t ~request:(fun t ->
                  T.with_response t (Request.make Method.GET "/") (fun _ ->
                      Weft.Time.sleep Mono (Weft.Span.s 3600) ) ) ;
              Weft.sync (Weft.Ivar.read l.logged) ;
              errors_are [] t ) ;
          match l.entries () with
          | [e] ->
              equal (option int) (Some 200) (Option.map Status.code e.status) ;
              equal string "Cancelled" (show_outcome e.outcome)
          | _ ->
              fail "one entry" )
    ; test "the cancel of a child scope of the handler is a failure" (fun () ->
          let l = logger () in
          let h _ =
            Weft.Fiber.scope (fun s ->
                Weft.Fiber.cancel s Exit ;
                Weft.Fiber.yield () ;
                Response.empty () )
          in
          T.run (with_log l h) (fun t ->
              equal status Status.internal_server_error (T.get t "/").status ;
              equal int 1 (List.length (T.take_errors t)) ) ;
          is_true (failed_with "500 0 Failed" (shown l)) )
    ; test "a cleanup that raises after the client left" (fun () ->
          let l = logger () and waiting = Weft.Ivar.create () in
          T.run
            (with_log l (waits ~cleanup:"cleanup" waiting))
            (fun t ->
              leave_after waiting t ;
              Weft.sync (Weft.Ivar.read l.logged) ;
              errors_are [{|Failure("cleanup")|}] t ) ;
          equal (list string) ["- 0 Cancelled"] (shown l) )
    ; test "upgrades" (fun () ->
          let l = logger () in
          ignore
            (Weft_sim.run (fun _ ->
                 L.middleware ~log:l.log ()
                   (fun _ -> Response.upgrade ignore)
                   (Request.make Method.GET "/") ) ) ;
          T.run
            (with_log l (fun _ -> Response.upgrade ignore))
            (fun t ->
              equal status Status.internal_server_error (T.get t "/").status ;
              equal int 1 (List.length (T.take_errors t)) ) ;
          equal (list string) ["101 0 Upgraded"; "101 0 Upgraded"] (shown l) )
    ; test "log runs in a shield" (fun () ->
          let l = logger () and waiting = Weft.Ivar.create () in
          let log e =
            Weft.Time.sleep Mono Weft.Span.zero ;
            l.log e
          in
          T.run
            (Handler.compose [L.middleware ~log ()] (waits waiting))
            (fun t ->
              leave_after waiting t ;
              Weft.sync (Weft.Ivar.read l.logged) ) ;
          equal (list string) ["- 0 Cancelled"] (shown l) )
    ; test "a route that raises" (fun () ->
          let l = logger () and closes = ref 0 in
          let h _ =
            Response.make
              (Body.of_source ~close:(fun () -> incr closes) (fun _ _ _ -> 0))
          in
          let mw =
            L.middleware ~route:(fun _ -> failwith "route") ~log:l.log ()
          in
          ignore (T.run (Handler.compose [mw] h) (fun t -> T.get t "/")) ;
          equal (list string) ["200 0 Complete"] (shown l) ;
          equal int 1 !closes ;
          match l.entries () with
          | [e] ->
              equal (option string) None e.route
          | _ ->
              fail "one entry" )
    ; test "a sink failure of the handler is not a request body error"
        (fun () ->
          let l = logger () in
          let h req =
            match Body.to_string ~max:1 (Request.body req) with
            | _ ->
                Response.empty ()
            | exception Body.Too_large ->
                raise Body.Truncated
          in
          T.run (with_log l h) (fun t ->
              equal status Status.internal_server_error
                (T.fetch t
                   (Request.make ~body:(Body.of_string "hello") Method.POST "/") )
                  .status ;
              errors_are ["Caravan__Body.Truncated"] t ) ;
          is_true (failed_with "500 0 Failed" (shown l)) )
    ; test "a response that streams a request body that is too large" (fun () ->
          let l = logger () in
          let h req = Response.make (Request.body req) in
          T.run ~config:small_body (with_log l h) (fun t ->
              tolerant (fun () -> T.fetch t (too_large ())) ) ;
          is_true
            ~msg:(String.concat ";" (shown l))
            (failed_with "413 " (shown l)) )
    ; test "an exception of log is dropped" (fun () ->
          let h =
            Handler.compose
              [L.middleware ~log:(fun _ -> failwith "log") ()]
              (reply "hi")
          in
          let r = T.run h (fun t -> T.get t "/") in
          equal string "hi" r.body )
    ; test "the client, the request ID and the route" (fun () ->
          let l = logger () in
          let trusted = function F.Ip "client" -> true | _ -> false in
          let router =
            Caravan_router.(handler [get "/users/:id" (reply "u")])
          in
          let h =
            Handler.compose
              [ F.middleware ~trusted ()
              ; Id.middleware ()
              ; L.middleware
                  ~route:(Response.find_attr Caravan_router.template)
                  ~log:l.log () ]
              router
          in
          T.run h (fun t ->
              ignore
                (T.fetch t
                   (Request.make
                      ~headers:(Headers.of_list [(xff, "1.2.3.4")])
                      Method.GET "/users/7" ) ) ) ;
          match l.entries () with
          | [e] ->
              equal string "Ip 1.2.3.4"
                (Option.fold ~none:"-" ~some:show_node e.client) ;
              equal (option string) (Some "0") e.request_id ;
              equal (option string) (Some "/users/:id") e.route
          | _ ->
              fail "one entry" )
    ; test "line" (fun () ->
          let e =
            { L.start= Weft.Time.posix_of_unix_s 0.
            ; duration= Weft.Span.us 1500
            ; meth= Method.GET
            ; target= "/a?b=c"
            ; version= Version.Http_1_1
            ; client= Some (F.Ip "1.2.3.4")
            ; request_id= Some "7"
            ; route= Some "/a"
            ; status= Some Status.ok
            ; bytes= 5
            ; outcome= Complete }
          in
          expect
            (String.concat "\n"
               [ L.line e
               ; L.line
                   { e with
                     client= None
                   ; request_id= None
                   ; route= None
                   ; status= None
                   ; outcome= Cancelled }
               ; L.line {e with outcome= Failed (Failure "a\nb")}
               ; L.line
                   { e with
                     meth= Method.CONNECT
                   ; target= "t.example:443"
                   ; outcome= Not_sent }
               ; L.line {e with target= "/a\"b"; client= Some F.Unix}
               ; L.line
                   { e with
                     client= Some F.Unknown
                   ; route= Some "a b"
                   ; outcome= Incomplete }
               ; L.line {e with route= Some "\x7f"; outcome= Upgraded} ] )
          @@ __POS_OF__
               {|1.2.3.4 7 "GET /a?b=c HTTP/1.1" 200 5 1.500ms /a
- - "GET /a?b=c HTTP/1.1" - 5 1.500ms - cancelled
1.2.3.4 7 "GET /a?b=c HTTP/1.1" 200 5 1.500ms /a failed "Failure(\"a\\nb\")"
1.2.3.4 7 "CONNECT t.example:443 HTTP/1.1" 200 5 1.500ms /a not-sent
unix 7 "GET "/a\"b" HTTP/1.1" 200 5 1.500ms /a
unknown 7 "GET /a?b=c HTTP/1.1" 200 5 1.500ms "a b" incomplete
1.2.3.4 7 "GET /a?b=c HTTP/1.1" 200 5 1.500ms "\127" upgraded|} )
    ; test "an outer middleware reads the template" (fun () ->
          let seen = ref None in
          let outer : Handler.middleware =
           fun next req ->
            let r = next req in
            seen := Response.find_attr Caravan_router.template r ;
            r
          in
          let l = logger () in
          let router = Caravan_router.(handler [get "/u/:id" (reply "u")]) in
          ignore
            (T.run (with_log ~outer:[outer] l router) (fun t -> T.get t "/u/1")) ;
          equal (option string) (Some "/u/:id") !seen ) ]

let error_page_tests =
  let reports () =
    let seen = ref [] in
    ( (fun _ e bt ->
        seen :=
          (Printexc.to_string e, Printexc.raw_backtrace_length bt) :: !seen )
    , seen )
  in
  let page _ e =
    Response.string ~status:Status.service_unavailable
      ("page " ^ Printexc.to_string e)
  in
  group "Error_page"
    [ test "a handler that raises gets the page" (fun () ->
          let report, seen = reports () in
          let h =
            Handler.compose
              [P.middleware ~report ~page ()]
              (fun _ -> failwith "x")
          in
          let r = T.run h (fun t -> T.get t "/") in
          equal status Status.service_unavailable r.status ;
          equal string {|page Failure("x")|} r.body ;
          equal (list string) [{|Failure("x")|}] (List.map fst !seen) )
    ; test "plain does not show the exception" (fun () ->
          let report, _ = reports () in
          let h =
            Handler.compose
              [P.middleware ~report ()]
              (fun _ -> failwith "secret")
          in
          let r = T.run h (fun t -> T.get t "/") in
          equal status Status.internal_server_error r.status ;
          equal string "Internal Server Error\n" r.body )
    ; test "page raises" (fun () ->
          let report, _ = reports () in
          let page _ _ = failwith "page" in
          let h =
            Handler.compose
              [P.middleware ~report ~page ()]
              (fun _ -> failwith "x")
          in
          T.run h (fun t ->
              equal status Status.internal_server_error (T.get t "/").status ;
              errors_are [{|Failure("page")|}] t ) )
    ; test "an exception of a body is not a page" (fun () ->
          let report, seen = reports () in
          let h =
            Handler.compose
              [P.middleware ~report ~page ()]
              (fun _ -> Response.stream (fun _ -> failwith "body"))
          in
          T.run h (fun t ->
              equal status Status.internal_server_error (T.get t "/").status ;
              errors_are [{|Failure("body")|}] t ) ;
          equal int 0 (List.length !seen) )
    ; test "a request body that is too large is not a page" (fun () ->
          let report, seen = reports () in
          let config = small_body in
          let h =
            Handler.compose
              [P.middleware ~report ~page ()]
              (fun req -> Response.string (Body.to_string (Request.body req)))
          in
          let r = T.run ~config h (fun t -> T.fetch t (too_large ())) in
          equal status Status.content_too_large r.status ;
          equal int 0 (List.length !seen) )
    ; test "cancels are not a page" (fun () ->
          let report, seen = reports () in
          let waiting = Weft.Ivar.create () and l = logger () in
          T.run
            (Handler.compose
               [L.middleware ~log:l.log (); P.middleware ~report ~page ()]
               (waits ~cleanup:"cleanup" waiting) )
            (fun t ->
              leave_after waiting t ;
              Weft.sync (Weft.Ivar.read l.logged) ;
              errors_are [{|Failure("cleanup")|}] t ) ;
          equal int 0 (List.length !seen) )
    ; test "the cancel of a child scope gets a page" (fun () ->
          let report, seen = reports () in
          let h =
            Handler.compose
              [P.middleware ~report ~page ()]
              (fun _ ->
                Weft.Fiber.scope (fun s ->
                    Weft.Fiber.cancel s Exit ;
                    Weft.Fiber.yield () ;
                    Response.empty () ) )
          in
          let r = T.run h (fun t -> T.get t "/") in
          equal status Status.service_unavailable r.status ;
          equal int 1 (List.length !seen) )
    ; test "Body.Truncated of the handler gets a page" (fun () ->
          let report, seen = reports () in
          let h =
            Handler.compose
              [P.middleware ~report ~page ()]
              (fun _ -> raise Body.Truncated)
          in
          let r = T.run h (fun t -> T.get t "/") in
          equal status Status.service_unavailable r.status ;
          equal int 1 (List.length !seen) )
    ; test "report raises" (fun () ->
          let report _ _ _ = failwith "report" in
          let h =
            Handler.compose
              [P.middleware ~report ~page ()]
              (fun _ -> failwith "x")
          in
          T.run h (fun t ->
              equal status Status.internal_server_error (T.get t "/").status ;
              errors_are [{|Failure("report")|}] t ) )
    ; test "the page keeps the connection" (fun () ->
          let report, _ = reports () in
          let h req =
            if Request.path req = "/fail" then failwith "x"
            else
              Response.string
                (string_of_int (Conn_info.id (Option.get (Request.conn req))))
          in
          let a, b =
            T.run
              (Handler.compose [P.middleware ~report ~page ()] h)
              (fun t ->
                let a = (T.get t "/").body in
                ignore (T.get t "/fail") ;
                (a, (T.get t "/").body) )
          in
          equal string a b )
    ; test "the access log sees the page" (fun () ->
          let report, _ = reports () and l = logger () in
          let h =
            Handler.compose
              [L.middleware ~log:l.log (); P.middleware ~report ~page ()]
              (fun _ -> failwith "x")
          in
          ignore (T.run h (fun t -> T.get t "/")) ;
          match shown l with
          | [s] ->
              is_true ~msg:s
                ( String.starts_with ~prefix:"503 " s
                && String.ends_with ~suffix:" Complete" s )
          | l ->
              fail (String.concat ";" l) ) ]

(* Cors *)

module C = Caravan_middleware.Cors

let field name r = Headers.find name (Response.headers r)

(* [cors ?meth ?fields mw h] is the response of [h] behind [mw] to a request
   with [fields], and [true] if [h] ran. *)
let cors ?(meth = Method.GET) ?(fields = []) mw h =
  let ran = ref false in
  let r =
    mw
      (fun req ->
        ran := true ;
        h req )
      (Request.make ~headers:(Headers.of_list fields) meth "/")
  in
  (r, !ran)

let origin o = ("Origin", o)

let preflight ?(fields = []) o m =
  [origin o; ("Access-Control-Request-Method", m)] @ fields

(* [preflight_to ~fields mw o m] is the response and the run flag of a preflight
   from the origin [o] for the method [m]. *)
let preflight_to ?fields mw o m =
  cors ~meth:Method.OPTIONS ~fields:(preflight ?fields o m) mw (reply "")

let preflight_vary =
  Some "Origin, Access-Control-Request-Method, Access-Control-Request-Headers"

let a = "https://a.example"

let cors_fields r =
  List.filter
    (fun (n, _) ->
      String.starts_with ~prefix:"access-control-" (String.lowercase_ascii n) )
    (Headers.to_list (Response.headers r))

(* [made mw] ignores the middleware [mw]: it is made only for its checks. *)
let made (_ : Handler.middleware) = ()

let cors_tests =
  let any = C.middleware ~origins:C.Any () in
  let only = C.middleware ~origins:(C.Only [a]) () in
  group "Cors"
    [ test "a request with no Origin" (fun () ->
          (* With Any, a cache can give the response to a CORS fetch: it has the
             constant fields, and no Vary. *)
          let r, ran = cors any (reply "hi") in
          is_true ran ;
          equal
            (list (pair string string))
            [("Access-Control-Allow-Origin", "*")]
            (cors_fields r) ;
          equal (option string) None (field "vary" r) ;
          let r, _ = cors only (reply "hi") in
          equal (list (pair string string)) [] (cors_fields r) ;
          equal (option string) (Some "Origin") (field "vary" r) )
    ; test "Any gives *" (fun () ->
          let r, _ = cors ~fields:[origin a] any (reply "hi") in
          equal (option string) (Some "*")
            (field "access-control-allow-origin" r) ;
          equal (option string) None (field "vary" r) )
    ; test "Only compares whole origins" (fun () ->
          let allowed o =
            field "access-control-allow-origin"
              (fst (cors ~fields:[origin o] only (reply "")))
          in
          equal (option string) (Some "https://A.example")
            (allowed "https://A.example") ;
          List.iter
            (fun o -> equal ~msg:o (option string) None (allowed o))
            [ "https://a.example.evil.com"
            ; "https://a.exampl"
            ; "http://a.example"
            ; "https://a.example:8443"
            ; "null" ] )
    ; test "Matching, and credentials" (fun () ->
          let mw =
            C.middleware
              ~origins:(C.Matching (String.ends_with ~suffix:".example"))
              ~credentials:true ()
          in
          let r, _ = cors ~fields:[origin a] mw (reply "") in
          equal (option string) (Some a) (field "access-control-allow-origin" r) ;
          equal (option string) (Some "true")
            (field "access-control-allow-credentials" r) ;
          equal (option string) (Some "Origin") (field "vary" r) ;
          let r, _ = cors ~fields:[origin "https://b.other"] mw (reply "") in
          equal (option string) None (field "access-control-allow-origin" r) )
    ; test "expose_headers" (fun () ->
          let mw =
            C.middleware ~origins:(C.Only [a]) ~expose_headers:["X-A"; "X-B"] ()
          in
          let r, _ = cors ~fields:[origin a] mw (reply "") in
          equal (option string) (Some "X-A, X-B")
            (field "access-control-expose-headers" r) ;
          let r, _ = preflight_to mw a "GET" in
          equal (option string) None (field "access-control-expose-headers" r) )
    ; test "the fields of the handler" (fun () ->
          let h _ =
            Response.string
              ~headers:(Headers.of_list [("Access-Control-Allow-Origin", "*")])
              ""
          in
          let r, _ = cors ~fields:[origin a] only h in
          equal
            (list (pair string string))
            [("Access-Control-Allow-Origin", a)]
            (cors_fields r) ;
          let r, _ = cors ~fields:[origin "https://evil.example"] only h in
          equal (list (pair string string)) [] (cors_fields r) )
    ; test "a preflight" (fun () ->
          let mw =
            C.middleware ~origins:(C.Only [a])
              ~allow_methods:Method.[GET; PUT]
              ()
          in
          let r, ran = preflight_to mw a "PUT" in
          is_false ran ;
          equal status Status.no_content (Response.status r) ;
          equal (option string) (Some a) (field "access-control-allow-origin" r) ;
          equal (option string) (Some "GET, PUT")
            (field "access-control-allow-methods" r) ;
          equal (option string) preflight_vary (field "vary" r) )
    ; test "refused preflights" (fun () ->
          let mw =
            C.middleware ~origins:(C.Only [a]) ~allow_methods:Method.[PUT] ()
          in
          List.iter
            (fun (label, fields) ->
              let r, ran = cors ~meth:Method.OPTIONS ~fields mw (reply "") in
              is_false ~msg:label ran ;
              equal ~msg:label status Status.no_content (Response.status r) ;
              equal ~msg:label (list (pair string string)) [] (cors_fields r) ;
              equal ~msg:label (option string) preflight_vary (field "vary" r) )
            [ ("DELETE", preflight a "DELETE")
            ; ( "a field"
              , preflight
                  ~fields:[("Access-Control-Request-Headers", "x-a")]
                  a "PUT" )
            ; ("an origin", preflight "https://evil.example" "PUT") ] )
    ; test "the safelisted methods" (fun () ->
          let mw =
            C.middleware ~origins:(C.Only [a])
              ~allow_methods:Method.[PUT]
              ~allow_headers:["x-a"] ()
          in
          let r, _ =
            preflight_to
              ~fields:[("Access-Control-Request-Headers", "x-a")]
              mw a "POST"
          in
          equal (option string) (Some a) (field "access-control-allow-origin" r) )
    ; test "a bad Access-Control-Request-Method" (fun () ->
          let mw =
            C.middleware ~origins:(C.Only [a]) ~allow_methods:Method.[PATCH] ()
          in
          List.iter
            (fun m ->
              let r, _ = preflight_to mw a m in
              equal ~msg:m (list (pair string string)) [] (cors_fields r) )
            ["GET, PUT"; ""; "patch"] ;
          let r, _ = preflight_to mw a "PATCH" in
          equal (option string) (Some a) (field "access-control-allow-origin" r) )
    ; test "Access-Control-Request-Headers" (fun () ->
          let mw =
            C.middleware ~origins:(C.Only [a]) ~allow_headers:["x-a"; "X-B"] ()
          in
          let r, _ =
            preflight_to
              ~fields:[("Access-Control-Request-Headers", "X-A ,, x-b")]
              mw a "GET"
          in
          equal (option string) (Some "X-A, x-b")
            (field "access-control-allow-headers" r) ;
          let r, _ =
            preflight_to
              ~fields:[("Access-Control-Request-Headers", "")]
              mw a "GET"
          in
          equal (option string) (Some a) (field "access-control-allow-origin" r) ;
          equal (option string) None (field "access-control-allow-headers" r) )
    ; test "allow_headers with *" (fun () ->
          let mw =
            C.middleware ~origins:(C.Only [a]) ~allow_headers:["x-a"; "*"] ()
          in
          let r, _ =
            preflight_to
              ~fields:[("Access-Control-Request-Headers", "x-z, authorization")]
              mw a "GET"
          in
          equal (option string) (Some "x-z, authorization")
            (field "access-control-allow-headers" r) )
    ; test "max_age and preflight_status" (fun () ->
          let mw =
            C.middleware ~origins:(C.Only [a]) ~max_age:600
              ~preflight_status:Status.ok ()
          in
          let r, _ = preflight_to mw a "GET" in
          equal (option string) (Some "600") (field "access-control-max-age" r) ;
          equal status Status.ok (Response.status r) )
    ; test "an OPTIONS that is not a preflight" (fun () ->
          let r, ran =
            cors ~meth:Method.OPTIONS ~fields:[origin a] only (reply "o")
          in
          is_true ran ;
          equal (option string) (Some a) (field "access-control-allow-origin" r) )
    ; test "Vary" (fun () ->
          let vary_reply v _ =
            Response.string ~headers:(Headers.of_list [("Vary", v)]) ""
          in
          let vary v =
            field "vary" (fst (cors ~fields:[origin a] only (vary_reply v)))
          in
          equal (option string) (Some "Accept-Encoding, Origin")
            (vary "Accept-Encoding") ;
          equal (option string) (Some "origin") (vary "origin") ;
          equal (option string) (Some "*") (vary "*") ;
          let r, _ =
            cors ~fields:[origin a] only (vary_reply "Accept-Encoding")
          in
          equal int 1
            (List.length (Headers.find_all "vary" (Response.headers r))) )
    ; test "an upgrade and a HEAD" (fun () ->
          let r, _ =
            cors ~fields:[origin a] only (fun _ -> Response.upgrade ignore)
          in
          equal (option string) (Some a) (field "access-control-allow-origin" r) ;
          let r, _ =
            cors ~meth:Method.HEAD ~fields:[origin a] only (reply "x")
          in
          equal (option string) (Some a) (field "access-control-allow-origin" r) )
    ; test "a page of Error_page gets the fields" (fun () ->
          let page = P.middleware ~report:(fun _ _ _ -> ()) () in
          let r, _ =
            Weft_sim.run (fun _ ->
                cors
                  ~fields:[origin a]
                  (fun next -> only (page next))
                  (fun _ -> failwith "x") )
          in
          equal status Status.internal_server_error (Response.status r) ;
          equal (option string) (Some a) (field "access-control-allow-origin" r) )
    ; test "with the router" (fun () ->
          let router =
            Caravan_router.(
              handler [put "/r" (reply "put"); get "/r" (reply "get")] )
          in
          let h =
            Handler.compose
              [C.middleware ~origins:(C.Only [a]) ~allow_methods:Method.[PUT] ()]
              router
          in
          T.run h (fun t ->
              let pre path =
                T.fetch t
                  (Request.make
                     ~headers:(Headers.of_list (preflight a "PUT"))
                     Method.OPTIONS path )
              in
              List.iter
                (fun path ->
                  let r = pre path in
                  equal ~msg:path status Status.no_content r.status ;
                  equal ~msg:path (option string) (Some a)
                    (Headers.find "access-control-allow-origin" r.headers) ;
                  equal ~msg:path (option string) None
                    (Headers.find "allow" r.headers) )
                ["/r"; "/none"] ;
              let r =
                T.fetch t
                  (Request.make
                     ~headers:(Headers.of_list [origin a])
                     Method.GET "/r" )
              in
              equal string "get" r.body ;
              equal (option string) (Some a)
                (Headers.find "access-control-allow-origin" r.headers) ) )
    ; test "build errors" (fun () ->
          List.iter
            (fun (label, f) -> raises_match ~msg:label is_invalid_arg f)
            [ ( "Any with credentials"
              , fun () ->
                  made (C.middleware ~origins:C.Any ~credentials:true ()) )
            ; ( "a slash"
              , fun () ->
                  made
                    (C.middleware ~origins:(C.Only ["https://a.example/"]) ())
              )
            ; ( "a default port"
              , fun () ->
                  made
                    (C.middleware ~origins:(C.Only ["https://a.example:443"]) ())
              )
            ; ( "http 80"
              , fun () ->
                  made
                    (C.middleware ~origins:(C.Only ["http://a.example:80"]) ())
              )
            ; ("*", fun () -> made (C.middleware ~origins:(C.Only ["*"]) ()))
            ; ( "a bad scheme"
              , fun () ->
                  made (C.middleware ~origins:(C.Only ["h_t://a.example"]) ())
              )
            ; ( "no host"
              , fun () -> made (C.middleware ~origins:(C.Only ["http://"]) ())
              )
            ; ( "no host with a port"
              , fun () ->
                  made (C.middleware ~origins:(C.Only ["http://:8080"]) ()) )
            ; ( "a port of 65536"
              , fun () ->
                  made
                    (C.middleware ~origins:(C.Only ["https://a.example:65536"])
                       () ) )
            ; ( "ws"
              , fun () ->
                  made (C.middleware ~origins:(C.Only ["ws://a.example"]) ()) )
            ; ( "a leading zero"
              , fun () ->
                  made
                    (C.middleware ~origins:(C.Only ["https://a.example:0443"])
                       () ) )
            ; ( "a port too large"
              , fun () ->
                  made
                    (C.middleware ~origins:(C.Only ["http://a.example:99999"])
                       () ) )
            ; ( "a scheme with no letter first"
              , fun () -> made (C.middleware ~origins:(C.Only ["+x://a"]) ()) )
            ; ( "an IPv6 that is not canonical"
              , fun () ->
                  made (C.middleware ~origins:(C.Only ["http://[0:0::1]"]) ())
              )
            ; ( "a percent"
              , fun () ->
                  made
                    (C.middleware ~origins:(C.Only ["https://a%2eexample"]) ())
              )
            ; ( "Other \"*\" in allow_methods"
              , fun () ->
                  made
                    (C.middleware ~origins:C.Any
                       ~allow_methods:[Method.Other "*"] () ) )
            ; ( "track in allow_methods"
              , fun () ->
                  made
                    (C.middleware ~origins:C.Any
                       ~allow_methods:[Method.Other "track"] () ) )
            ; ( "a method that is not a token"
              , fun () ->
                  made
                    (C.middleware ~origins:C.Any
                       ~allow_methods:[Method.Other "a b"] () ) )
            ; ( "127.1"
              , fun () ->
                  made (C.middleware ~origins:(C.Only ["http://127.1"]) ()) )
            ; ( "1.2.3.256"
              , fun () ->
                  made (C.middleware ~origins:(C.Only ["http://1.2.3.256"]) ())
              )
            ; ( "0x7f.1"
              , fun () ->
                  made (C.middleware ~origins:(C.Only ["http://0x7f.1"]) ()) )
            ; ( "empty labels"
              , fun () ->
                  made (C.middleware ~origins:(C.Only ["http://..."]) ()) )
            ; ( "IPv4 mapped as a dotted quad"
              , fun () ->
                  made
                    (C.middleware ~origins:(C.Only ["http://[::ffff:1.2.3.4]"])
                       () ) )
            ; ( "TRACE in allow_methods"
              , fun () ->
                  made
                    (C.middleware ~origins:C.Any
                       ~allow_methods:Method.[TRACE]
                       () ) )
            ; ( "no scheme"
              , fun () -> made (C.middleware ~origins:(C.Only ["a.example"]) ())
              )
            ; ( "an empty port"
              , fun () ->
                  made
                    (C.middleware ~origins:(C.Only ["https://a.example:"]) ())
              )
            ; ( "CONNECT"
              , fun () ->
                  made
                    (C.middleware ~origins:C.Any
                       ~allow_methods:Method.[CONNECT]
                       () ) )
            ; ( "a field"
              , fun () ->
                  made (C.middleware ~origins:C.Any ~allow_headers:["a b"] ())
              )
            ; ( "an exposed field"
              , fun () ->
                  made (C.middleware ~origins:C.Any ~expose_headers:["a b"] ())
              )
            ; ( "* exposed with credentials"
              , fun () ->
                  made
                    (C.middleware ~origins:(C.Only [a]) ~credentials:true
                       ~expose_headers:["*"] () ) )
            ; ( "max_age"
              , fun () -> made (C.middleware ~origins:C.Any ~max_age:(-1) ()) )
            ; ( "preflight_status"
              , fun () ->
                  made
                    (C.middleware ~origins:C.Any
                       ~preflight_status:Status.not_found () ) ) ] ;
          made
            (C.middleware
               ~origins:
                 (C.Only
                    [ "null"
                    ; "http://localhost:8080"
                    ; "https://a.example:8443"
                    ; "http://[::1]:8080"
                    ; "capacitor://localhost"
                    ; "com.example.app://x"
                    ; "http://a9.example:8089"
                    ; "https://my-app_1.example"
                    ; "https://a.example:65535"
                    ; "http://127.0.0.1:3000"
                    ; "http://localhost"
                    ; "https://a.example."
                    ; "http://[::ffff:7f00:1]"
                    ; "https://xn--bcher-kva.example" ] )
               () ) ;
          (* 0 is a valid max_age: a browser does not keep the preflight. *)
          made (C.middleware ~origins:C.Any ~max_age:0 ()) )
    ; test "a star in an origin gets a hint" (fun () ->
          raises_match
            (function
              | Invalid_argument m ->
                  String.ends_with ~suffix:"use Matching for a pattern" m
              | _ ->
                  false )
            (fun () ->
              made (C.middleware ~origins:(C.Only ["https://*.example.com"]) ()) ) ;
          raises_match
            (function
              | Invalid_argument m ->
                  not (String.ends_with ~suffix:"use Matching for a pattern" m)
              | _ ->
                  false )
            (fun () ->
              made (C.middleware ~origins:(C.Only ["https://a.example/"]) ()) ) ;
          raises_match
            (function
              | Invalid_argument m ->
                  String.ends_with ~suffix:"use Any" m
              | _ ->
                  false )
            (fun () -> made (C.middleware ~origins:(C.Only ["*"]) ())) )
    ; test "credentials on a preflight" (fun () ->
          let mw = C.middleware ~origins:(C.Only [a]) ~credentials:true () in
          let r, _ = preflight_to mw a "GET" in
          equal (option string) (Some "true")
            (field "access-control-allow-credentials" r) ;
          let r, _ = preflight_to only a "GET" in
          equal (option string) None
            (field "access-control-allow-credentials" r) )
    ; test "the removal keeps the other fields" (fun () ->
          let h _ =
            Response.string
              ~headers:
                (Headers.of_list
                   [ ("Acc", "1")
                   ; ("X-Long-Field-Name-1", "2")
                   ; ("Access-Control-", "3")
                   ; ("Access-Control-Max-Age", "4") ] )
              ""
          in
          let r, _ = cors ~fields:[origin "https://evil.example"] only h in
          equal
            (list (pair string string))
            [("Acc", "1"); ("X-Long-Field-Name-1", "2")]
            (List.filter
               (fun (n, _) -> n <> "Content-Type" && n <> "Vary")
               (Headers.to_list (Response.headers r)) ) ;
          equal (option string) (Some "text/plain; charset=utf-8")
            (field "content-type" r) )
    ; test "null and an exception of Matching" (fun () ->
          let mw = C.middleware ~origins:(C.Only ["null"]) () in
          let r, _ = cors ~fields:[origin "null"] mw (reply "") in
          equal (option string) (Some "null")
            (field "access-control-allow-origin" r) ;
          let mw =
            C.middleware ~origins:(C.Matching (fun _ -> raise Exit)) ()
          in
          raises Exit (fun () -> cors ~fields:[origin a] mw (reply "")) )
    ; test "requested names that are not tokens" (fun () ->
          let mw = C.middleware ~origins:(C.Only [a]) ~allow_headers:["*"] () in
          let r, _ =
            preflight_to
              ~fields:[("Access-Control-Request-Headers", "x a")]
              mw a "GET"
          in
          equal (option string) (Some a) (field "access-control-allow-origin" r) ;
          equal (option string) None (field "access-control-allow-headers" r) )
    ; test "permits" (fun () ->
          is_true (C.permits C.Any "x") ;
          is_true (C.permits (C.Only [a]) "HTTPS://A.EXAMPLE") ;
          is_false (C.permits (C.Only [a]) "https://b.example") ;
          is_true (C.permits (C.Matching (fun o -> o = "x")) "x") ) ]

let () =
  exit
    (Windtrap.run "middleware"
       [ forwarded_tests
       ; request_id_tests
       ; access_log_tests
       ; error_page_tests
       ; cors_tests ] )
