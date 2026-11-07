(* The tests of the checks of the fake client of the HTTP/2 engine tests, over
   TCP pairs of the simulator. Each test runs 1 seed on 1 domain. *)

open Windtrap
open Weft
open Caravan_http2.Private
module Flow = Caravan.Flow
module Tcp = Caravan.Tcp
module Reader = Weft_flow.Reader
module Writer = Weft_flow.Writer
module Fake = Fake_h2_client

let show = H2_support.show

let frames_text l = String.concat "\n" (List.map show l)

let failed text = Sim_support.Failed text

(* The identifier of SETTINGS_INITIAL_WINDOW_SIZE. *)
let initial_window_size = 0x4

(* {1 The fake client} *)

(* The server side of a test of the checks: the test writes the frames of the
   server by hand. *)
type peer = {r: Reader.t; w: Writer.t; flow: Flow.t}

let peer_write p frames =
  List.iter (Frame.write p.w) frames ;
  Writer.flush p.w

let peer_raw p s = Writer.write_string p.w s ; Writer.flush p.w

let peer_read p = Frame.read p.r ~max_size:16384

(* [with_peer sim ~settings ~server f] connects a fake client with [settings] to
   a peer that the test drives. The peer reads the preface, the SETTINGS frame
   and the PING of the client. It sends a SETTINGS frame with [server] (and a
   WINDOW_UPDATE frame of [update], if any), the ACK of the client settings and
   the PING ACK, and reads the ACK of the client. Then [f c p ~start] runs.
   [start] is the frames of the client that the peer read. At the end of [f],
   the client and the peer end their streams, and [Fake.finish] raises the
   failure of a check, if any. *)
let with_peer ?(settings = []) ?(server = []) ?update sim f =
  Sim_support.with_pair sim (fun client socket ->
      let flow = Tcp.flow socket in
      let p = {r= Reader.create flow; w= Writer.create flow; flow} in
      Fiber.scope (fun s ->
          let start =
            Sim_support.call ~name:"peer" s (fun () ->
                Reader.ensure p.r 24 ;
                let preface = Reader.sub_string p.r 0 24 in
                Reader.consume p.r 24 ;
                let first = peer_read p in
                let ping = peer_read p in
                let ping_data =
                  match ping with Frame.Ping {data; _} -> data | _ -> ""
                in
                peer_write p
                  ( Frame.Settings {ack= false; settings= server}
                    :: Option.to_list
                         (Option.map
                            (fun increment ->
                              Frame.Window_update {stream= 0; increment} )
                            update )
                  @ [ Frame.Settings {ack= true; settings= []}
                    ; Frame.Ping {ack= true; data= ping_data} ] ) ;
                let ack = peer_read p in
                (preface = Frame.preface, [first; ping; ack]) )
          in
          let c = Fake.connect ~settings s (Tcp.flow client) in
          let preface_ok, start = Sim_support.await start in
          equal Testable.bool true preface_ok ;
          let v = f c p ~start in
          Fake.close c ;
          (try Flow.shutdown_send p.flow with Flow.Closed -> ()) ;
          Fake.finish c ; v ) )

(* [fails ~settings text f] makes sure that a run of [with_peer] with [f] raises
   the failure [text]. *)
let fails ?settings ?server text f =
  raises (failed text) (fun () ->
      Sim_support.run (fun sim -> with_peer ?settings ?server sim f) )

(* [fails_with ~settings ~setup frames text] sends [frames] from the peer after
   [setup c], and makes sure that the client fails with [text]. *)
let fails_with ?settings ?(setup = fun _ -> ()) frames text =
  fails ?settings text (fun c p ~start:_ -> setup c ; peer_write p frames)

let get = [(":method", "GET"); (":scheme", "http"); (":path", "/")]

(* Stream 1, which the client does not end: a RST of the server after its
   END_STREAM is legal there. *)
let open_1 c = Fake.request ~end_stream:false c ~stream:1 get

let data ?(end_stream = false) stream s =
  Frame.Data {stream; end_stream; data= s; flow_length= String.length s}

let headers ?(end_stream = false) ?(end_headers = true) stream fields =
  let buf = Buffer.create 64 in
  Hpack.encode buf fields ;
  Frame.Headers
    {stream; end_stream; end_headers; priority= None; block= Buffer.contents buf}

let ping data = Frame.Ping {ack= false; data}

(* [plain fields] is the block of [fields], with no table size update at its
   start: the encoder starts each block with one (the byte 0x20). *)
let plain fields =
  let b = Buffer.create 64 in
  Hpack.encode b fields ;
  let b = Buffer.contents b in
  String.sub b 1 (String.length b - 1)

(* [ok f] runs [with_peer] with [f], and makes sure that no check fails. *)
let ok ?settings ?server ?update f =
  Sim_support.run (fun sim -> with_peer ?settings ?server ?update sim f)

let over_stream n w =
  Printf.sprintf "DATA of %d bytes on stream 1 over the stream window of %d" n w

let fake_client_tests =
  group "Fake_h2_client"
    [ test "connect: the preface, the SETTINGS frames, the ACK" (fun () ->
          ok
            ~settings:[(initial_window_size, 1000)]
            ~server:[(0x3, 100); (initial_window_size, 65535)]
            ~update:((100 * 65535) - 65535)
            (fun c p ~start ->
              equal Testable.string
                "SETTINGS [0x4=1000]\nPING \"start000\"\nSETTINGS ACK []"
                (frames_text start) ;
              equal
                Testable.(list (pair int int))
                [(0x3, 100); (initial_window_size, 65535)]
                (Fake.server_settings c) ;
              (* The WINDOW_UPDATE frame of the start is not in the mailbox. *)
              peer_write p [ping "12345678"] ;
              equal Testable.string {|PING "12345678"|} (show (Fake.next c)) ) )
    ; test "a first frame of the server other than SETTINGS fails" (fun () ->
          raises
            (failed
               {|the first frame of the server is not SETTINGS: PING "12345678"|} )
            (fun () ->
              Sim_support.run (fun sim ->
                  Sim_support.with_pair sim (fun client socket ->
                      let flow = Tcp.flow socket in
                      let p =
                        {r= Reader.create flow; w= Writer.create flow; flow}
                      in
                      peer_write p
                        [ ping "12345678"
                        ; Frame.Settings {ack= false; settings= []} ] ;
                      Fiber.scope (fun s ->
                          ignore (Fake.connect s (Tcp.flow client)) ) ) ) ) )
    ; test
        "a RST_STREAM frame on a stream that ended in the two directions fails"
        (fun () ->
          fails_with
            ~setup:(fun c -> Fake.request c ~stream:1 get)
            [data ~end_stream:true 1 "a"; Rst_stream {stream= 1; code= No_error}]
            "a RST_STREAM frame on stream 1, which ended in the two directions" )
    ; test "a DATA frame over the stream window fails" (fun () ->
          fails
            ~settings:[(initial_window_size, 10)]
            (over_stream 1 0)
            (fun c p ~start:_ ->
              open_1 c ;
              peer_write p [data 1 "0123456789"] ;
              equal Testable.string {|DATA 1 "0123456789" flow_length=10|}
                (show (Fake.next c)) ;
              peer_write p [data 1 "a"] ) )
    ; test "a DATA frame over the connection window fails" (fun () ->
          fails_with
            ~settings:[(initial_window_size, 100_000)]
            ~setup:open_1
            (List.init 4 (fun _ -> data 1 (String.make 16384 'x')))
            "DATA of 16384 bytes on stream 1 over the connection window of \
             16383" )
    ; test "a WINDOW_UPDATE frame of the client adds to its windows" (fun () ->
          fails
            ~settings:[(initial_window_size, 10)]
            (over_stream 1 0)
            (fun c p ~start:_ ->
              open_1 c ;
              Fake.credit c ~stream:1 5 ;
              peer_write p [data 1 (String.make 15 'x')] ;
              ignore (Fake.next c) ;
              peer_write p [data 1 "a"] ) )
    ; test "a smaller initial window applies at the SETTINGS ACK" (fun () ->
          fails
            ~settings:[(initial_window_size, 100)]
            (over_stream 1 (-40))
            (fun c p ~start:_ ->
              open_1 c ;
              ignore (peer_read p) ;
              Fake.send c
                (Settings {ack= false; settings= [(initial_window_size, 10)]}) ;
              ignore (peer_read p) ;
              (* Before the ACK, the old window applies. *)
              peer_write p [data 1 (String.make 50 'x')] ;
              ignore (Fake.next c) ;
              peer_write p [Settings {ack= true; settings= []}; data 1 "a"] ) )
    ; test "a larger initial window applies at once" (fun () ->
          fails
            ~settings:[(initial_window_size, 10)]
            (over_stream 1 0)
            (fun c p ~start:_ ->
              open_1 c ;
              Fake.send c
                (Settings {ack= false; settings= [(initial_window_size, 30)]}) ;
              peer_write p [data 1 (String.make 30 'x')] ;
              ignore (Fake.next c) ;
              peer_write p [data 1 "a"] ) )
    ; test "a frame in a header block fails" (fun () ->
          fails_with ~setup:open_1
            [headers ~end_headers:false 1 [(":status", "200")]; data 1 "a"]
            {|a frame in the header block of stream 1: DATA 1 "a" flow_length=1|} )
    ; test "a CONTINUATION frame of another stream in a block fails" (fun () ->
          fails_with
            ~setup:(fun c ->
              open_1 c ;
              Fake.request c ~stream:3 get )
            [ headers ~end_headers:false 1 [(":status", "200")]
            ; Continuation {stream= 3; end_headers= true; block= ""} ]
            {|a frame in the header block of stream 1: CONTINUATION 3 END_HEADERS ""|} )
    ; test "a CONTINUATION frame with no block fails" (fun () ->
          fails_with ~setup:open_1
            [Continuation {stream= 1; end_headers= true; block= ""}]
            "a CONTINUATION frame on stream 1 with no open block" )
    ; test "a header block that does not decode fails" (fun () ->
          fails_with ~setup:open_1
            [ Headers
                { stream= 1
                ; end_stream= false
                ; end_headers= true
                ; priority= None
                ; block= "\x80" } ]
            "a header block that does not decode: the index 0" )
    ; test "after END_STREAM, only RST_STREAM, WINDOW_UPDATE and PRIORITY"
        (fun () ->
          ok (fun c p ~start:_ ->
              open_1 c ;
              let after =
                [ data ~end_stream:true 1 "a"
                ; Window_update {stream= 1; increment= 1}
                ; Priority
                    { stream= 1
                    ; priority= {exclusive= false; dependency= 0; weight= 1} }
                ; Rst_stream {stream= 1; code= No_error} ]
              in
              peer_write p after ;
              equal Testable.string (frames_text after)
                (frames_text (List.init 4 (fun _ -> Fake.next c))) ) )
    ; test "DATA after END_STREAM fails" (fun () ->
          fails_with ~setup:open_1
            [data ~end_stream:true 1 "a"; data 1 "b"]
            "a frame on stream 1 after END_STREAM" )
    ; test "END_STREAM of a HEADERS frame applies at its END_HEADERS" (fun () ->
          fails_with ~setup:open_1
            [ headers ~end_stream:true ~end_headers:false 1 [(":status", "200")]
            ; Continuation {stream= 1; end_headers= true; block= ""}
            ; data 1 "b" ]
            "a frame on stream 1 after END_STREAM" )
    ; test "a frame after a RST_STREAM frame of the server fails" (fun () ->
          fails_with ~setup:open_1
            [ Rst_stream {stream= 1; code= Cancel}
            ; Window_update {stream= 1; increment= 1} ]
            "a frame on stream 1 after its RST_STREAM frame" )
    ; test "reset: frames before the PING ACK are legal, not after" (fun () ->
          fails
            {|a frame on stream 1 after the barrier of its reset: DATA 1 "b" flow_length=1|}
            (fun c p ~start:_ ->
              open_1 c ;
              ignore (peer_read p) ;
              Fake.reset c ~stream:1 Cancel
                ~also:[Window_update {stream= 0; increment= 1}] ;
              let rst = peer_read p in
              let also = peer_read p in
              let barrier = peer_read p in
              equal Testable.string
                "RST_STREAM 1 CANCEL\nWINDOW_UPDATE 0 +1\nPING \"barr0001\""
                (frames_text [rst; also; barrier]) ;
              peer_write p
                [data 1 "a"; Ping {ack= true; data= "barr0001"}; data 1 "b"] ;
              (* The PING ACK of the barrier is not in the mailbox. *)
              equal Testable.string {|DATA 1 "a" flow_length=1|}
                (show (Fake.next c)) ) )
    ; test "a PING ACK of another data goes in the mailbox" (fun () ->
          ok (fun c p ~start:_ ->
              peer_write p [Ping {ack= true; data= "abcdefgh"}] ;
              equal Testable.string {|PING ACK "abcdefgh"|} (show (Fake.next c)) ) )
    ; test "a frame over the maximum frame size fails" (fun () ->
          fails
            "the server sent a bad frame: a frame of 16385 bytes, more than \
             16384" (fun c p ~start:_ ->
              open_1 c ;
              peer_raw p
                (H2_support.raw ~kind:0 ~stream:1 (String.make 16385 'x')) ) )
    ; test "a larger SETTINGS_MAX_FRAME_SIZE of the client applies" (fun () ->
          ok
            ~settings:[(initial_window_size, 100_000); (0x5, 20000)]
            (fun c p ~start:_ ->
              open_1 c ;
              peer_write p [data 1 (String.make 20000 'x')] ;
              equal Testable.int 20000
                ( match Fake.next c with
                | Data {flow_length; _} ->
                    flow_length
                | _ ->
                    -1 ) ) )
    ; test "PUSH_PROMISE fails" (fun () ->
          fails "the server sent PUSH_PROMISE" (fun c p ~start:_ ->
              open_1 c ;
              peer_raw p (H2_support.raw ~kind:5 ~stream:1 "\000\000\000\002") ) )
    ; test "a frame on a stream that the client did not open fails" (fun () ->
          fails_with
            [headers 2 [(":status", "200")]]
            "a frame on stream 2, which the client did not open" )
    ; test "a malformed frame fails" (fun () ->
          fails
            "the server sent a malformed frame: a PRIORITY frame that depends \
             on its own stream" (fun c p ~start:_ ->
              open_1 c ;
              peer_raw p
                (H2_support.raw ~kind:2 ~stream:1 "\000\000\000\001\000") ) )
    ; test "a frame of an unknown type fails" (fun () ->
          fails "the server sent a frame of the unknown type 0xfa"
            (fun _ p ~start:_ ->
              peer_raw p (H2_support.raw ~kind:0xfa ~stream:0 "") ) )
    ; test "response: the heads, the body, the trailers; the rest stays"
        (fun () ->
          ok (fun c p ~start:_ ->
              open_1 c ;
              Fake.request c ~stream:3 get ;
              let b = plain [(":status", "200"); ("x", "y")] in
              peer_write p
                [ headers 1 [(":status", "100")]
                ; ping "00000000"
                ; Headers
                    { stream= 1
                    ; end_stream= false
                    ; end_headers= false
                    ; priority= None
                    ; block= String.sub b 0 2 }
                ; Continuation
                    { stream= 1
                    ; end_headers= true
                    ; block= String.sub b 2 (String.length b - 2) }
                ; data 1 "ab"
                ; data 3 "z"
                ; data 1 "cd"
                ; headers ~end_stream:true 1 [("t", "1")]
                ; Rst_stream {stream= 1; code= No_error} ] ;
              let r = Fake.response c ~stream:1 in
              let fields l =
                String.concat " " (List.map (fun (n, v) -> n ^ "=" ^ v) l)
              in
              equal Testable.string
                "interim :status=100; head :status=200 x=y; body abcd; \
                 trailers t=1; no reset"
                (Printf.sprintf "interim %s; head %s; body %s; trailers %s; %s"
                   (String.concat ", " (List.map fields r.interim))
                   (fields r.head) r.body (fields r.trailers)
                   ( match r.reset with
                   | None ->
                       "no reset"
                   | Some code ->
                       H2_support.show_code code ) ) ;
              equal Testable.string
                "PING \"00000000\"\n\
                 DATA 3 \"z\" flow_length=1\n\
                 RST_STREAM 1 NO_ERROR"
                (frames_text (List.init 3 (fun _ -> Fake.next c))) ) )
    ; test "response: a reset ends it, and so does END_STREAM on a head"
        (fun () ->
          ok (fun c p ~start:_ ->
              open_1 c ;
              Fake.request c ~stream:3 get ;
              Fake.request c ~stream:5 get ;
              let b = plain [(":status", "204")] in
              peer_write p
                [ headers 1 [(":status", "200")]
                ; Rst_stream {stream= 1; code= Cancel}
                ; headers ~end_stream:true 3 [(":status", "204")]
                ; Headers
                    { stream= 5
                    ; end_stream= true
                    ; end_headers= false
                    ; priority= None
                    ; block= String.sub b 0 1 }
                ; Continuation
                    { stream= 5
                    ; end_headers= true
                    ; block= String.sub b 1 (String.length b - 1) } ] ;
              let rs =
                List.map (fun stream -> Fake.response c ~stream) [1; 3; 5]
              in
              equal
                Testable.(list (option string))
                [Some "CANCEL"; None; None]
                (List.map
                   (fun (r : Fake.response) ->
                     Option.map H2_support.show_code r.reset )
                   rs ) ;
              equal
                Testable.(list (list (pair string string)))
                [ [(":status", "200")]
                ; [(":status", "204")]
                ; [(":status", "204")] ]
                (List.map (fun (r : Fake.response) -> r.head) rs) ) )
    ; test "frames_until gives the frames up to the first match" (fun () ->
          ok (fun c p ~start:_ ->
              peer_write p [ping "00000001"; ping "00000002"; ping "00000003"] ;
              equal Testable.string "PING \"00000001\"\nPING \"00000002\""
                (frames_text
                   (Fake.frames_until c (function
                     | Ping {data; _} ->
                         data = "00000002"
                     | _ ->
                         false )) ) ;
              equal Testable.string {|PING "00000003"|} (show (Fake.next c)) ) )
    ; test "next fails at the end of the stream of the server" (fun () ->
          ok (fun c p ~start:_ ->
              Flow.shutdown_send p.flow ;
              raises
                (failed "the server closed the connection before the next frame")
                (fun () -> Fake.next c ) ) )
    ; test "data waits for the credit of the server" (fun () ->
          Sim_support.run (fun sim ->
              with_peer
                ~server:[(initial_window_size, 10)]
                sim
                (fun c p ~start:_ ->
                  Fake.request ~end_stream:false c ~stream:1 get ;
                  ignore (peer_read p) ;
                  Fiber.scope (fun s ->
                      let send =
                        Sim_support.call s (fun () ->
                            Fake.data ~end_stream:true c ~stream:1
                              (String.make 25 'x') )
                      in
                      equal Testable.string
                        {|DATA 1 "xxxxxxxxxx" flow_length=10|}
                        (show (peer_read p)) ;
                      equal Testable.bool false (Sim_support.ended sim send) ;
                      peer_write p [Window_update {stream= 1; increment= 10}] ;
                      equal Testable.string
                        {|DATA 1 "xxxxxxxxxx" flow_length=10|}
                        (show (peer_read p)) ;
                      peer_write p [Window_update {stream= 1; increment= 100}] ;
                      equal Testable.string
                        {|DATA 1 END_STREAM "xxxxx" flow_length=5|}
                        (show (peer_read p)) ;
                      Sim_support.await send ) ) ) )
    ; test "data stops at a reset of the server" (fun () ->
          ok
            ~server:[(initial_window_size, 0)]
            (fun c p ~start:_ ->
              Fake.request ~end_stream:false c ~stream:1 get ;
              peer_write p [Rst_stream {stream= 1; code= Cancel}] ;
              ignore (Fake.next c) ;
              raises (failed "the server reset stream 1 during a send")
                (fun () -> Fake.data c ~stream:1 "x" ) ;
              raises (failed "DATA on stream 3, which is not open") (fun () ->
                  Fake.data c ~stream:3 "x" ) ) )
    ; test "pause_reading stops the reads until resume_reading" (fun () ->
          Sim_support.run (fun sim ->
              with_peer sim (fun c p ~start:_ ->
                  Fake.pause_reading c ;
                  Fiber.scope (fun s ->
                      (* More than the buffers of the socket and of the
                         reader. *)
                      let pings = List.init 20_000 (fun _ -> ping "pppppppp") in
                      let writes =
                        Sim_support.call s (fun () -> peer_write p pings)
                      in
                      equal Testable.bool false (Sim_support.ended sim writes) ;
                      Fake.resume_reading c ;
                      Sim_support.await writes ) ) ) ) ]

let () = exit (Windtrap.run "h2_engine" [fake_client_tests])
