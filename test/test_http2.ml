(* The tests of caravan.http2: the error codes and the frames. *)

open Windtrap
open Weft
module Error = Caravan_http2.Private.Error
module Frame = Caravan_http2.Private.Frame
module Reader = Weft_flow.Reader
module Writer = Weft_flow.Writer
module Flow = Caravan.Flow

let code = Testable.structural ~pp:Error.pp_code

let error_tests =
  group "Error"
    [ test "each code of RFC 9113 has its value, and back" (fun () ->
          let names =
            List.init 14 (fun n ->
                let c = Error.code_of_int n in
                equal Testable.int n (Error.code_to_int c) ;
                Format.asprintf "%a" Error.pp_code c )
          in
          equal
            Testable.(list string)
            [ "NO_ERROR"
            ; "PROTOCOL_ERROR"
            ; "INTERNAL_ERROR"
            ; "FLOW_CONTROL_ERROR"
            ; "SETTINGS_TIMEOUT"
            ; "STREAM_CLOSED"
            ; "FRAME_SIZE_ERROR"
            ; "REFUSED_STREAM"
            ; "CANCEL"
            ; "COMPRESSION_ERROR"
            ; "CONNECT_ERROR"
            ; "ENHANCE_YOUR_CALM"
            ; "INADEQUATE_SECURITY"
            ; "HTTP_1_1_REQUIRED" ]
            names )
    ; test "an unknown code keeps its value" (fun () ->
          List.iter
            (fun n ->
              let c = Error.code_of_int n in
              equal code (Error.Unknown_code n) c ;
              equal Testable.int n (Error.code_to_int c) )
            [0xe; 0xff; 0xffff_ffff] ;
          equal Testable.string "0xff"
            (Format.asprintf "%a" Error.pp_code (Error.code_of_int 0xff)) )
    ; test "a value out of 32 bits is refused" (fun () ->
          raises (Invalid_argument "Error.code_of_int: -1") (fun () ->
              Error.code_of_int (-1) ) ;
          raises (Invalid_argument "Error.code_of_int: 4294967296") (fun () ->
              Error.code_of_int 0x1_0000_0000 ) ) ]

(* {1 Frames} *)

let sprintf = Printf.sprintf

let show_code c = Format.asprintf "%a" Error.pp_code c

(* The names of the flags that are set, each after a space. *)
let flags l =
  String.concat ""
    (List.filter_map
       (fun (set, name) -> if set then Some (" " ^ name) else None)
       l )

let show_priority {Frame.exclusive; dependency; weight} =
  sprintf "priority=%s%d/%d"
    (if exclusive then "exclusive:" else "")
    dependency weight

let show : Frame.t -> string = function
  | Data {stream; end_stream; data; flow_length} ->
      sprintf "DATA %d%s %S flow_length=%d" stream
        (flags [(end_stream, "END_STREAM")])
        data flow_length
  | Headers {stream; end_stream; end_headers; priority; block} ->
      sprintf "HEADERS %d%s%s %S" stream
        (flags [(end_stream, "END_STREAM"); (end_headers, "END_HEADERS")])
        (match priority with None -> "" | Some p -> " " ^ show_priority p)
        block
  | Priority {stream; priority} ->
      sprintf "PRIORITY %d %s" stream (show_priority priority)
  | Rst_stream {stream; code} ->
      sprintf "RST_STREAM %d %s" stream (show_code code)
  | Settings {ack; settings} ->
      sprintf "SETTINGS%s [%s]"
        (flags [(ack, "ACK")])
        (String.concat "; "
           (List.map (fun (id, v) -> sprintf "0x%x=%d" id v) settings) )
  | Push_promise {stream} ->
      sprintf "PUSH_PROMISE %d" stream
  | Ping {ack; data} ->
      sprintf "PING%s %S" (flags [(ack, "ACK")]) data
  | Goaway {last_stream; code; debug} ->
      sprintf "GOAWAY %d %s %S" last_stream (show_code code) debug
  | Window_update {stream; increment} ->
      sprintf "WINDOW_UPDATE %d +%d" stream increment
  | Continuation {stream; end_headers; block} ->
      sprintf "CONTINUATION %d%s %S" stream
        (flags [(end_headers, "END_HEADERS")])
        block
  | Unknown {kind; stream} ->
      sprintf "UNKNOWN 0x%x %d" kind stream
  | Malformed {stream; code; reason} ->
      sprintf "MALFORMED %d %s: %s" stream (show_code code) reason

let frame =
  Testable.structural ~pp:(fun ppf f -> Format.pp_print_string ppf (show f))

let of_hex h =
  let h = String.concat "" (String.split_on_char ' ' h) in
  String.init
    (String.length h / 2)
    (fun i -> Char.chr (int_of_string ("0x" ^ String.sub h (2 * i) 2)))

(* [writes ~max_frame f] is the bytes that [f w] writes to a new writer [w]. *)
let writes ?max_frame f =
  let out = Buffer.create 64 in
  let w = Writer.create ?max_frame (Flow.of_string "" out) in
  f w ; Writer.flush w ; Buffer.contents out

let encode f = writes (fun w -> Frame.write w f)

(* A frame built by hand, with no help from Frame. [length] is the length of
   [payload] by default. *)
let raw ?length ~kind ?(flags = 0) ~stream payload =
  let length = Option.value length ~default:(String.length payload) in
  let b = Bytes.create 9 in
  Bytes.set_uint8 b 0 (length lsr 16) ;
  Bytes.set_uint16_be b 1 (length land 0xffff) ;
  Bytes.set_uint8 b 3 kind ;
  Bytes.set_uint8 b 4 flags ;
  Bytes.set_int32_be b 5 (Int32.of_int stream) ;
  Bytes.to_string b ^ payload

(* A reader of [s]. A read of the flow gives at most [chunk] bytes. *)
let reader ?size ?chunk s =
  Reader.create ?size (Flow.of_string ?chunk s (Buffer.create 0))

(* [at_end r] is [true] if [r] holds no byte and its stream ended. *)
let at_end r =
  Reader.buffered r = 0
  && match Reader.ensure r 1 with () -> false | exception End_of_file -> true

(* What [Frame.read] makes of [input]: the frame, and the bytes after it if
   there are some, or the error and the number of bytes that it consumed. *)
let outcome ?(max_size = 16384) input =
  let r = reader input in
  match Frame.read r ~max_size with
  | f ->
      if at_end r then show f
      else sprintf "%s, then %d bytes" (show f) (Reader.buffered r)
  | exception Frame.Connection_error {code; reason} ->
      sprintf "connection error %s: %s (consumed %d)" (show_code code) reason
        (Reader.consumed r)
  | exception End_of_file ->
      "end of input"

(* The outcomes of [cases], one line for each: the name, then the outcome. *)
let outcomes ?max_size cases =
  String.concat "\n"
    (List.map
       (fun (name, input) -> name ^ ": " ^ outcome ?max_size input)
       cases )

(* The generators of frames that [read] gives back from [write]: no
   Push_promise, Unknown or Malformed, and a [flow_length] that is the length of
   the data. *)
let frame_gen =
  let open Gen in
  let stream = int_range 1 0x7fff_ffff in
  let any_stream = int_range 0 0x7fff_ffff in
  let uint32 = int_range 0 0xffff_ffff in
  let code = map Error.code_of_int (one_of [int_range 0 15; uint32]) in
  let payload = string_of ~size:(int_range 0 40) char in
  let priority =
    let+ exclusive = bool
    and+ dependency = any_stream
    and+ weight = int_range 0 255 in
    {Frame.exclusive; dependency; weight}
  in
  with_pp
    (fun ppf f -> Format.pp_print_string ppf (show f))
    (one_of
       [ (let+ stream = stream and+ end_stream = bool and+ data = payload in
          Frame.Data {stream; end_stream; data; flow_length= String.length data}
         )
       ; (let+ stream = stream
          and+ end_stream = bool
          and+ end_headers = bool
          and+ priority = option priority
          and+ block = payload in
          Frame.Headers {stream; end_stream; end_headers; priority; block} )
       ; (let+ stream = stream and+ priority = priority in
          (* A PRIORITY frame that depends on its own stream is Malformed. *)
          let priority =
            if priority.dependency = stream then {priority with dependency= 0}
            else priority
          in
          Frame.Priority {stream; priority} )
       ; (let+ stream = stream and+ code = code in
          Frame.Rst_stream {stream; code} )
       ; (let+ ack = bool
          and+ settings =
            list ~size:(int_range 0 5)
              (pair (of_list [1; 2; 3; 4; 5; 6; 8]) uint32)
          in
          Frame.Settings {ack; settings= (if ack then [] else settings)} )
       ; (let+ ack = bool and+ data = string_of ~size:(constant 8) char in
          Frame.Ping {ack; data} )
       ; (let+ last_stream = any_stream and+ code = code and+ debug = payload in
          Frame.Goaway {last_stream; code; debug} )
       ; (let+ stream = any_stream and+ increment = int_range 0 0x7fff_ffff in
          Frame.Window_update {stream; increment} )
       ; (let+ stream = stream and+ end_headers = bool and+ block = payload in
          Frame.Continuation {stream; end_headers; block} ) ] )

(* A reader that waits for [gate] when it gave all the bytes of [pending]. *)
let gated_flow pending gate =
  let rec read buf off len =
    match (!pending, Ivar.peek gate) with
    | "", Some () ->
        0
    | "", None ->
        sync (Ivar.read gate) ;
        read buf off len
    | s, _ ->
        let n = Int.min len (String.length s) in
        Bytes.blit_string s 0 buf off n ;
        pending := String.sub s n (String.length s - n) ;
        n
  in
  Flow.make ~read
    ~read_now:(fun _ _ _ -> None)
    ~write:(fun _ _ _ -> ())
    ~readable:(Ivar.read gate) ~shutdown_send:ignore ~close:ignore
    ~set_read_deadline:ignore ~set_write_deadline:ignore ()

let frame_tests =
  group "Frame"
    [ prop "read inverts write" frame_gen (fun f ->
          let r = reader (encode f) in
          equal frame f (Frame.read r ~max_size:16384) ;
          is_true ~msg:"the frame is consumed" (at_end r) )
    ; prop "frames split in parts of 1 to 20 bytes read the same"
        Gen.(
          triple
            (list ~size:(int_range 1 5) frame_gen)
            (int_range 1 20) (int_range 1 32) )
        (fun (fs, chunk, size) ->
          let r = reader ~size ~chunk (String.concat "" (List.map encode fs)) in
          equal (list frame) fs
            (List.map (fun _ -> Frame.read r ~max_size:16384) fs) ;
          is_true ~msg:"all frames are consumed" (at_end r) )
    ; (* The rules of read, one test for each row. *)
      test "a frame longer than max_size" (fun () ->
          expect
            (outcomes ~max_size:5
               [ ("5 bytes", raw ~kind:0 ~stream:1 "abcde")
               ; ("6 bytes", raw ~kind:0 ~stream:1 "abcdef")
               ; ("unknown type", raw ~kind:0xff ~stream:1 "abcdef")
               ; ("no payload yet", raw ~length:6 ~kind:0 ~stream:1 "") ] )
          @@ __POS_OF__
               {|
               5 bytes: DATA 1 "abcde" flow_length=5
               6 bytes: connection error FRAME_SIZE_ERROR: a frame of 6 bytes, more than 5 (consumed 0)
               unknown type: connection error FRAME_SIZE_ERROR: a frame of 6 bytes, more than 5 (consumed 0)
               no payload yet: connection error FRAME_SIZE_ERROR: a frame of 6 bytes, more than 5 (consumed 0)
               |} )
    ; test "a stream frame on stream 0" (fun () ->
          let p = "\x00\x00\x00\x01\x00" in
          expect
            (outcomes
               [ ("DATA", raw ~kind:0 ~stream:0 "")
               ; ("HEADERS", raw ~kind:1 ~stream:0 "")
               ; ("PRIORITY", raw ~kind:2 ~stream:0 p)
               ; ("RST_STREAM", raw ~kind:3 ~stream:0 "\x00\x00\x00\x08")
               ; ("PUSH_PROMISE", raw ~kind:5 ~stream:0 "\x00\x00\x00\x02")
               ; ("CONTINUATION", raw ~kind:9 ~stream:0 "")
               ; ("reserved bit only", raw ~kind:0 ~stream:0x8000_0000 "")
               ; ("reserved bit and 1", raw ~kind:0 ~stream:0x8000_0001 "")
               ; ("PUSH_PROMISE 2", raw ~kind:5 ~stream:2 "\x00\x00\x00\x04")
               ; ("WINDOW_UPDATE", raw ~kind:8 ~stream:0 "\x00\x00\x00\x01")
               ; ("RST_STREAM, header only", raw ~length:4 ~kind:3 ~stream:0 "")
               ] )
          @@ __POS_OF__
               {|
               DATA: connection error PROTOCOL_ERROR: a DATA frame on stream 0 (consumed 0)
               HEADERS: connection error PROTOCOL_ERROR: a HEADERS frame on stream 0 (consumed 0)
               PRIORITY: connection error PROTOCOL_ERROR: a PRIORITY frame on stream 0 (consumed 0)
               RST_STREAM: connection error PROTOCOL_ERROR: a RST_STREAM frame on stream 0 (consumed 0)
               PUSH_PROMISE: connection error PROTOCOL_ERROR: a PUSH_PROMISE frame on stream 0 (consumed 0)
               CONTINUATION: connection error PROTOCOL_ERROR: a CONTINUATION frame on stream 0 (consumed 0)
               reserved bit only: connection error PROTOCOL_ERROR: a DATA frame on stream 0 (consumed 0)
               reserved bit and 1: DATA 1 "" flow_length=0
               PUSH_PROMISE 2: PUSH_PROMISE 2
               WINDOW_UPDATE: WINDOW_UPDATE 0 +1
               RST_STREAM, header only: connection error PROTOCOL_ERROR: a RST_STREAM frame on stream 0 (consumed 0)
               |} )
    ; test "a connection frame on another stream" (fun () ->
          expect
            (outcomes
               [ ("SETTINGS", raw ~kind:4 ~stream:1 "")
               ; ("PING", raw ~kind:6 ~stream:3 "01234567")
               ; ( "GOAWAY"
                 , raw ~kind:7 ~stream:0x7fff_ffff (String.make 8 '\x00') )
               ; ("SETTINGS, reserved bit", raw ~kind:4 ~stream:0x8000_0000 "")
               ; ("PING, header only", raw ~length:8 ~kind:6 ~stream:3 "") ] )
          @@ __POS_OF__
               {|
               SETTINGS: connection error PROTOCOL_ERROR: a SETTINGS frame on stream 1 (consumed 0)
               PING: connection error PROTOCOL_ERROR: a PING frame on stream 3 (consumed 0)
               GOAWAY: connection error PROTOCOL_ERROR: a GOAWAY frame on stream 2147483647 (consumed 0)
               SETTINGS, reserved bit: SETTINGS []
               PING, header only: connection error PROTOCOL_ERROR: a PING frame on stream 3 (consumed 0)
               |} )
    ; test "a PADDED frame with no Pad Length byte" (fun () ->
          expect
            (outcomes
               [ ("DATA", raw ~kind:0 ~flags:0x8 ~stream:1 "")
               ; ("HEADERS", raw ~kind:1 ~flags:0xc ~stream:1 "")
               ; ("DATA, 1 byte", raw ~kind:0 ~flags:0x8 ~stream:1 "\x00")
               ; ("HEADERS, 1 byte", raw ~kind:1 ~flags:0xc ~stream:1 "\x00") ] )
          @@ __POS_OF__
               {|
               DATA: connection error FRAME_SIZE_ERROR: a PADDED frame with no Pad Length byte (consumed 0)
               HEADERS: connection error FRAME_SIZE_ERROR: a PADDED frame with no Pad Length byte (consumed 0)
               DATA, 1 byte: DATA 1 "" flow_length=1
               HEADERS, 1 byte: HEADERS 1 END_HEADERS ""
               |} )
    ; test "a padding longer than the rest of the payload" (fun () ->
          let prio = "\x80\x00\x00\x03\x0f" in
          expect
            (outcomes
               [ ("DATA", raw ~kind:0 ~flags:0x8 ~stream:1 "\x03ab")
               ; ("HEADERS", raw ~kind:1 ~flags:0xc ~stream:1 "\x03ab")
               ; ( "HEADERS with PRIORITY"
                 , raw ~kind:1 ~flags:0x2c ~stream:1 ("\x03" ^ prio ^ "ab") )
               ; ( "HEADERS with PRIORITY, pad 2"
                 , raw ~kind:1 ~flags:0x2c ~stream:1 ("\x02" ^ prio ^ "ab") )
               ; ( "HEADERS with PRIORITY, pad 1"
                 , raw ~kind:1 ~flags:0x2c ~stream:1 ("\x01" ^ prio ^ "ab") ) ] )
          @@ __POS_OF__
               {|
               DATA: connection error PROTOCOL_ERROR: a padding of 3 bytes in a payload of 2 bytes (consumed 0)
               HEADERS: connection error PROTOCOL_ERROR: a padding of 3 bytes in a payload of 2 bytes (consumed 0)
               HEADERS with PRIORITY: connection error PROTOCOL_ERROR: a padding of 3 bytes in a payload of 2 bytes (consumed 0)
               HEADERS with PRIORITY, pad 2: HEADERS 1 END_HEADERS priority=exclusive:3/15 ""
               HEADERS with PRIORITY, pad 1: HEADERS 1 END_HEADERS priority=exclusive:3/15 "a"
               |} )
    ; test "a HEADERS frame with PRIORITY and less than 5 bytes" (fun () ->
          expect
            (outcomes
               [ ( "4 bytes"
                 , raw ~kind:1 ~flags:0x20 ~stream:1 "\x00\x00\x00\x03" )
               ; ( "PADDED, 4 bytes after the Pad Length byte"
                 , raw ~kind:1 ~flags:0x28 ~stream:1 "\x00\x00\x00\x00\x03" )
               ; ( "5 bytes"
                 , raw ~kind:1 ~flags:0x20 ~stream:1 "\x00\x00\x00\x03\x00" )
               ; ( "PADDED, 5 bytes after the Pad Length byte"
                 , raw ~kind:1 ~flags:0x28 ~stream:1 "\x00\x00\x00\x00\x03\x00"
                 ) ] )
          @@ __POS_OF__
               {|
               4 bytes: connection error FRAME_SIZE_ERROR: a HEADERS frame with PRIORITY and less than 5 bytes (consumed 0)
               PADDED, 4 bytes after the Pad Length byte: connection error FRAME_SIZE_ERROR: a HEADERS frame with PRIORITY and less than 5 bytes (consumed 0)
               5 bytes: HEADERS 1 priority=3/0 ""
               PADDED, 5 bytes after the Pad Length byte: HEADERS 1 priority=3/0 ""
               |} )
    ; test "a PRIORITY frame with a length other than 5" (fun () ->
          expect
            (outcomes
               [ ("4 bytes", raw ~kind:2 ~stream:1 "\x00\x00\x00\x03")
               ; ("6 bytes", raw ~kind:2 ~stream:1 "\x00\x00\x00\x03\x00\x00")
               ; ("0 bytes", raw ~kind:2 ~stream:1 "")
               ; ("5 bytes", raw ~kind:2 ~stream:1 "\x00\x00\x00\x03\x07") ] )
          @@ __POS_OF__
               {|
               4 bytes: MALFORMED 1 FRAME_SIZE_ERROR: a PRIORITY frame with 4 bytes, not 5
               6 bytes: MALFORMED 1 FRAME_SIZE_ERROR: a PRIORITY frame with 6 bytes, not 5
               0 bytes: MALFORMED 1 FRAME_SIZE_ERROR: a PRIORITY frame with 0 bytes, not 5
               5 bytes: PRIORITY 1 priority=3/7
               |} )
    ; test "a PRIORITY frame that depends on its own stream" (fun () ->
          expect
            (outcomes
               [ ("stream 3", raw ~kind:2 ~stream:3 "\x00\x00\x00\x03\x07")
               ; ("exclusive", raw ~kind:2 ~stream:3 "\x80\x00\x00\x03\x07")
               ; ("stream 3 on 1", raw ~kind:2 ~stream:1 "\x80\x00\x00\x03\x07")
               ] )
          @@ __POS_OF__
               {|
               stream 3: MALFORMED 3 PROTOCOL_ERROR: a PRIORITY frame that depends on its own stream
               exclusive: MALFORMED 3 PROTOCOL_ERROR: a PRIORITY frame that depends on its own stream
               stream 3 on 1: PRIORITY 1 priority=exclusive:3/7
               |} )
    ; test "RST_STREAM, WINDOW_UPDATE and PING with an incorrect length"
        (fun () ->
          expect
            (outcomes
               [ ("RST_STREAM 3", raw ~kind:3 ~stream:1 "\x00\x00\x08")
               ; ("RST_STREAM 5", raw ~kind:3 ~stream:1 "\x00\x00\x00\x08\x00")
               ; ("WINDOW_UPDATE 3", raw ~kind:8 ~stream:1 "\x00\x00\x01")
               ; ( "WINDOW_UPDATE 5"
                 , raw ~kind:8 ~stream:0 "\x00\x00\x00\x01\x00" )
               ; ("PING 7", raw ~kind:6 ~stream:0 "0123456")
               ; ("PING 9", raw ~kind:6 ~stream:0 "012345678")
               ; ("RST_STREAM 4", raw ~kind:3 ~stream:1 "\x00\x00\x00\x08")
               ; ("WINDOW_UPDATE 4", raw ~kind:8 ~stream:1 "\xff\xff\xff\xff")
               ; ("PING 8", raw ~kind:6 ~flags:0xff ~stream:0 "01234567")
               ; ( "RST_STREAM 5, header only"
                 , raw ~length:5 ~kind:3 ~stream:1 "" )
               ; ( "WINDOW_UPDATE 3, header only"
                 , raw ~length:3 ~kind:8 ~stream:1 "" )
               ; ("PING 9, header only", raw ~length:9 ~kind:6 ~stream:0 "") ] )
          @@ __POS_OF__
               {|
               RST_STREAM 3: connection error FRAME_SIZE_ERROR: a RST_STREAM frame with 3 bytes, not 4 (consumed 0)
               RST_STREAM 5: connection error FRAME_SIZE_ERROR: a RST_STREAM frame with 5 bytes, not 4 (consumed 0)
               WINDOW_UPDATE 3: connection error FRAME_SIZE_ERROR: a WINDOW_UPDATE frame with 3 bytes, not 4 (consumed 0)
               WINDOW_UPDATE 5: connection error FRAME_SIZE_ERROR: a WINDOW_UPDATE frame with 5 bytes, not 4 (consumed 0)
               PING 7: connection error FRAME_SIZE_ERROR: a PING frame with 7 bytes, not 8 (consumed 0)
               PING 9: connection error FRAME_SIZE_ERROR: a PING frame with 9 bytes, not 8 (consumed 0)
               RST_STREAM 4: RST_STREAM 1 CANCEL
               WINDOW_UPDATE 4: WINDOW_UPDATE 1 +2147483647
               PING 8: PING ACK "01234567"
               RST_STREAM 5, header only: connection error FRAME_SIZE_ERROR: a RST_STREAM frame with 5 bytes, not 4 (consumed 0)
               WINDOW_UPDATE 3, header only: connection error FRAME_SIZE_ERROR: a WINDOW_UPDATE frame with 3 bytes, not 4 (consumed 0)
               PING 9, header only: connection error FRAME_SIZE_ERROR: a PING frame with 9 bytes, not 8 (consumed 0)
               |} )
    ; test "a SETTINGS frame with an incorrect length" (fun () ->
          let one = "\x00\x03\x00\x00\x00\x64" in
          expect
            (outcomes
               [ ("ACK and 6 bytes", raw ~kind:4 ~flags:0x1 ~stream:0 one)
               ; ("5 bytes", raw ~kind:4 ~stream:0 (String.sub one 0 5))
               ; ("7 bytes", raw ~kind:4 ~stream:0 (one ^ "\x00"))
               ; ("ACK", raw ~kind:4 ~flags:0x1 ~stream:0 "")
               ; ("12 bytes", raw ~kind:4 ~stream:0 (one ^ one))
               ; ( "ACK and 6 bytes, header only"
                 , raw ~length:6 ~kind:4 ~flags:0x1 ~stream:0 "" )
               ; ("7 bytes, header only", raw ~length:7 ~kind:4 ~stream:0 "") ] )
          @@ __POS_OF__
               {|
               ACK and 6 bytes: connection error FRAME_SIZE_ERROR: a SETTINGS frame with ACK and a payload (consumed 0)
               5 bytes: connection error FRAME_SIZE_ERROR: a SETTINGS frame with 5 bytes, not a multiple of 6 (consumed 0)
               7 bytes: connection error FRAME_SIZE_ERROR: a SETTINGS frame with 7 bytes, not a multiple of 6 (consumed 0)
               ACK: SETTINGS ACK []
               12 bytes: SETTINGS [0x3=100; 0x3=100]
               ACK and 6 bytes, header only: connection error FRAME_SIZE_ERROR: a SETTINGS frame with ACK and a payload (consumed 0)
               7 bytes, header only: connection error FRAME_SIZE_ERROR: a SETTINGS frame with 7 bytes, not a multiple of 6 (consumed 0)
               |} )
    ; test "a SETTINGS frame keeps only the known identifiers" (fun () ->
          (* The identifiers 0, 1, 6, 7, 8, 9 and 0xffff, each with the value of
             its identifier. *)
          let setting id =
            let b = Bytes.create 6 in
            Bytes.set_uint16_be b 0 id ;
            Bytes.set_int32_be b 2 (Int32.of_int id) ;
            Bytes.to_string b
          in
          expect
            (outcome
               (raw ~kind:4 ~stream:0
                  (String.concat ""
                     (List.map setting [0; 1; 6; 7; 8; 9; 0xffff]) ) ) )
          @@ __POS_OF__ {|SETTINGS [0x1=1; 0x6=6; 0x8=8]|} )
    ; test "a GOAWAY frame with less than 8 bytes" (fun () ->
          expect
            (outcomes
               [ ("7 bytes", raw ~kind:7 ~stream:0 (String.make 7 '\x00'))
               ; ("0 bytes", raw ~kind:7 ~stream:0 "")
               ; ( "8 bytes"
                 , raw ~kind:7 ~stream:0 "\x80\x00\x00\x07\x00\x00\x00\x02" )
               ; ("7 bytes, header only", raw ~length:7 ~kind:7 ~stream:0 "") ] )
          @@ __POS_OF__
               {|
               7 bytes: connection error FRAME_SIZE_ERROR: a GOAWAY frame with 7 bytes, less than 8 (consumed 0)
               0 bytes: connection error FRAME_SIZE_ERROR: a GOAWAY frame with 0 bytes, less than 8 (consumed 0)
               8 bytes: GOAWAY 7 INTERNAL_ERROR ""
               7 bytes, header only: connection error FRAME_SIZE_ERROR: a GOAWAY frame with 7 bytes, less than 8 (consumed 0)
               |} )
    ; test "a frame of an unknown type is consumed" (fun () ->
          let ping = raw ~kind:6 ~stream:0 "01234567" in
          let r =
            reader (raw ~kind:0xa ~flags:0xff ~stream:5 "payload" ^ ping)
          in
          equal frame
            (Unknown {kind= 0xa; stream= 5})
            (Frame.read r ~max_size:16384) ;
          equal int 16 (Reader.consumed r) ;
          equal frame
            (Ping {ack= false; data= "01234567"})
            (Frame.read r ~max_size:16384) ;
          expect (outcome (raw ~kind:0xff ~stream:0 ""))
          @@ __POS_OF__ {|UNKNOWN 0xff 0|} )
    ; test "unknown flags are ignored" (fun () ->
          (* Each flag that the type does not define is set. *)
          expect
            (outcomes
               [ ("DATA", raw ~kind:0 ~flags:0xf6 ~stream:1 "ab")
               ; ("HEADERS", raw ~kind:1 ~flags:0xd2 ~stream:1 "ab")
               ; ( "PRIORITY"
                 , raw ~kind:2 ~flags:0xff ~stream:1 "\x00\x00\x00\x03\x07" )
               ; ( "RST_STREAM"
                 , raw ~kind:3 ~flags:0xff ~stream:1 "\x00\x00\x00\x00" )
               ; ("SETTINGS", raw ~kind:4 ~flags:0xfe ~stream:0 "")
               ; ("PING", raw ~kind:6 ~flags:0xfe ~stream:0 "01234567")
               ; ( "GOAWAY"
                 , raw ~kind:7 ~flags:0xff ~stream:0 (String.make 8 '\x00') )
               ; ( "WINDOW_UPDATE"
                 , raw ~kind:8 ~flags:0xff ~stream:1 "\x00\x00\x00\x01" )
               ; ("CONTINUATION", raw ~kind:9 ~flags:0xfb ~stream:1 "ab") ] )
          @@ __POS_OF__
               {|
               DATA: DATA 1 "ab" flow_length=2
               HEADERS: HEADERS 1 "ab"
               PRIORITY: PRIORITY 1 priority=3/7
               RST_STREAM: RST_STREAM 1 NO_ERROR
               SETTINGS: SETTINGS []
               PING: PING "01234567"
               GOAWAY: GOAWAY 0 NO_ERROR ""
               WINDOW_UPDATE: WINDOW_UPDATE 1 +1
               CONTINUATION: CONTINUATION 1 "ab"
               |} )
    ; test "an input that ends in a frame" (fun () ->
          let input = raw ~kind:0 ~stream:1 "abc" in
          equal (list string)
            (List.init (String.length input) (fun _ -> "end of input"))
            (List.init (String.length input) (fun n ->
                 outcome (String.sub input 0 n) ) ) )
    ; test "padding boundaries" (fun () ->
          expect
            (outcomes
               [ ("pad = rest", raw ~kind:0 ~flags:0x9 ~stream:1 "\x03abc")
               ; ("pad = rest + 1", raw ~kind:0 ~flags:0x9 ~stream:1 "\x04abc")
               ; ("pad = rest - 1", raw ~kind:0 ~flags:0x9 ~stream:1 "\x02abc")
               ; ("pad 0", raw ~kind:0 ~flags:0x8 ~stream:1 "\x00abc")
               ; ( "pad 255"
                 , raw ~kind:0 ~flags:0x8 ~stream:1
                     ("\xffxy" ^ String.make 255 '\x00') )
               ; ( "HEADERS pad = rest"
                 , raw ~kind:1 ~flags:0xc ~stream:1 "\x03abc" )
               ; ( "HEADERS pad = rest + 1"
                 , raw ~kind:1 ~flags:0xc ~stream:1 "\x04abc" ) ] )
          @@ __POS_OF__
               {|
               pad = rest: DATA 1 END_STREAM "" flow_length=4
               pad = rest + 1: connection error PROTOCOL_ERROR: a padding of 4 bytes in a payload of 3 bytes (consumed 0)
               pad = rest - 1: DATA 1 END_STREAM "a" flow_length=4
               pad 0: DATA 1 "abc" flow_length=4
               pad 255: DATA 1 "xy" flow_length=258
               HEADERS pad = rest: HEADERS 1 END_HEADERS ""
               HEADERS pad = rest + 1: connection error PROTOCOL_ERROR: a padding of 4 bytes in a payload of 3 bytes (consumed 0)
               |} )
    ; test "a cancel of read in the middle of a frame consumes nothing"
        (fun () ->
          let f =
            Frame.Data
              {stream= 1; end_stream= true; data= "hello world"; flow_length= 11}
          in
          let input = encode f in
          (* 5 bytes stop in the header, 12 bytes stop in the payload. *)
          List.iter
            (fun first ->
              Sim_support.run
              @@ fun sim ->
              let pending = ref (String.sub input 0 first)
              and gate = Ivar.create () in
              let r = Reader.create (gated_flow pending gate) in
              Fiber.scope
              @@ fun s ->
              let c =
                Sim_support.call s (fun () -> Frame.read r ~max_size:16384)
              in
              is_false ~msg:"read waits" (Sim_support.ended sim c) ;
              equal int first (Reader.buffered r) ;
              Sim_support.cancel c ;
              raises Promise.Cancelled (fun () -> ignore (Sim_support.await c)) ;
              equal int 0 (Reader.consumed r) ;
              equal int first (Reader.buffered r) ;
              pending := String.sub input first (String.length input - first) ;
              Ivar.fill gate () ;
              equal frame f (Frame.read r ~max_size:16384) ;
              is_true (at_end r) )
            [5; 12] )
    ; test "a Malformed frame is consumed" (fun () ->
          let r =
            reader
              ( raw ~kind:2 ~stream:1 "\x00\x00\x00\x03"
              ^ raw ~kind:6 ~stream:0 "01234567" )
          in
          equal frame
            (Malformed
               { stream= 1
               ; code= Frame_size_error
               ; reason= "a PRIORITY frame with 4 bytes, not 5" } )
            (Frame.read r ~max_size:16384) ;
          equal frame
            (Ping {ack= false; data= "01234567"})
            (Frame.read r ~max_size:16384) ;
          is_true (at_end r) )
    ; test "write_header_block splits a block in frames of max_size bytes"
        (fun () ->
          (* The length, the type, the flags and the stream of each frame, and
             the payloads joined. *)
          let split ~end_stream ~max_size block =
            let s =
              writes (fun w ->
                  Frame.write_header_block w ~stream:7 ~end_stream ~max_size
                    block )
            in
            let rec walk off =
              if off = String.length s then []
              else
                let length =
                  (Char.code s.[off] lsl 16) lor String.get_uint16_be s (off + 1)
                in
                ( sprintf "%d 0x%x 0x%x %ld" length
                    (Char.code s.[off + 3])
                    (Char.code s.[off + 4])
                    (String.get_int32_be s (off + 5))
                , String.sub s (off + 9) length )
                :: walk (off + 9 + length)
            in
            let frames = walk 0 in
            equal string block (String.concat "" (List.map snd frames)) ;
            String.concat "; " (List.map fst frames)
          in
          let block = String.init 40000 (fun i -> Char.chr (i land 255)) in
          expect
            (String.concat "\n"
               [ split ~end_stream:true ~max_size:16384 block
               ; split ~end_stream:false ~max_size:16384 block
               ; split ~end_stream:true ~max_size:16384
                   (String.sub block 0 32768)
               ; split ~end_stream:true ~max_size:16384
                   (String.sub block 0 16384)
               ; split ~end_stream:true ~max_size:16384
                   (String.sub block 0 16385)
               ; split ~end_stream:false ~max_size:16384 "abc"
               ; split ~end_stream:true ~max_size:1 ""
               ; split ~end_stream:false ~max_size:1 "ab" ] )
          @@ __POS_OF__
               {|
               16384 0x1 0x1 7; 16384 0x9 0x0 7; 7232 0x9 0x4 7
               16384 0x1 0x0 7; 16384 0x9 0x0 7; 7232 0x9 0x4 7
               16384 0x1 0x1 7; 16384 0x9 0x4 7
               16384 0x1 0x5 7
               16384 0x1 0x1 7; 1 0x9 0x4 7
               3 0x1 0x4 7
               0 0x1 0x5 7
               1 0x1 0x0 7; 1 0x9 0x4 7
               |} )
    ; test "frames built by hand" (fun () ->
          let settings =
            ( Frame.Settings {ack= false; settings= [(0x1, 4096); (0x3, 100)]}
            , [ "00000c" (* length 12: 2 settings of 6 bytes *)
              ; "04" (* type SETTINGS *)
              ; "00" (* no flags *)
              ; "00000000" (* stream 0 *)
              ; "0001" (* SETTINGS_HEADER_TABLE_SIZE *)
              ; "00001000" (* 4096 *)
              ; "0003" (* SETTINGS_MAX_CONCURRENT_STREAMS *)
              ; "00000064" (* 100 *) ] )
          in
          let ping =
            ( Frame.Ping {ack= true; data= "01234567"}
            , [ "000008" (* length 8 *)
              ; "06" (* type PING *)
              ; "01" (* ACK *)
              ; "00000000" (* stream 0 *)
              ; "3031323334353637" (* "01234567" in ASCII *) ] )
          in
          let goaway =
            ( Frame.Goaway {last_stream= 5; code= Protocol_error; debug= "bye"}
            , [ "00000b" (* length 11: 4 + 4 + 3 *)
              ; "07" (* type GOAWAY *)
              ; "00" (* no flags *)
              ; "00000000" (* stream 0 *)
              ; "00000005" (* last stream 5 *)
              ; "00000001" (* PROTOCOL_ERROR *)
              ; "627965" (* "bye" in ASCII *) ] )
          in
          let rst =
            ( Frame.Rst_stream
                {stream= 0x7fff_ffff; code= Unknown_code 0xffff_ffff}
            , [ "000004" (* length 4 *)
              ; "03" (* type RST_STREAM *)
              ; "00" (* no flags *)
              ; "7fffffff" (* stream 2^31 - 1 *)
              ; "ffffffff" (* an unknown code *) ] )
          in
          let headers =
            ( Frame.Headers
                { stream= 3
                ; end_stream= false
                ; end_headers= true
                ; priority= Some {exclusive= true; dependency= 1; weight= 255}
                ; block= "x" }
            , [ "000006" (* length 6: 5 bytes of priority, 1 byte of block *)
              ; "01" (* type HEADERS *)
              ; "24" (* END_HEADERS (0x4) and PRIORITY (0x20) *)
              ; "00000003" (* stream 3 *)
              ; "80000001" (* the E flag and the stream 1 *)
              ; "ff" (* the weight field 255 *)
              ; "78" (* the block "x" *) ] )
          in
          let priority =
            ( Frame.Priority
                { stream= 5
                ; priority= {exclusive= false; dependency= 3; weight= 16} }
            , [ "000005" (* length 5 *)
              ; "02" (* type PRIORITY *)
              ; "00" (* no flags *)
              ; "00000005" (* stream 5 *)
              ; "00000003" (* no E flag, the stream 3 *)
              ; "10" (* the weight field 16 *) ] )
          in
          let window =
            ( Frame.Window_update {stream= 0; increment= 0x7fff_ffff}
            , [ "000004" (* length 4 *)
              ; "08" (* type WINDOW_UPDATE *)
              ; "00" (* no flags *)
              ; "00000000" (* stream 0 *)
              ; "7fffffff" (* the increment 2^31 - 1 *) ] )
          in
          List.iter
            (fun (f, fields) ->
              let bytes = of_hex (String.concat "" fields) in
              equal string bytes (encode f) ;
              equal frame f (Frame.read (reader bytes) ~max_size:16384) )
            [settings; ping; goaway; rst; headers; priority; window] ;
          (* A DATA frame of 70000 bytes (0x011170) on stream 3, with
             END_STREAM. *)
          let data = String.make 70000 'd' in
          let f =
            Frame.Data {stream= 3; end_stream= true; data; flow_length= 70000}
          in
          let bytes =
            of_hex
              (String.concat ""
                 [ "011170" (* length 70000 *)
                 ; "00" (* type DATA *)
                 ; "01" (* END_STREAM *)
                 ; "00000003" (* stream 3 *) ] )
            ^ data
          in
          equal string bytes (encode f) ;
          equal frame f (Frame.read (reader bytes) ~max_size:70000) ;
          equal string "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n" Frame.preface )
    ; test "write refuses a frame that it cannot write, and keeps no part of it"
        (fun () ->
          (* The flow refuses more than 64 KiB of output. Thus a write that does
             not end (for example frames with no payload, for ever) raises
             [Failure]. *)
          let invalid f =
            let out = Buffer.create 64 in
            let write buf off len =
              if Buffer.length out + len > 65536 then failwith "endless output" ;
              Buffer.add_subbytes out buf off len
            in
            let flow =
              Flow.make
                ~read:(fun _ _ _ -> 0)
                ~read_now:(fun _ _ _ -> Some 0)
                ~write ~readable:(always ()) ~shutdown_send:ignore ~close:ignore
                ~set_read_deadline:ignore ~set_write_deadline:ignore ()
            in
            let w = Writer.create ~max_frame:(1 lsl 25) flow in
            Frame.write w (Ping {ack= false; data= "before.."}) ;
            raises_match
              (function Invalid_argument _ -> true | _ -> false)
              (fun () -> f w) ;
            Frame.write w (Ping {ack= true; data= "after..."}) ;
            Writer.flush w ;
            equal string
              ( encode (Ping {ack= false; data= "before.."})
              ^ encode (Ping {ack= true; data= "after..."}) )
              (Buffer.contents out)
          in
          let data stream =
            Frame.Data {stream; end_stream= false; data= "x"; flow_length= 1}
          in
          List.iter
            (fun f -> invalid (fun w -> Frame.write w f))
            [ Push_promise {stream= 1}
            ; Unknown {kind= 0xa; stream= 1}
            ; Malformed {stream= 1; code= Cancel; reason= ""}
            ; data 0x8000_0000
            ; data (-1)
            ; Settings {ack= true; settings= [(0x1, 0)]}
            ; data 0
            ; Headers
                { stream= 0
                ; end_stream= false
                ; end_headers= true
                ; priority= None
                ; block= "" }
            ; Priority
                { stream= 0
                ; priority= {exclusive= false; dependency= 1; weight= 0} }
            ; Rst_stream {stream= 0; code= Cancel}
            ; Continuation {stream= 0; end_headers= true; block= ""}
            ; Ping {ack= false; data= "0123456"}
            ; Ping {ack= false; data= "012345678"}
            ; Data
                { stream= 1
                ; end_stream= false
                ; data= String.make (1 lsl 24) 'x'
                ; flow_length= 0 } ] ;
          invalid (fun w ->
              Frame.write_header_block w ~stream:1 ~end_stream:false ~max_size:0
                "a" ) ;
          invalid (fun w ->
              Frame.write_data w ~stream:1 ~end_stream:false (Bytes.create 4) 2
                3 ) ;
          (* The largest payload and stream. *)
          let max_data =
            Frame.Data
              { stream= 0x7fff_ffff
              ; end_stream= false
              ; data= String.make 0xff_ffff 'x'
              ; flow_length= 0xff_ffff }
          in
          let bytes =
            writes ~max_frame:(1 lsl 25) (fun w -> Frame.write w max_data)
          in
          equal string (of_hex "ffffff 00 00 7fffffff") (String.sub bytes 0 9) ;
          equal int (9 + 0xff_ffff) (String.length bytes) ) ]

let () = exit (Windtrap.run "http2" [error_tests; frame_tests])
