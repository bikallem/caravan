(* The tests of caravan.http2: the error codes. *)

open Windtrap
module Error = Caravan_http2.Private.Error

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
              equal Testable.bool true (c = Error.Unknown_code n) ;
              equal Testable.int n (Error.code_to_int c) )
            [0xe; 0xff; 0xffff_ffff] ;
          equal Testable.string "0xff"
            (Format.asprintf "%a" Error.pp_code (Error.code_of_int 0xff)) )
    ; test "a value out of 32 bits is refused" (fun () ->
          raises (Invalid_argument "Error.code_of_int: -1") (fun () ->
              Error.code_of_int (-1) ) ;
          raises (Invalid_argument "Error.code_of_int: 4294967296") (fun () ->
              Error.code_of_int 0x1_0000_0000 ) ) ]

let () = exit (Windtrap.run "http2" [error_tests])
