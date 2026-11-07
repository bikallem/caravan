(* The test helpers of caravan.http2 that more than one suite uses. See
   h2_support.mli. *)

open Caravan_http2.Private

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
  Windtrap.Testable.structural ~pp:(fun ppf f ->
      Format.pp_print_string ppf (show f) )

let raw ?length ~kind ?(flags = 0) ~stream payload =
  let length = Option.value length ~default:(String.length payload) in
  let b = Bytes.create 9 in
  Bytes.set_uint8 b 0 (length lsr 16) ;
  Bytes.set_uint16_be b 1 (length land 0xffff) ;
  Bytes.set_uint8 b 3 kind ;
  Bytes.set_uint8 b 4 flags ;
  Bytes.set_int32_be b 5 (Int32.of_int stream) ;
  Bytes.to_string b ^ payload
