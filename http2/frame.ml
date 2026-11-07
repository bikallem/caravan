module Reader = Weft_flow.Reader
module Writer = Weft_flow.Writer

type code = Error.code

type priority = {exclusive: bool; dependency: int; weight: int}

type setting = int * int

type t =
  | Data of {stream: int; end_stream: bool; data: string; flow_length: int}
  | Headers of
      { stream: int
      ; end_stream: bool
      ; end_headers: bool
      ; priority: priority option
      ; block: string }
  | Priority of {stream: int; priority: priority}
  | Rst_stream of {stream: int; code: code}
  | Settings of {ack: bool; settings: setting list}
  | Push_promise of {stream: int}
  | Ping of {ack: bool; data: string}
  | Goaway of {last_stream: int; code: code; debug: string}
  | Window_update of {stream: int; increment: int}
  | Continuation of {stream: int; end_headers: bool; block: string}
  | Unknown of {kind: int; stream: int}
  | Malformed of {stream: int; code: code; reason: string}

exception Connection_error of {code: code; reason: string}

let preface = "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n"

(* The frame header: the length (24 bits), the type, the flags, then the
   reserved bit and the stream identifier (31 bits). *)
let header_size = 9

(* The frame types of RFC 9113 section 6. OCaml patterns need literals, thus the
   code uses the values with the name in a comment. *)
let kind_name = function
  | 0x0 ->
      "DATA"
  | 0x1 ->
      "HEADERS"
  | 0x2 ->
      "PRIORITY"
  | 0x3 ->
      "RST_STREAM"
  | 0x4 ->
      "SETTINGS"
  | 0x5 ->
      "PUSH_PROMISE"
  | 0x6 ->
      "PING"
  | 0x7 ->
      "GOAWAY"
  | 0x8 ->
      "WINDOW_UPDATE"
  | 0x9 ->
      "CONTINUATION"
  | kind ->
      Printf.sprintf "0x%x" kind

(* The types of the frames that are on a stream, never on stream 0. *)
let is_stream_kind = function
  | 0x0 | 0x1 | 0x2 | 0x3 | 0x5 | 0x9 ->
      true
  | _ ->
      false

(* The flags. ACK and END_STREAM have the same bit. *)
let ack_flag = 0x1

let end_stream_flag = 0x1

let end_headers_flag = 0x4

let padded_flag = 0x8

let priority_flag = 0x20

let connection_error code reason = raise (Connection_error {code; reason})

(* {1 Read} *)

let get_uint32 r off =
  (Reader.get_uint16_be r off lsl 16) lor Reader.get_uint16_be r (off + 2)

(* A 31-bit field after a reserved bit. *)
let get_uint31 r off = get_uint32 r off land 0x7fff_ffff

let get_priority r off =
  let v = get_uint32 r off in
  { exclusive= v land 0x8000_0000 <> 0
  ; dependency= v land 0x7fff_ffff
  ; weight= Reader.get_uint8 r (off + 4) }

(* The checks that need only the header. They come before the wait for the
   payload. *)
let check_header ~kind ~flags ~length ~stream =
  if is_stream_kind kind && stream = 0 then
    connection_error Protocol_error
      (Printf.sprintf "a %s frame on stream 0" (kind_name kind)) ;
  match kind with
  | (0x4 | 0x6 | 0x7 (* SETTINGS, PING, GOAWAY *)) when stream <> 0 ->
      connection_error Protocol_error
        (Printf.sprintf "a %s frame on stream %d" (kind_name kind) stream)
  | (0x3 | 0x8 (* RST_STREAM, WINDOW_UPDATE *)) when length <> 4 ->
      connection_error Frame_size_error
        (Printf.sprintf "a %s frame with %d bytes, not 4" (kind_name kind)
           length )
  | 0x6 (* PING *) when length <> 8 ->
      connection_error Frame_size_error
        (Printf.sprintf "a PING frame with %d bytes, not 8" length)
  | 0x4 (* SETTINGS *) when flags land ack_flag <> 0 && length <> 0 ->
      connection_error Frame_size_error
        "a SETTINGS frame with ACK and a payload"
  | 0x4 (* SETTINGS *) when length mod 6 <> 0 ->
      connection_error Frame_size_error
        (Printf.sprintf "a SETTINGS frame with %d bytes, not a multiple of 6"
           length )
  | 0x7 (* GOAWAY *) when length < 8 ->
      connection_error Frame_size_error
        (Printf.sprintf "a GOAWAY frame with %d bytes, less than 8" length)
  | _ ->
      ()

(* The offset and the length of the content of a DATA or HEADERS frame: the
   payload without the Pad Length byte, the priority of HEADERS and the
   padding. *)
let content r ~flags ~length ~priority =
  let pad_field = if flags land padded_flag <> 0 then 1 else 0 in
  let fixed = if priority then 5 else 0 in
  if length < pad_field then
    connection_error Frame_size_error "a PADDED frame with no Pad Length byte" ;
  if length - pad_field < fixed then
    connection_error Frame_size_error
      "a HEADERS frame with PRIORITY and less than 5 bytes" ;
  let pad = if pad_field = 1 then Reader.get_uint8 r header_size else 0 in
  let rest = length - pad_field - fixed in
  if pad > rest then
    connection_error Protocol_error
      (Printf.sprintf "a padding of %d bytes in a payload of %d bytes" pad rest) ;
  (header_size + pad_field + fixed, rest - pad)

(* The identifiers of the settings that RFC 9113 section 6.5.2 defines (1 to 6)
   and SETTINGS_ENABLE_CONNECT_PROTOCOL of RFC 8441 (8). *)
let is_known_setting id = (id >= 1 && id <= 6) || id = 8

(* The known settings of the payload, in order. RFC 9113 section 6.5.2: an
   endpoint ignores a setting that it does not know. *)
let decode_settings r ~length =
  let rec settings i acc =
    if i < 0 then acc
    else
      let off = header_size + (6 * i) in
      let id = Reader.get_uint16_be r off in
      let acc =
        if is_known_setting id then (id, get_uint32 r (off + 2)) :: acc else acc
      in
      settings (i - 1) acc
  in
  settings ((length / 6) - 1) []

(* Decodes the payload, which the reader holds after the header. *)
let decode r ~kind ~flags ~length ~stream =
  let p = header_size in
  let has flag = flags land flag <> 0 in
  match kind with
  | 0x0 (* DATA *) ->
      let off, n = content r ~flags ~length ~priority:false in
      Data
        { stream
        ; end_stream= has end_stream_flag
        ; data= Reader.sub_string r off n
        ; flow_length= length }
  | 0x1 (* HEADERS *) ->
      let priority = has priority_flag in
      let off, n = content r ~flags ~length ~priority in
      Headers
        { stream
        ; end_stream= has end_stream_flag
        ; end_headers= has end_headers_flag
        ; priority= (if priority then Some (get_priority r (off - 5)) else None)
        ; block= Reader.sub_string r off n }
  | 0x2 (* PRIORITY *) when length <> 5 ->
      Malformed
        { stream
        ; code= Frame_size_error
        ; reason= Printf.sprintf "a PRIORITY frame with %d bytes, not 5" length
        }
  | 0x2 (* PRIORITY *) ->
      let priority = get_priority r p in
      if priority.dependency = stream then
        Malformed
          { stream
          ; code= Protocol_error
          ; reason= "a PRIORITY frame that depends on its own stream" }
      else Priority {stream; priority}
  | 0x3 (* RST_STREAM *) ->
      Rst_stream {stream; code= Error.code_of_int (get_uint32 r p)}
  | 0x4 (* SETTINGS *) ->
      Settings {ack= has ack_flag; settings= decode_settings r ~length}
  | 0x5 (* PUSH_PROMISE *) ->
      Push_promise {stream}
  | 0x6 (* PING *) ->
      Ping {ack= has ack_flag; data= Reader.sub_string r p 8}
  | 0x7 (* GOAWAY *) ->
      Goaway
        { last_stream= get_uint31 r p
        ; code= Error.code_of_int (get_uint32 r (p + 4))
        ; debug= Reader.sub_string r (p + 8) (length - 8) }
  | 0x8 (* WINDOW_UPDATE *) ->
      Window_update {stream; increment= get_uint31 r p}
  | 0x9 (* CONTINUATION *) ->
      Continuation
        { stream
        ; end_headers= has end_headers_flag
        ; block= Reader.sub_string r p length }
  | kind ->
      Unknown {kind; stream}

let read r ~max_size =
  Reader.ensure r header_size ;
  let length = (Reader.get_uint8 r 0 lsl 16) lor Reader.get_uint16_be r 1 in
  let kind = Reader.get_uint8 r 3 and flags = Reader.get_uint8 r 4 in
  let stream = get_uint31 r 5 in
  if length > max_size then
    connection_error Frame_size_error
      (Printf.sprintf "a frame of %d bytes, more than %d" length max_size) ;
  check_header ~kind ~flags ~length ~stream ;
  Reader.ensure r (header_size + length) ;
  let frame = decode r ~kind ~flags ~length ~stream in
  Reader.consume r (header_size + length) ;
  frame

(* {1 Write} *)

let write_uint32 w v =
  Writer.write_uint16_be w (v lsr 16) ;
  Writer.write_uint16_be w (v land 0xffff)

(* Writes a frame: [payload ()] writes the payload. The writer computes the
   length. If a write raises, the frame is dropped. *)
let write_frame w ~kind ~flags ~stream payload =
  if stream lsr 31 <> 0 then
    invalid_arg (Printf.sprintf "Frame.write: the stream %d" stream) ;
  if is_stream_kind kind && stream = 0 then
    invalid_arg
      (Printf.sprintf "Frame.write: a %s frame on stream 0" (kind_name kind)) ;
  Writer.start_frame w ~header:header_size ;
  try
    payload () ;
    let length = Writer.end_frame w in
    Writer.write_uint8 w (length lsr 16) ;
    Writer.write_uint16_be w (length land 0xffff) ;
    Writer.write_uint8 w kind ;
    Writer.write_uint8 w flags ;
    write_uint32 w stream
  with e -> Writer.abort_frame w ; raise e

let flag cond flag = if cond then flag else 0

let write_priority w {exclusive; dependency; weight} =
  write_uint32 w (flag exclusive 0x8000_0000 lor dependency) ;
  Writer.write_uint8 w weight

let write_data w ~stream ~end_stream buf off len =
  write_frame w ~kind:0x0 (* DATA *) ~flags:(flag end_stream end_stream_flag)
    ~stream (fun () -> Writer.write_bytes w buf off len )

let write w = function
  | Data {stream; end_stream; data; flow_length= _} ->
      write_data w ~stream ~end_stream
        (Bytes.unsafe_of_string data)
        0 (String.length data)
  | Headers {stream; end_stream; end_headers; priority; block} ->
      let flags =
        flag end_stream end_stream_flag
        lor flag end_headers end_headers_flag
        lor flag (Option.is_some priority) priority_flag
      in
      write_frame w ~kind:0x1 (* HEADERS *) ~flags ~stream (fun () ->
          Option.iter (write_priority w) priority ;
          Writer.write_string w block )
  | Priority {stream; priority} ->
      write_frame w ~kind:0x2 (* PRIORITY *) ~flags:0 ~stream (fun () ->
          write_priority w priority )
  | Rst_stream {stream; code} ->
      write_frame w ~kind:0x3 (* RST_STREAM *) ~flags:0 ~stream (fun () ->
          write_uint32 w (Error.code_to_int code) )
  | Settings {ack; settings} ->
      if ack && settings <> [] then
        invalid_arg "Frame.write: a SETTINGS frame with ACK and settings" ;
      write_frame w ~kind:0x4 (* SETTINGS *)
        ~flags:(flag ack ack_flag) ~stream:0 (fun () ->
          List.iter
            (fun (id, value) ->
              Writer.write_uint16_be w id ;
              write_uint32 w value )
            settings )
  | Ping {ack; data} ->
      if String.length data <> 8 then
        invalid_arg "Frame.write: PING data must have 8 bytes" ;
      write_frame w ~kind:0x6 (* PING *) ~flags:(flag ack ack_flag) ~stream:0
        (fun () -> Writer.write_string w data )
  | Goaway {last_stream; code; debug} ->
      write_frame w ~kind:0x7 (* GOAWAY *) ~flags:0 ~stream:0 (fun () ->
          write_uint32 w last_stream ;
          write_uint32 w (Error.code_to_int code) ;
          Writer.write_string w debug )
  | Window_update {stream; increment} ->
      write_frame w ~kind:0x8 (* WINDOW_UPDATE *) ~flags:0 ~stream (fun () ->
          write_uint32 w increment )
  | Continuation {stream; end_headers; block} ->
      write_frame w ~kind:0x9 (* CONTINUATION *)
        ~flags:(flag end_headers end_headers_flag) ~stream (fun () ->
          Writer.write_string w block )
  | Push_promise _ | Unknown _ | Malformed _ ->
      invalid_arg
        "Frame.write: a PUSH_PROMISE, unknown or malformed frame is not written"

let write_header_block w ~stream ~end_stream ~max_size block =
  if max_size < 1 then
    invalid_arg
      (Printf.sprintf "Frame.write_header_block: max_size %d" max_size) ;
  let total = String.length block in
  let buf = Bytes.unsafe_of_string block in
  let rec frames off =
    let len = Int.min max_size (total - off) in
    let last = off + len = total in
    let flags = flag last end_headers_flag in
    (* The first frame is a HEADERS frame, the next ones are CONTINUATION
       frames. *)
    let kind, flags =
      if off = 0 then (0x1, flags lor flag end_stream end_stream_flag)
      else (0x9, flags)
    in
    write_frame w ~kind ~flags ~stream (fun () ->
        Writer.write_bytes w buf off len ) ;
    if not last then frames (off + len)
  in
  frames 0
