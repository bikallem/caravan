type code =
  | No_error
  | Protocol_error
  | Internal_error
  | Flow_control_error
  | Settings_timeout
  | Stream_closed
  | Frame_size_error
  | Refused_stream
  | Cancel
  | Compression_error
  | Connect_error
  | Enhance_your_calm
  | Inadequate_security
  | Http_1_1_required
  | Unknown_code of int

(* RFC 9113 section 7, in the order of the values. *)
let known =
  [| No_error
   ; Protocol_error
   ; Internal_error
   ; Flow_control_error
   ; Settings_timeout
   ; Stream_closed
   ; Frame_size_error
   ; Refused_stream
   ; Cancel
   ; Compression_error
   ; Connect_error
   ; Enhance_your_calm
   ; Inadequate_security
   ; Http_1_1_required |]

let code_to_int = function
  | No_error ->
      0x0
  | Protocol_error ->
      0x1
  | Internal_error ->
      0x2
  | Flow_control_error ->
      0x3
  | Settings_timeout ->
      0x4
  | Stream_closed ->
      0x5
  | Frame_size_error ->
      0x6
  | Refused_stream ->
      0x7
  | Cancel ->
      0x8
  | Compression_error ->
      0x9
  | Connect_error ->
      0xa
  | Enhance_your_calm ->
      0xb
  | Inadequate_security ->
      0xc
  | Http_1_1_required ->
      0xd
  | Unknown_code n ->
      n

let code_of_int n =
  if n < 0 || n > 0xffff_ffff then
    invalid_arg (Printf.sprintf "Error.code_of_int: %d" n) ;
  if n < Array.length known then known.(n) else Unknown_code n

let name = function
  | No_error ->
      "NO_ERROR"
  | Protocol_error ->
      "PROTOCOL_ERROR"
  | Internal_error ->
      "INTERNAL_ERROR"
  | Flow_control_error ->
      "FLOW_CONTROL_ERROR"
  | Settings_timeout ->
      "SETTINGS_TIMEOUT"
  | Stream_closed ->
      "STREAM_CLOSED"
  | Frame_size_error ->
      "FRAME_SIZE_ERROR"
  | Refused_stream ->
      "REFUSED_STREAM"
  | Cancel ->
      "CANCEL"
  | Compression_error ->
      "COMPRESSION_ERROR"
  | Connect_error ->
      "CONNECT_ERROR"
  | Enhance_your_calm ->
      "ENHANCE_YOUR_CALM"
  | Inadequate_security ->
      "INADEQUATE_SECURITY"
  | Http_1_1_required ->
      "HTTP_1_1_REQUIRED"
  | Unknown_code n ->
      Printf.sprintf "0x%x" n

let pp_code ppf c = Format.pp_print_string ppf (name c)

exception Hpack of string
