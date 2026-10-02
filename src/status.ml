type t = int

let v n =
  if n < 100 || n > 599
  then invalid_arg (Printf.sprintf "Status.v: %d is not in [100;599]" n)
  else n

let code s = s

let reason = function
| 100 -> "Continue"
| 101 -> "Switching Protocols"
| 103 -> "Early Hints"
| 200 -> "OK"
| 201 -> "Created"
| 202 -> "Accepted"
| 203 -> "Non-Authoritative Information"
| 204 -> "No Content"
| 205 -> "Reset Content"
| 206 -> "Partial Content"
| 300 -> "Multiple Choices"
| 301 -> "Moved Permanently"
| 302 -> "Found"
| 303 -> "See Other"
| 304 -> "Not Modified"
| 307 -> "Temporary Redirect"
| 308 -> "Permanent Redirect"
| 400 -> "Bad Request"
| 401 -> "Unauthorized"
| 402 -> "Payment Required"
| 403 -> "Forbidden"
| 404 -> "Not Found"
| 405 -> "Method Not Allowed"
| 406 -> "Not Acceptable"
| 407 -> "Proxy Authentication Required"
| 408 -> "Request Timeout"
| 409 -> "Conflict"
| 410 -> "Gone"
| 411 -> "Length Required"
| 412 -> "Precondition Failed"
| 413 -> "Content Too Large"
| 414 -> "URI Too Long"
| 415 -> "Unsupported Media Type"
| 416 -> "Range Not Satisfiable"
| 417 -> "Expectation Failed"
| 421 -> "Misdirected Request"
| 422 -> "Unprocessable Content"
| 426 -> "Upgrade Required"
| 428 -> "Precondition Required"
| 429 -> "Too Many Requests"
| 431 -> "Request Header Fields Too Large"
| 451 -> "Unavailable For Legal Reasons"
| 500 -> "Internal Server Error"
| 501 -> "Not Implemented"
| 502 -> "Bad Gateway"
| 503 -> "Service Unavailable"
| 504 -> "Gateway Timeout"
| 505 -> "HTTP Version Not Supported"
| 511 -> "Network Authentication Required"
| _ -> ""

let kind s =
  match s / 100 with
  | 1 -> `Informational
  | 2 -> `Success
  | 3 -> `Redirection
  | 4 -> `Client_error
  | _ -> `Server_error

let equal = Int.equal
let compare = Int.compare

let pp ppf s =
  match reason s with
  | "" -> Format.pp_print_int ppf s
  | r -> Format.fprintf ppf "%d %s" s r

let continue = 100
let switching_protocols = 101
let early_hints = 103
let ok = 200
let created = 201
let accepted = 202
let non_authoritative_information = 203
let no_content = 204
let reset_content = 205
let partial_content = 206
let multiple_choices = 300
let moved_permanently = 301
let found = 302
let see_other = 303
let not_modified = 304
let temporary_redirect = 307
let permanent_redirect = 308
let bad_request = 400
let unauthorized = 401
let payment_required = 402
let forbidden = 403
let not_found = 404
let method_not_allowed = 405
let not_acceptable = 406
let proxy_authentication_required = 407
let request_timeout = 408
let conflict = 409
let gone = 410
let length_required = 411
let precondition_failed = 412
let content_too_large = 413
let uri_too_long = 414
let unsupported_media_type = 415
let range_not_satisfiable = 416
let expectation_failed = 417
let misdirected_request = 421
let unprocessable_content = 422
let upgrade_required = 426
let precondition_required = 428
let too_many_requests = 429
let request_header_fields_too_large = 431
let unavailable_for_legal_reasons = 451
let internal_server_error = 500
let not_implemented = 501
let bad_gateway = 502
let service_unavailable = 503
let gateway_timeout = 504
let http_version_not_supported = 505
let network_authentication_required = 511
