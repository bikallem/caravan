type t =
| Connect of string
| Timeout of [ `Connect | `Response | `Read | `Write ]
| Closed
| Truncated
| Bad_response of string
| Unsupported of string

exception Error of t

let to_string = function
| Connect s -> "cannot connect: " ^ s
| Timeout `Connect -> "connect timeout"
| Timeout `Response -> "response timeout"
| Timeout `Read -> "read timeout"
| Timeout `Write -> "write timeout"
| Closed -> "the connection closed before the response"
| Truncated -> "the response body is truncated"
| Bad_response s -> "bad response: " ^ s
| Unsupported s -> "unsupported: " ^ s

let () =
  Printexc.register_printer (function
    | Error e -> Some ("Caravan_client.Error: " ^ to_string e)
    | _ -> None)

let fail e = raise (Error e)
