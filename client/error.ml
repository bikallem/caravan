type t =
  | Connect of string
  | Timeout of [`Connect | `Response | `Read | `Write]
  | Closed
  | Truncated
  | Bad_response of string
  | Unsupported of string

exception Error of t

let to_string = function
  | Connect s ->
      "cannot connect: " ^ s
  | Timeout `Connect ->
      "connect timeout"
  | Timeout `Response ->
      "response timeout"
  | Timeout `Read ->
      "read timeout"
  | Timeout `Write ->
      "write timeout"
  | Closed ->
      "the connection closed before the response"
  | Truncated ->
      "the response body is truncated"
  | Bad_response s ->
      "bad response: " ^ s
  | Unsupported s ->
      "unsupported: " ^ s

let () =
  Printexc.register_printer (function
    | Error e ->
        Some ("Caravan_client.Error: " ^ to_string e)
    | (_ [@mutate off "Printexc skips a printer that raises"]) ->
        None )

let fail e = raise (Error e)

(* The refusal of a request to a closed client. *)
let refuse_closed () = invalid_arg "Caravan_client: the client is closed"

(* The text of an error of a connect or of a lookup. A refusal and a reset have
   the text of strerror(3). [No_resources] has the name of the errno, for
   example EMFILE. [Other] has the text that weft gives: the name of an errno
   (ETIMEDOUT), a text with a reason ("EINVAL: a NUL byte in a string"), or the
   gai_strerror(3) text of a lookup. The other constructors of [Net.error] do
   not come from a connect or a lookup in weft at this time. *)
let net_text : Weft.Net.error -> string = function
  | Refused ->
      "Connection refused"
  | (Reset [@mutate off "the simulator gives no reset to a connect"]) ->
      "Connection reset by peer"
  | No_resources s ->
      "no resources (" ^ s ^ ")"
  | Other s ->
      s
  | (_ [@mutate off "weft gives no other constructor for a connect or a lookup"])
    ->
      "error"
