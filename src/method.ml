type t =
| GET
| HEAD
| POST
| PUT
| DELETE
| CONNECT
| OPTIONS
| TRACE
| PATCH
| Other of string

let of_string = function
| "GET" -> GET
| "HEAD" -> HEAD
| "POST" -> POST
| "PUT" -> PUT
| "DELETE" -> DELETE
| "CONNECT" -> CONNECT
| "OPTIONS" -> OPTIONS
| "TRACE" -> TRACE
| "PATCH" -> PATCH
| s when Token.is_token s -> Other s
| s -> invalid_arg (Printf.sprintf "Method.of_string: %S is not a token" s)

let to_string = function
| GET -> "GET"
| HEAD -> "HEAD"
| POST -> "POST"
| PUT -> "PUT"
| DELETE -> "DELETE"
| CONNECT -> "CONNECT"
| OPTIONS -> "OPTIONS"
| TRACE -> "TRACE"
| PATCH -> "PATCH"
| Other s -> s

let compare (a: t) (b: t) = String.compare (to_string a) (to_string b)
let equal (a: t) (b: t) = String.equal (to_string a) (to_string b)
let is_safe = function GET | HEAD | OPTIONS| TRACE -> true | _ -> false
let is_idempotent = function PUT | DELETE -> true | m -> is_safe m
let pp ppf v = Format.pp_print_string ppf (to_string v)