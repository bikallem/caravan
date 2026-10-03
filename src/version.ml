type t = Http_1_0 | Http_1_1 | Http_2 | Http_3

let to_string = function
| Http_1_0 -> "HTTP/1.0"
| Http_1_1 -> "HTTP/1.1"
| Http_2 -> "HTTP/2"
| Http_3 ->"HTTP/3"

let of_string = function
| "HTTP/1.0" -> Some Http_1_0
| "HTTP/1.1" -> Some Http_1_1
| "HTTP/2" -> Some Http_2
| "HTTP/3" -> Some Http_3
| _ -> None

let compare (a: t) (b:t) = Stdlib.compare a b
let equal (a: t) (b:t) = compare a b = 0
let pp ppf v = Format.pp_print_string ppf (to_string v)
