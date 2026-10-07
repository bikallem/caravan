type t = [`Connection | `Resources | `Listener]

(* Linux's EPROTO and ENONET, which Unix has no constructor for. *)
let eproto = 71

let enonet = 64

let classify : Unix.error -> t = function
  | ECONNABORTED
  | EINTR
  | ENETDOWN
  | ENOPROTOOPT
  | EHOSTDOWN
  | EHOSTUNREACH
  | EOPNOTSUPP
  | ENETUNREACH
  | EPERM ->
      `Connection
  | EUNKNOWNERR n when n = eproto || n = enonet ->
      `Connection
  | EMFILE | ENFILE | ENOBUFS | ENOMEM ->
      `Resources
  | _ ->
      `Listener
