type t = [`Connection | `Resources | `Listener]

let classify : Weft.Net.error -> t = function
  | No_resources _ ->
      `Resources
  | Other
      ( "EINTR"
      | "ENETDOWN"
      | "ENOPROTOOPT"
      | "EHOSTUNREACH"
      | "EOPNOTSUPP"
      | "ENETUNREACH"
      | "EPERM"
      | "EPROTO"
      | "ENONET"
      | "EHOSTDOWN" ) ->
      `Connection
  | _ ->
      `Listener
