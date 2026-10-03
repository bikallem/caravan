type t = {
  id : int;
  peer : Unix.sockaddr;
  local : Unix.sockaddr;
  scheme : [ `Http | `Https ];
  alpn : string option;
}

let make ~id ~peer ~local ?(scheme = `Http) ?alpn () =
  { id; peer; local; scheme; alpn }

let id c = c.id
let peer c = c.peer
let local c = c.local
let scheme c = c.scheme
let alpn c = c.alpn

let pp_addr ppf = function
| Unix.ADDR_UNIX path -> Format.pp_print_string ppf path
| Unix.ADDR_INET (a, port) ->
    let a = Unix.string_of_inet_addr a in
    if String.contains a ':'
    then Format.fprintf ppf "[%s]:%d" a port
    else Format.fprintf ppf "%s:%d" a port

let pp ppf c = Format.fprintf ppf "#%d %a -> %a" c.id pp_addr c.peer pp_addr c.local
