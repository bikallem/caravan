type t =
  { id: int
  ; peer: Weft.Net.addr
  ; local: Weft.Net.addr
  ; scheme: [`Http | `Https]
  ; alpn: string option
  ; stopping: unit Weft.event }

let make ~id ~peer ~local ?(scheme = `Http) ?alpn ?(stopping = Weft.never) () =
  {id; peer; local; scheme; alpn; stopping}

let with_tls ?alpn c = {c with scheme= `Https; alpn}

let id c = c.id

let peer c = c.peer

let local c = c.local

let scheme c = c.scheme

let alpn c = c.alpn

let stopping c = c.stopping

let pp_addr ppf = function
  | Weft.Net.Unix path ->
      Format.pp_print_string ppf path
  | Weft.Net.Tcp (host, port) ->
      if String.contains host ':' then Format.fprintf ppf "[%s]:%d" host port
      else Format.fprintf ppf "%s:%d" host port

let pp ppf c =
  Format.fprintf ppf "#%d %a -> %a" c.id pp_addr c.peer pp_addr c.local
