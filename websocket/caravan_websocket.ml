let guid = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
let accept_key key = Base64.encode (Sha1.digest (key ^ guid))

type t = Connection.t
type message = Connection.message = Text of string | Binary of string

exception Closed = Connection.Closed

let recv = Connection.recv
let send = Connection.send
let ping = Connection.ping
let close = Connection.close

module Private = struct
  module Sha1 = Sha1
  module Base64 = Base64
  module Frame = Frame

  let of_flow = Connection.make
end
