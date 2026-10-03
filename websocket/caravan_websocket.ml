let guid = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
let accept_key key = Base64.encode (Sha1.digest (key ^ guid))

module Private = struct
  module Sha1 = Sha1
  module Base64 = Base64
  module Frame = Frame
end
