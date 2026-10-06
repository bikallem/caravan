module Config = Config

type error = Error.t =
| Connect of string
| Timeout of [ `Connect | `Response | `Read | `Write ]
| Closed
| Truncated
| Bad_response of string
| Unsupported of string

exception Error = Error.Error

module Private = struct
  module Conn = Conn
  module Writers = Writers
  module Exchange = Exchange
  module Resolver = Resolver
  module Connect = Connect
end
