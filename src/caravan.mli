(** Caravan, an HTTP server built on weft. Code that uses caravan runs in a weft
    world: [Weft_sim] in tests, [Weft_os] or [Weft_linux] in production, for
    example with [Server.run]. *)

(** {1:core Core types} *)

module Method = Method
module Version = Version
module Status = Status
module Headers = Headers
module Flow = Flow
module Body = Body
module Conn_info = Conn_info
module Attrs = Attrs
module Form = Form
module Cookie = Cookie
module Multipart = Multipart
module Request = Request
module Response = Response
module Handler = Handler

(** {1:transport Transport} *)

module Tcp = Tcp
module Config = Config
module Engine = Engine
module Server = Server

(**/**)

(** Internal modules, exposed for tests. Not part of the public API. *)
module Private : sig
  module Date = Date
  module Percent = Percent
  module Base64 = Base64
  module Token = Token
  module Deadline = Deadline
  module Cancel = Cancel
  module Buf_reader = Buf_reader
  module Buf_writer = Buf_writer
  module H1_error = H1_error
  module H1_parse = H1_parse
  module H1_body = H1_body
  module H1_write = H1_write
  module H1_server = H1_server
  module Accept_error = Accept_error
end
