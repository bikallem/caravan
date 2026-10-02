(** HTTP/1.1 server and client library built on
    {{:https://erratique.ch/software/affect}affect}. *)

(** {1:core Core types} *)

module Method = Method
module Version = Version
module Status = Status
module Headers = Headers
module Flow = Flow
module Body = Body
module Conn_info = Conn_info
module Request = Request
module Response = Response
module Handler = Handler

(**/**)

(** Internal modules, exposed for tests. Not part of the public API. *)
module Private : sig
  module Date = Date
  module Buf_reader = Buf_reader
  module H1_error = H1_error
  module H1_parse = H1_parse
end
