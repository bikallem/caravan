(** HTTP/1.1 server and client library built on
    {{:https://erratique.ch/software/affect}affect}. *)

(** {1:core Core types} *)

module Method = Method
module Version = Version
module Status = Status
module Headers = Headers

(**/**)

(** Internal modules, exposed for tests. Not part of the public API. *)
module Private : sig
  module Date = Date
end
