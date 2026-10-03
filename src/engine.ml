type ctx = {
  config : Config.t;
  conn : Conn_info.t;
  stopping : unit Affect.Action.t;
  is_stopping : unit -> bool;
}

type t = ctx -> Flow.t -> Handler.t -> unit
