type ctx = {config: Config.t; conn: Conn_info.t; is_stopping: unit -> bool}

type t = ctx -> Flow.t -> Handler.t -> unit
