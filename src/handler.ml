type t = Request.t -> Response.t
type middleware = t -> t

let compose ms h = List.fold_right (fun m h -> m h) ms h
let not_found _ = Response.string ~status:Status.not_found "Not Found\n"
