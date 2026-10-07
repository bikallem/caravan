module Mtime = Affect_unix.Mtime

type t = {
  flow : Caravan.Flow.t;
  host : string;
  port : int;
  id : int;
  mutable released_at : Mtime.t;
}

let next_id = Atomic.make 0

let make ~host ~port flow =
  { flow; host; port; id = Atomic.fetch_and_add next_id 1; released_at = Mtime.now () }
