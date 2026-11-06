include Weft_flow.Flow

let linger flow ~max =
  let buf = Bytes.create 4096 in
  let rec discard left =
    if left > 0 then
      match read flow buf 0 (Int.min left (Bytes.length buf)) with
      | 0 ->
          ()
      | n ->
          discard (left - n)
  in
  (* Each read of the linger is a new call, so a read timeout would give each
     read a new deadline. Only the read deadline must end the linger. *)
  set_read_timeout flow None ;
  (* A different fiber can close [flow] at any time. *)
  try shutdown_send flow ; discard max with Closed | Timeout -> ()
