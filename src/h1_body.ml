let fixed r n =
  let remaining = ref n in
  let read buf off len =
    if !remaining = 0
    then 0
    else begin
      match Buf_reader.read_into r buf off (Int.min len !remaining) with
      | 0 -> raise Body.Truncated
      | k -> remaining := !remaining - k; k
    end
  in
  Body.of_source ~length:n read
