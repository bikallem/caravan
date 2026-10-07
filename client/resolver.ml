open Affect

(* getaddrinfo blocks its system thread, and a blocked domain stops all
   its fibers. Thus a system thread does the lookup and writes one byte to
   a pipe, and the fiber waits on the pipe. The read end has two owners,
   the thread and the fiber: the last one closes it. Thus the thread never
   writes to a closed pipe, also after a cancel of the fiber. *)

let threads_started = Atomic.make 0

(* The fibers that wait for a lookup. A cancelled lookup releases its
   place, but its thread runs until getaddrinfo returns. *)
let lookups = Semaphore.make 16

let numeric host =
  let h =
    if String.starts_with ~prefix:"[" host && String.ends_with ~suffix:"]" host
    then String.sub host 1 (String.length host - 2)
    else host
  in
  match Unix.inet_addr_of_string h with a -> Some a | exception Failure _ -> None

let close_noerr fd = try Unix.close fd with Unix.Unix_error _ -> ()

let lookup getaddrinfo host port =
  Semaphore.with_acquired lookups @@ fun () ->
  let rd, wr = Unix.pipe ~cloexec:true () in
  let result = Atomic.make None and owners = Atomic.make 2 in
  let drop () = if Atomic.fetch_and_add owners (-1) = 1 then close_noerr rd in
  let work () =
    let r =
      try Ok (getaddrinfo host (string_of_int port) [ Unix.AI_SOCKTYPE Unix.SOCK_STREAM ])
      with e -> Error e
    in
    Atomic.set result (Some r);
    (try ignore (Unix.write_substring wr "x" 0 1) with Unix.Unix_error _ -> ());
    close_noerr wr;
    drop ()
  in
  (match
     Unix.set_nonblock rd;
     Thread.create work ()
   with
  | _ -> Atomic.incr threads_started
  | exception e -> close_noerr rd; close_noerr wr; raise e);
  Stdlib.Fun.protect ~finally:drop @@ fun () ->
  let rec wait () =
    match Atomic.get result with
    | Some (Ok addrs) -> addrs
    | Some (Error e) -> raise e
    | None -> Action.invoke (Affect_unix.Unix.wait_readable rd ()); wait ()
  in
  wait ()

let resolve ?(getaddrinfo = Unix.getaddrinfo) ~host ~port () =
  match numeric host with
  | Some a -> [ Unix.ADDR_INET (a, port) ]
  | None -> (
      let addrs =
        List.filter_map
          (fun (a : Unix.addr_info) -> match a.ai_addr with Unix.ADDR_INET _ as s -> Some s | _ -> None)
          (lookup getaddrinfo host port)
      in
      match addrs with [] -> Error.fail (Connect ("no address for " ^ host)) | l -> l)
