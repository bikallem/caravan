open Affect

(* getaddrinfo blocks its system thread, and a blocked domain stops all its
   fibers. Thus a system thread does the lookup and writes one byte to a pipe,
   and the fiber waits on the pipe. The read end has two owners, the thread and
   the fiber: the last one closes it. Thus the thread never writes to a closed
   pipe, also after a cancel of the fiber. *)

let threads_started = Atomic.make 0

(* The fibers that wait for a lookup. A cancelled lookup releases its place, but
   its thread runs until getaddrinfo returns. *)
let lookups = Semaphore.make 16

(* The numeric hosts are the texts that inet_pton accepts. The parser does not
   call Unix: the weft port forbids Unix in the library. *)

let is_digit c = c >= '0' && c <= '9'

let is_hex c = is_digit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

(* Four parts in dotted decimal, each 0 to 255 with no leading zero. *)
let is_ipv4 s =
  let part p =
    let n = String.length p in
    n >= 1 && n <= 3 && String.for_all is_digit p
    && (n = 1 || p.[0] <> '0')
    && int_of_string p <= 255
  in
  match String.split_on_char '.' s with
  | [_; _; _; _] as parts ->
      List.for_all part parts
  | _ ->
      false

(* The number of 16-bit groups in [s], a side of "::": groups of 1 to 4 hex
   digits that a ':' separates. On the right side, an IPv4 address can replace
   the last two groups. It gives [None] if [s] is not a side. *)
let groups ~right s =
  let group g =
    let n = String.length g in
    n >= 1 && n <= 4 && String.for_all is_hex g
  in
  if s = "" then Some 0
  else
    match List.rev (String.split_on_char ':' s) with
    | last :: rest when right && String.contains last '.' ->
        if is_ipv4 last && List.for_all group rest then
          Some (List.length rest + 2)
        else None
    | parts ->
        if List.for_all group parts then Some (List.length parts) else None

(* Eight groups, or less than eight with one "::" in place of the zero
   groups. *)
let is_ipv6 s =
  let n = String.length s in
  let rec double_colon i =
    if i + 1 >= n then None
    else if s.[i] = ':' && s.[i + 1] = ':' then Some i
    else double_colon (i + 1)
  in
  match double_colon 0 with
  | None ->
      groups ~right:true s = Some 8
  | Some i -> (
      let left = String.sub s 0 i
      and right = String.sub s (i + 2) (n - i - 2) in
      match (groups ~right:false left, groups ~right:true right) with
      | Some l, Some r ->
          l + r <= 7
      | _ ->
          false )

let numeric host =
  let h =
    if String.starts_with ~prefix:"[" host && String.ends_with ~suffix:"]" host
    then String.sub host 1 (String.length host - 2)
    else host
  in
  if is_ipv4 h || is_ipv6 h then Some h else None

let close_noerr fd = try Unix.close fd with Unix.Unix_error _ -> ()

let lookup getaddrinfo host port =
  Semaphore.with_acquired lookups
  @@ fun () ->
  let rd, wr = Unix.pipe ~cloexec:true () in
  let result = Atomic.make None and owners = Atomic.make 2 in
  let drop () = if Atomic.fetch_and_add owners (-1) = 1 then close_noerr rd in
  let work () =
    let r =
      try
        Ok
          (getaddrinfo host (string_of_int port)
             [Unix.AI_SOCKTYPE Unix.SOCK_STREAM] )
      with e -> Error e
    in
    Atomic.set result (Some r) ;
    (try ignore (Unix.write_substring wr "x" 0 1) with Unix.Unix_error _ -> ()) ;
    close_noerr wr ; drop ()
  in
  ( match Unix.set_nonblock rd ; Thread.create work () with
  | _ ->
      Atomic.incr threads_started
  | exception e ->
      close_noerr rd ; close_noerr wr ; raise e ) ;
  Stdlib.Fun.protect ~finally:drop
  @@ fun () ->
  let rec wait () =
    match Atomic.get result with
    | Some (Ok addrs) ->
        addrs
    | Some (Error e) ->
        raise e
    | None ->
        Action.invoke (Affect_unix.Unix.wait_readable rd ()) ;
        wait ()
  in
  wait ()

let resolve ?(getaddrinfo = Unix.getaddrinfo) ~host ~port () =
  match numeric host with
  | Some a ->
      (* This second parse is temporary. The port of the client to weft removes
         it, because [resolve] then gives the address as text. *)
      [Unix.ADDR_INET (Unix.inet_addr_of_string a, port)]
  | None -> (
      let addrs =
        List.filter_map
          (fun (a : Unix.addr_info) ->
            match a.ai_addr with Unix.ADDR_INET _ as s -> Some s | _ -> None )
          (lookup getaddrinfo host port)
      in
      match addrs with
      | [] ->
          Error.fail (Connect ("no address for " ^ host))
      | l ->
          l )
