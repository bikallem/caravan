(* Host names to addresses. Weft does each lookup on the name pool of the world,
   and the pool queues the lookups: thus the resolver has no limit of its
   own. *)

open Weft

(* The numeric hosts are the texts that inet_pton accepts. The parser is pure:
   it does no system call. *)

let is_digit c = c >= '0' && c <= '9'

let is_hex c = is_digit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

(* Four parts in dotted decimal, each 0 to 255 with no leading zero. A part of
   many digits gives [None] from [int_of_string_opt]. *)
let is_ipv4 s =
  let part p =
    let n = String.length p in
    n >= 1 && String.for_all is_digit p
    && (n = 1 || p.[0] <> '0')
    && match int_of_string_opt p with Some v -> v <= 255 | None -> false
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

(* [Dns.resolve] can give an address more than one time. Keep the first one, in
   the order of the lookup. *)
let dedup addrs =
  List.rev
    (List.fold_left
       (fun acc a -> if List.mem a acc then acc else a :: acc)
       [] addrs )

let resolve host =
  match numeric host with
  | Some a ->
      [a]
  | None -> (
    match Dns.resolve host with
    | [] | (exception Net.Error (No_entry, _)) ->
        Error.fail (Connect ("no address for " ^ host))
    (* The text names the lookup and the host: an error of getaddrinfo, for
       example EAI_SYSTEM with its errno, tells neither. *)
    | exception Net.Error (e, _) ->
        Error.fail
          (Connect
             ( "the lookup of " ^ String.escaped host ^ " failed: "
             ^ Error.net_text e ) )
    | addrs ->
        dedup addrs )
