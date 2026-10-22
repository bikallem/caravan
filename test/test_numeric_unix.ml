(* The numeric parser of Resolver against inet_pton, through
   Unix.inet_addr_of_string. It is a suite of its own, so that the suites on the
   simulator do not call this parser of Unix. *)

open Windtrap
module Resolver = Caravan_client.Private.Resolver

(* The numeric check before the pure parser: it calls inet_pton. *)
let unix_numeric host =
  let h =
    if String.starts_with ~prefix:"[" host && String.ends_with ~suffix:"]" host
    then String.sub host 1 (String.length host - 2)
    else host
  in
  match Unix.inet_addr_of_string h with
  | a ->
      Some (Unix.string_of_inet_addr a)
  | exception Failure _ ->
      None

(* The address of Resolver.numeric, in the form of Unix.string_of_inet_addr. *)
let pure_numeric host =
  Option.map
    (fun a -> Unix.string_of_inet_addr (Unix.inet_addr_of_string a))
    (Resolver.numeric host)

(* Hosts near the numeric forms: IPv4, IPv6 with "::" and an IPv4 end, broken
   parts, and brackets on no side, on one side or on the two sides. Most parts
   are correct, thus a part of the hosts is numeric. *)
let numeric_host =
  let open Gen in
  let mostly good bad = frequency [(9, of_list good); (1, of_list bad)] in
  let ipv4 =
    map (String.concat ".")
      (list
         ~size:(frequency [(6, constant 4); (1, int_range 3 5)])
         (mostly
            ["0"; "1"; "9"; "10"; "99"; "249"; "255"]
            ["256"; "300"; "01"; "001"; "1000"; ""; "a"] ) )
  in
  let groups n =
    list ~size:n
      (mostly ["0"; "1"; "ff"; "FFFF"; "abcd"; "0000"] ["12345"; "g"; ""])
  in
  let last = of_list [[]; ["1.2.3.4"]; ["255.255.255.255"]; ["1.2.3"]] in
  let ipv6 =
    one_of
      [ map
          (fun (gs, l) -> String.concat ":" (gs @ l))
          (pair (groups (int_range 5 9)) last)
      ; map
          (fun (l, sep, r, last) ->
            String.concat ":" l ^ sep ^ String.concat ":" (r @ last) )
          (quad
             (groups (int_range 0 5))
             (frequency [(6, constant "::"); (1, of_list [":"; ":::"; ""])])
             (groups (int_range 0 5))
             last ) ]
  in
  let any =
    string_of ~size:(int_range 0 12)
      (of_list ['0'; '1'; '9'; 'a'; 'F'; 'g'; ':'; '.'; '['; ']'; '%'; ' '])
  in
  with_pp Format.pp_print_string
    (map
       (fun ((o, c), h) -> o ^ h ^ c)
       (pair
          (of_list [("", ""); ("[", "]"); ("[", ""); ("", "]")])
          (frequency [(2, ipv4); (3, ipv6); (1, any)]) ) )

let () =
  exit
    (Windtrap.run "numeric_unix"
       [ group "Resolver.numeric"
           [ prop "numeric accepts the same hosts as inet_addr_of_string"
               ~count:5000 ~examples:(List.map fst Numeric_hosts.table)
               numeric_host (fun host ->
                 equal ~msg:host (option string) (unix_numeric host)
                   (pure_numeric host) ) ] ] )
