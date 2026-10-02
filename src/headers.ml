type t = (string * string) list (* in wire order *)

let name_equal a b =
  let n = String.length a in
  n = String.length b
  && (let rec loop i =
        i = n
        || (Char.lowercase_ascii a.[i] = Char.lowercase_ascii b.[i]
            && loop (i + 1))
      in
      loop 0)

let is_valid_name = Token.is_token

let is_valid_value s =
  let is_field_char = function
  | '\t' | ' ' .. '~' | '\x80' .. '\xff' -> true
  | _ -> false
  in
  let is_ws = function ' ' | '\t' -> true | _ -> false in
  let n = String.length s in
  String.for_all is_field_char s
  && (n = 0 || not (is_ws s.[0] || is_ws s.[n - 1]))

let check name value =
  if not (is_valid_name name)
  then invalid_arg (Printf.sprintf "Headers: invalid field name %S" name);
  if not (is_valid_value value)
  then invalid_arg (Printf.sprintf "Headers: invalid value %S for %s" value name)

let empty = []
let is_empty h = h = []
let length = List.length

let of_list fields =
  List.iter (fun (name, value) -> check name value) fields;
  fields

let to_list h = h

let add name value h =
  check name value;
  h @ [ (name, value) ]

let remove name h = List.filter (fun (n, _) -> not (name_equal name n)) h

let set name value h =
  check name value;
  let rec replace = function
  | [] -> [ (name, value) ]
  | (n, _) :: rest when name_equal name n -> (name, value) :: remove name rest
  | field :: rest -> field :: replace rest
  in
  replace h

let find name h =
  List.find_map (fun (n, v) -> if name_equal name n then Some v else None) h

let find_all name h =
  List.filter_map (fun (n, v) -> if name_equal name n then Some v else None) h

let mem name h = List.exists (fun (n, _) -> name_equal name n) h
let fold f h acc = List.fold_left (fun acc (n, v) -> f n v acc) acc h
let iter f h = List.iter (fun (n, v) -> f n v) h

let tokens name h =
  find_all name h
  |> List.concat_map (String.split_on_char ',')
  |> List.map String.trim
  |> List.filter (fun s -> s <> "")

let has_token name token h = List.exists (name_equal token) (tokens name h)

let hop_by_hop =
  [ "connection"; "keep-alive"; "proxy-connection"; "proxy-authenticate";
    "proxy-authorization"; "te"; "trailer"; "transfer-encoding"; "upgrade" ]

let remove_hop_by_hop h =
  let named = tokens "connection" h in
  let is_hop n =
    List.exists (name_equal n) hop_by_hop || List.exists (name_equal n) named
  in
  List.filter (fun (n, _) -> not (is_hop n)) h

let equal h h' =
  List.equal (fun (n, v) (n', v') -> name_equal n n' && String.equal v v') h h'

let pp ppf h =
  let pp_field ppf (n, v) = Format.fprintf ppf "%s: %s" n v in
  Format.fprintf ppf "@[<v>%a@]" (Format.pp_print_list pp_field) h
