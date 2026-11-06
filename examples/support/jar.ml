(* A cookie jar for the tests of the examples. *)

open Caravan

(* The pairs, in the order of their first Set-Cookie. *)
type t = {mutable pairs: (string * string) list}

let create () = {pairs= []}

let find name jar = List.assoc_opt name jar.pairs

let with_cookies jar req =
  match jar.pairs with
  | [] ->
      req
  | pairs ->
      let field =
        String.concat "; " (List.map (fun (n, v) -> n ^ "=" ^ v) pairs)
      in
      Request.with_headers
        (Headers.add "Cookie" field (Request.headers req))
        req

(* [removes field] is true if an attribute of the Set-Cookie [field] is a
   Max-Age of 0 or less (RFC 6265 §5.2.2). *)
let removes field =
  match String.split_on_char ';' field with
  | [] ->
      false
  | _ :: attributes ->
      List.exists
        (fun a ->
          match String.index_opt a '=' with
          | None ->
              false
          | Some i -> (
              let name = String.trim (String.sub a 0 i) in
              let value = String.sub a (i + 1) (String.length a - i - 1) in
              String.lowercase_ascii name = "max-age"
              &&
              match int_of_string_opt (String.trim value) with
              | Some n ->
                  n <= 0
              | None ->
                  false ) )
        attributes

let keep_one jar field =
  match Cookie.parse_set_cookie field with
  | None ->
      ()
  | Some (name, _) when removes field ->
      jar.pairs <- List.remove_assoc name jar.pairs
  | Some (name, value) ->
      if List.mem_assoc name jar.pairs then
        jar.pairs <-
          List.map
            (fun (n, v) -> if n = name then (n, value) else (n, v))
            jar.pairs
      else jar.pairs <- jar.pairs @ [(name, value)]

let keep jar headers =
  List.iter (keep_one jar) (Headers.find_all "set-cookie" headers)

let fetch jar t req =
  let r = Caravan_testing.fetch t (with_cookies jar req) in
  keep jar r.headers ; r
