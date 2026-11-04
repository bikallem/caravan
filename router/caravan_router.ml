open Caravan
module Methods = Map.Make (Method)
module Literals = Map.Make (String)

type route =
  | Route of {meth: Method.t option; pattern: string; handler: Handler.t}
  | Group of
      {prefix: string; middleware: Handler.middleware list; routes: route list}

let route meth pattern handler = Route {meth= Some meth; pattern; handler}

let get = route Method.GET

let post = route Method.POST

let put = route Method.PUT

let delete = route Method.DELETE

let patch = route Method.PATCH

let any pattern handler = Route {meth= None; pattern; handler}

let group ?(middleware = []) prefix routes = Group {prefix; middleware; routes}

let decode = Caravan.Private.Percent.decode

let is_dot s = s = "." || s = ".."

(* [split p] splits [p], which starts with '/', at each '/' after the first. *)
let split p = String.split_on_char '/' (String.sub p 1 (String.length p - 1))

(* Patterns *)

type segment = Literal of string | Param of string | Rest

let fail pattern what =
  invalid_arg (Printf.sprintf "Caravan_router: %S: %s" pattern what)

let is_name_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' ->
      true
  | _ ->
      false

let is_bad_char = function
  | '?' | '#' | ' ' | '\x00' .. '\x1f' | '\x7f' ->
      true
  | _ ->
      false

let parse pattern =
  if not (String.starts_with ~prefix:"/" pattern) then
    fail pattern "a pattern must start with /" ;
  if String.exists is_bad_char pattern then
    fail pattern
      "a pattern must not contain ?, #, a space or a control character" ;
  let rec go names = function
    | [] ->
        []
    | ["**"] ->
        [Rest]
    | s :: _ when String.contains s '*' ->
        fail pattern "* is only in a last segment **"
    | s :: rest when String.starts_with ~prefix:":" s ->
        let name = String.sub s 1 (String.length s - 1) in
        if name = "" || not (String.for_all is_name_char name) then
          fail pattern "a parameter name has the characters a-z A-Z 0-9 _" ;
        if List.mem name names then fail pattern ("two parameters " ^ name) ;
        Param name :: go (name :: names) rest
    | s :: rest -> (
      match decode s with
      | None ->
          fail pattern "a bad % sequence"
      | Some d when is_dot d || String.contains d '\x00' ->
          fail pattern "a segment . or .., or a NUL"
      | Some d ->
          Literal d :: go names rest )
  in
  go [] (split pattern)

(* The tree *)

type entry =
  { names: string list  (** The names of the parameters, in pattern order. *)
  ; has_rest: bool
  ; pattern: string
  ; handler: Handler.t }

type node =
  { mutable literals: node Literals.t
  ; mutable param: node option
  ; mutable rest: node option
  ; mutable methods: entry Methods.t
  ; mutable any: entry option }

let node () =
  { literals= Literals.empty
  ; param= None
  ; rest= None
  ; methods= Methods.empty
  ; any= None }

(* [find_or_add get set] is the node of [get ()], or a new node that [set]
   stores. *)
let find_or_add get set =
  match get () with
  | Some n ->
      n
  | None ->
      let n = node () in
      set n ; n

let conflict (e : entry) (e' : entry) =
  invalid_arg
    (Printf.sprintf
       "Caravan_router: %S and %S have one method and one pattern (the names \
        of the parameters have no effect)"
       e'.pattern e.pattern )

let insert root meth segs entry =
  let rec walk n = function
    | [] ->
        n
    | Literal s :: rest ->
        let c =
          find_or_add
            (fun () -> Literals.find_opt s n.literals)
            (fun c -> n.literals <- Literals.add s c n.literals)
        in
        walk c rest
    | Param _ :: rest ->
        walk (find_or_add (fun () -> n.param) (fun c -> n.param <- Some c)) rest
    | Rest :: _ ->
        find_or_add (fun () -> n.rest) (fun c -> n.rest <- Some c)
  in
  let n = walk root segs in
  match meth with
  | None -> (
    match n.any with Some e -> conflict entry e | None -> n.any <- Some entry )
  | Some m -> (
    match Methods.find_opt m n.methods with
    | Some e ->
        conflict entry e
    | None ->
        n.methods <- Methods.add m entry n.methods )

let check_prefix prefix =
  if
    prefix <> ""
    && ( (not (String.starts_with ~prefix:"/" prefix))
       || String.ends_with ~suffix:"/" prefix )
  then fail prefix "a prefix is empty, or starts with / and does not end with /" ;
  (* A group with no routes gets the checks of a pattern too. *)
  if prefix <> "" then ignore (parse prefix)

(* [flatten routes] gives each route with its full pattern and its handler in
   the middleware of its groups. [outer] are the prefixes and the middleware of
   the groups around [routes], outermost first. *)
let rec flatten prefix outer routes =
  List.concat_map
    (function
      | Route {meth; pattern; handler} ->
          if pattern <> "" && not (String.starts_with ~prefix:"/" pattern) then
            fail pattern "a pattern must start with /" ;
          (* [Other "GET"] is [GET]. *)
          let meth =
            Option.map
              (fun m ->
                match Method.of_string (Method.to_string m) with
                | m ->
                    m
                | exception Invalid_argument _ ->
                    fail pattern "the name of the method is not a token" )
              meth
          in
          [(meth, prefix ^ pattern, Handler.compose outer handler)]
      | Group g ->
          check_prefix g.prefix ;
          flatten (prefix ^ g.prefix) (outer @ g.middleware) g.routes )
    routes

(* Attributes *)

let params_key : (string * string) list Attrs.key = Attrs.key ~name:"params" ()

let rest_key : string list Attrs.key = Attrs.key ~name:"rest" ()

let template : string Attrs.key = Attrs.key ~name:"template" ()

(* The match *)

(* The order of [Allow]: these methods in this order, then the other methods in
   the order of [Method.compare]. *)
let allow_order = Method.[GET; HEAD; POST; PUT; DELETE; OPTIONS; PATCH]

let rank m =
  Option.value
    (List.find_index (Method.equal m) allow_order)
    ~default:(List.length allow_order)

let compare_allow a b =
  match Int.compare (rank a) (rank b) with 0 -> Method.compare a b | c -> c

let pick (n : node) meth =
  match Methods.find_opt meth n.methods with
  | Some e ->
      Some e
  | None -> (
      let get =
        if Method.equal meth Method.HEAD then
          Methods.find_opt Method.GET n.methods
        else None
      in
      match get with Some e -> Some e | None -> n.any )

(* [search root meth segs] gives the first route in priority order that matches
   [segs] and [meth], with the values of its parameters (in path order) and of
   the rest. [allow] gets the methods of the other nodes that match [segs]. Each
   node is at the depth of its segment, thus the search visits it at most one
   time. *)
let search root meth segs allow =
  let match_node n values rest =
    match pick n meth with
    | Some e ->
        Some (e, List.rev values, rest)
    | None ->
        Methods.iter (fun m _ -> allow := m :: !allow) n.methods ;
        None
  in
  let match_rest n values segs =
    match n.rest with Some r -> match_node r values segs | None -> None
  in
  let rec go n values = function
    | [] -> (
      match match_node n values [] with
      | Some _ as r ->
          r
      | None ->
          match_rest n values [] )
    | s :: tail as segs -> (
        let lit =
          match Literals.find_opt s n.literals with
          | Some c ->
              go c values tail
          | None ->
              None
        in
        match lit with
        | Some _ ->
            lit
        | None -> (
            let param =
              match n.param with
              | Some c when s <> "" ->
                  go c (s :: values) tail
              | _ ->
                  None
            in
            match param with
            | Some _ ->
                param
            | None ->
                match_rest n values segs ) )
  in
  go root [] segs

(* [decoded_segments path] are the decoded segments of [path], or [None] for a
   path that gets 400. [Request.make] refuses a raw NUL, thus only a segment
   with a '%' can have one. *)
let decoded_segments path =
  let rec go acc = function
    | [] ->
        Some (List.rev acc)
    | s :: rest -> (
        if String.contains s '#' then None
        else if not (String.contains s '%') then
          if is_dot s then None else go (s :: acc) rest
        else
          match decode s with
          | Some d when not (is_dot d || String.contains d '\x00') ->
              go (d :: acc) rest
          | _ ->
              None )
  in
  go [] (split path)

let bad_request () = Response.string ~status:Status.bad_request "Bad Request\n"

let allow_field methods =
  let methods =
    (if List.exists (Method.equal Method.GET) methods then [Method.HEAD] else [])
    @ (Method.OPTIONS :: methods)
  in
  List.sort_uniq compare_allow methods
  |> List.map Method.to_string |> String.concat ", "

let handler ?(not_found = Handler.not_found) routes =
  let root = node () in
  List.iter
    (fun (meth, pattern, handler) ->
      if Option.fold ~none:false ~some:(Method.equal Method.CONNECT) meth then
        fail pattern "a CONNECT request has no path" ;
      let segs = parse pattern in
      let names =
        List.filter_map (function Param n -> Some n | _ -> None) segs
      in
      let has_rest = List.mem Rest segs in
      insert root meth segs {names; has_rest; pattern; handler} )
    (flatten "" [] routes) ;
  fun req ->
    let path = Request.path_only req in
    if not (String.starts_with ~prefix:"/" path) then not_found req
    else
      match decoded_segments path with
      | None ->
          bad_request ()
      | Some segs -> (
          let meth = Request.meth req in
          let allow = ref [] in
          match search root meth segs allow with
          | Some (e, values, rest) ->
              let attrs =
                Request.attrs req
                |> Attrs.add params_key (List.combine e.names values)
                |> Attrs.add template e.pattern
                |> fun a ->
                if e.has_rest then Attrs.add rest_key rest a
                else Attrs.remove rest_key a
              in
              let r = e.handler (Request.with_attrs attrs req) in
              (* The template of an inner router stays. *)
              if Attrs.mem template (Response.attrs r) then r
              else Response.with_attr template e.pattern r
          | None when !allow = [] ->
              not_found req
          | None ->
              let headers = Headers.of_list [("Allow", allow_field !allow)] in
              if Method.equal meth Method.OPTIONS then
                Response.empty ~status:Status.no_content ~headers ()
              else
                Response.string ~status:Status.method_not_allowed ~headers
                  "Method Not Allowed\n" )

let params req = Option.value ~default:[] (Request.find_attr params_key req)

let find_param name req = List.assoc_opt name (params req)

let param name req =
  match find_param name req with
  | Some v ->
      v
  | None ->
      invalid_arg ("Caravan_router.param: the route has no parameter " ^ name)

let rest req =
  match Request.find_attr rest_key req with
  | Some r ->
      r
  | None ->
      invalid_arg "Caravan_router.rest: the route has no **"
