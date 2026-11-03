type 'a key = {id: 'a Type.Id.t; uid: int; name: string option}

(* The uid of a [Type.Id.t] is the id of a fresh extension constructor. Each
   domain takes blocks of ids from a counter of the runtime. Thus two keys never
   have the same uid, also when domains make keys at the same time. *)
let key ?name () =
  let id = Type.Id.make () in
  {id; uid= Type.Id.uid id; name}

let key_name k = k.name

type binding = B : 'a key * 'a -> binding

module M = Map.Make (Int)

type t = binding M.t

let empty = M.empty

let is_empty = M.is_empty

let add k v m = M.add k.uid (B (k, v)) m

let find (type a) (k : a key) m : a option =
  match M.find_opt k.uid m with
  | None ->
      None
  | Some (B (k', v)) -> (
    match Type.Id.provably_equal k.id k'.id with
    | Some Type.Equal ->
        Some v
    | (None [@mutate off "two keys with the same uid are the same key"]) ->
        None )

let mem k m = M.mem k.uid m

let remove k m = M.remove k.uid m

let pp ppf m =
  let name (B (k, _)) = Option.value k.name ~default:"_" in
  let names =
    List.sort String.compare (List.map (fun (_, b) -> name b) (M.bindings m))
  in
  Format.pp_print_list
    ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ", ")
    Format.pp_print_string ppf names
