(* A job store over a Hashtbl, for the example: a real store is a table of your
   database. The documentation of Caravan_jobs.Durable.store gives the contract
   of each function, and the SQL for PostgreSQL. The store is in the memory of
   the process, thus here the jobs do not survive a restart.

   The store also keeps the users of the application. Thus a sign-up adds the
   user and the welcome job in one step, as a transaction of a database does.

   The lock is a Fiber.Mutex of weft: a wait for it suspends the fiber, not its
   domain. *)

open Weft
module D = Caravan_jobs.Durable

type job =
  { id: int
  ; name: string
  ; payload: string
  ; key: string option
  ; mutable due: Clock.posix
  ; mutable attempts: int
  ; (* The token and the end of the lease of the last claim, until a retry or a
       release. *)
    mutable lease: (string * Clock.posix) option }

type t =
  { lock: Fiber.Mutex.t
  ; jobs: (int, job) Hashtbl.t
  ; (* The name and the key of each job with a key. *)
    keys: (string * string, unit) Hashtbl.t
  ; (* The last tick of each recurring entry. *)
    ticks: (string, Clock.posix) Hashtbl.t
  ; users: (string, unit) Hashtbl.t
  ; mutable next_id: int
  ; mutable next_lease: int }

let create () =
  { lock= Fiber.Mutex.create ()
  ; jobs= Hashtbl.create 16
  ; keys= Hashtbl.create 16
  ; ticks= Hashtbl.create 4
  ; users= Hashtbl.create 16
  ; next_id= 0
  ; next_lease= 0 }

let locked t f = Fiber.Mutex.use_rw ~protect:true t.lock f

let before a b = Span.compare (Time.diff Posix a b) Span.zero < 0

let later now span =
  match Time.add Posix now span with
  | Some p ->
      p
  | None ->
      invalid_arg "Store: the time is out of the range of the clock"

(* The end of the lease of [j], if the lease lives at [now]. *)
let live_lease now j =
  match j.lease with
  | Some (_, until) when before now until ->
      Some until
  | _ ->
      None

(* Under the lock: a new job. *)
let add t ?key (row : D.row) =
  let id = t.next_id in
  t.next_id <- id + 1 ;
  Hashtbl.replace t.jobs id
    { id
    ; name= row.name
    ; payload= row.payload
    ; key
    ; due= row.due
    ; attempts= 0
    ; lease= None } ;
  Option.iter (fun k -> Hashtbl.replace t.keys (row.name, k) ()) key ;
  string_of_int id

let push t row = locked t (fun () -> add t row)

(* The jobs of [names] whose due time is [now] or before, and with no live
   lease. The first is the first due, then the first pushed. *)
let claim t ~names ~max ~lease =
  let now = Time.now Posix in
  let until = later now lease in
  locked t (fun () ->
      Hashtbl.to_seq_values t.jobs
      |> List.of_seq
      |> List.filter (fun j ->
          List.mem j.name names
          && (not (before now j.due))
          && live_lease now j = None )
      |> List.sort (fun a b ->
          match Span.compare (Time.diff Posix a.due b.due) Span.zero with
          | 0 ->
              Int.compare a.id b.id
          | c ->
              c )
      |> List.take max
      |> List.map (fun j ->
          let token = string_of_int t.next_lease in
          t.next_lease <- t.next_lease + 1 ;
          j.attempts <- j.attempts + 1 ;
          j.lease <- Some (token, until) ;
          { D.id= string_of_int j.id
          ; name= j.name
          ; payload= j.payload
          ; attempts= j.attempts
          ; lease= token } ) )

(* [owned t ~id ~lease f] runs [f] on the job [id] only if its lease token is
   [lease]. *)
let owned t ~id ~lease f =
  locked t (fun () ->
      match Option.bind (int_of_string_opt id) (Hashtbl.find_opt t.jobs) with
      | Some j when Option.map fst j.lease = Some lease ->
          f j ; true
      | _ ->
          false )

let extend t ~id ~lease span =
  let until = later (Time.now Posix) span in
  owned t ~id ~lease (fun j -> j.lease <- Some (lease, until))

(* The end of a job frees its key. *)
let remove t ~id ~lease =
  owned t ~id ~lease (fun j ->
      Hashtbl.remove t.jobs j.id ;
      Option.iter (fun k -> Hashtbl.remove t.keys (j.name, k)) j.key )

let retry t ~id ~lease ~due =
  owned t ~id ~lease (fun j ->
      j.lease <- None ;
      j.due <- due )

let release t ~id ~lease =
  owned t ~id ~lease (fun j ->
      j.lease <- None ;
      j.attempts <- j.attempts - 1 )

let next_due t ~names =
  let now = Time.now Posix in
  locked t (fun () ->
      Hashtbl.fold
        (fun _ j first ->
          if List.mem j.name names then
            let at = Option.value (live_lease now j) ~default:j.due in
            match first with Some f when before f at -> first | _ -> Some at
          else first )
        t.jobs None )

let push_unique t (row : D.row) ~key =
  locked t (fun () ->
      if Hashtbl.mem t.keys (row.name, key) then None else Some (add t ~key row) )

(* A tick more than 5 min after the time of the store gets no job: else a
   process whose clock is far ahead blocks the entry until that time. *)
let push_tick t row ~entry ~tick =
  let limit = later (Time.now Posix) (Span.s 300) in
  locked t (fun () ->
      if before limit tick then None
      else
        match Hashtbl.find_opt t.ticks entry with
        | Some last when not (before last tick) ->
            None
        | _ ->
            Hashtbl.replace t.ticks entry tick ;
            Some (add t row) )

(* [store t] is the job store of [t] for Caravan_jobs.Durable.create. Here
   complete and fail delete the job. A real store can keep the failed jobs in a
   table, with the error. *)
let store t =
  D.store ~push:(push t) ~claim:(claim t) ~extend:(extend t)
    ~complete:(remove t) ~retry:(retry t) ~release:(release t)
    ~fail:(fun ~id ~lease ~error:_ -> remove t ~id ~lease)
    ~next_due:(next_due t) ~push_unique:(push_unique t) ~push_tick:(push_tick t)
    ()

(* [sign_up t email row] adds the user [email] and the job [row] in one step,
   and gives the id of the job. It gives None if the user exists: then it adds
   no job. *)
let sign_up t email row =
  locked t (fun () ->
      if Hashtbl.mem t.users email then None
      else (
        Hashtbl.replace t.users email () ;
        Some (add t row) ) )

(* The jobs in the store, in the order of their ids: the id, the name, the
   attempts, and true if a worker runs the job (a live lease). *)
let jobs t =
  let now = Time.now Posix in
  locked t (fun () ->
      Hashtbl.to_seq_values t.jobs
      |> List.of_seq
      |> List.sort (fun a b -> Int.compare a.id b.id)
      |> List.map (fun j ->
          (string_of_int j.id, j.name, j.attempts, live_lease now j <> None) ) )
