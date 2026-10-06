open Affect

(* The writers of request bodies belong to the client, not to the fibers
   that make requests: a request returns its response while its body
   goes out, for example through a proxy. A child call belongs to the
   fiber that makes it, thus a supervisor of the client makes the calls. *)

type job = {
  body : unit -> unit;
  result : (unit, exn * Printexc.raw_backtrace) result Cell.Once.t;
  handle : unit Fun.Async.t Cell.Once.t;
  self : Fun.Async.Call.t Cell.Once.t;
  taken : bool Atomic.t;
}

let job body =
  { body; result = Cell.Once.make (); handle = Cell.Once.make (); self = Cell.Once.make ();
    taken = Atomic.make false }

type t = { jobs : job Port.t; stopping : unit Cell.Once.t; supervisor : unit Fun.Async.t }

(* A job sets its result in all cases, also when it is cancelled before
   its first step. *)
let run_job j =
  Cell.Once.try_set j.self (Fun.Async.Call.current ());
  let r = match j.body () with () -> Ok () | exception e -> Error (e, Printexc.get_raw_backtrace ()) in
  Cell.Once.try_set j.result r

let make ?(on_stop = ignore) () =
  let jobs = Port.make () and stopping = Cell.Once.make () in
  let stop = Action.map (fun () -> `Stop) (Cell.Once.get' stopping) in
  let take = Action.map (fun j -> `Job j) (Port.take' jobs) in
  let rec loop () =
    match Action.invoke (Action.choose [ stop; take ]) with
    | `Stop -> ()
    | `Job j ->
        Cell.Once.try_set j.handle (Fun.Async.call (fun () -> run_job j));
        loop ()
  in
  (* A cancel of the fiber that made the supervisor is a stop too. *)
  let supervisor =
    Fun.Async.call (fun () ->
        Fun.Async.protect
          ~finally:(fun () -> Cell.Once.try_set stopping (); on_stop ())
          (fun () -> try loop () with Fun.Async.Cancelled -> ()))
  in
  { jobs; stopping; supervisor }

let start t j =
  let offer = Port.offer' t.jobs j `Taken and stop = Action.map (fun () -> `Stop) (Cell.Once.get' t.stopping) in
  match Action.invoke (Action.choose [ offer; stop ]) with
  | `Taken -> Atomic.set j.taken true
  | `Stop -> invalid_arg "Caravan_client: the client is closed"

(* A cancel of the supervisor reaches all its jobs, also a job that it
   took after [stopping] was set. *)
let stop t =
  Cell.Once.try_set t.stopping ();
  Fun.Async.cancel t.supervisor

let wait t =
  Fun.Async.mask_cancellation (fun () ->
      try Fun.Async.get t.supervisor with Fun.Async.Cancelled -> ())

let is_self j =
  match Cell.Once.is_set j.self with
  | false -> false
  | true -> Fun.Async.Call.equal (Fun.Async.Call.current ()) (Cell.Once.get j.self)

(* The result of a job that the supervisor took. With [cancel], the job
   stops first if it runs: a cancel of a call that ended has no effect. *)
let await ~cancel j =
  Fun.Async.mask_cancellation (fun () ->
      if cancel then Fun.Async.cancel (Cell.Once.get j.handle);
      Cell.Once.get j.result)
