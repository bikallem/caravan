(* Durable jobs: the jobs are in a store. With a store in a database, the jobs
   survive a restart, and many processes can share them. The store of this
   example is in memory (see store.ml). Recurring jobs run at the ticks of
   schedules.

   Run dune exec examples/durable_jobs/main.exe, then send these requests:

   curl -i -d email=ann@example.com 127.0.0.1:8080/signup

   The response is 202, and the program prints the welcome mail at once, and
   about 10 s later a mail with tips. A second sign-up of the same address gets
   409, and no second mail.

   An export takes 5 s. Send the next request two times in 5 s: the second
   response tells that the export is already in the queue. Thus the user gets
   one export mail.

   curl -i -d user=ann 127.0.0.1:8080/export

   The jobs in the store. During the export, the list has the export job, with 1
   attempt, and running:

   curl -i 127.0.0.1:8080/jobs

   A client can send 5 posts (sign-ups and exports), then one more each 12 s. A
   post after them gets 429.

   Each 10 s, the program prints the counts of the queue. Each day at 03:00 UTC,
   a job sends the nightly report. *)

open Weft
open Caravan
module D = Caravan_jobs.Durable
module Schedule = Caravan_jobs.Schedule
module Recurring = Caravan_jobs.Recurring

(* The welcome mail has the id of the job. The id is the same at each attempt: a
   mail service that keeps the keys sends the mail one time, also when the job
   runs again. *)
let welcome_text ~id address =
  Printf.sprintf "To: %s\nIdempotency-Key: welcome-%s\n\nWelcome!\n" address id

(* A user signs up one time, thus the address is the key of the tips mail. *)
let tips_text address =
  Printf.sprintf "To: %s\nIdempotency-Key: tips-%s\n\nThree tips.\n" address
    address

(* The export mail has the id of the export job, as the welcome mail. *)
let export_text ~id user =
  Printf.sprintf "To: %s\nIdempotency-Key: export-%s\n\nYour export is ready.\n"
    user id

let report_text (tick : Clock.posix) =
  let d = Time.to_utc tick in
  Printf.sprintf
    "To: admin@example.com\nThe nightly report of %04d-%02d-%02d.\n" d.year
    d.month d.day

(* The schedule of the nightly report: each day at 03:00 UTC. *)
let nightly = Schedule.cron "0 3 * * *"

(* The schedule of the counts: at :00, :10, :20, ... of each minute. *)
let counts_schedule = Schedule.every (Span.s 10)

let tips_delay = Span.s 10

(* The address must be one address, not a list of addresses. Thus the check
   accepts exactly one @, a local part that is not empty, and a domain of
   letters, digits, dots and hyphens with a dot. It refuses a space, a control
   character, a double quote and , ; < > ( ) \. A real mailer gets the recipient
   as an argument, not as header text. *)
let valid_address address =
  let local_char c =
    c > ' ' && c < '\127' && not (String.contains ",;<>\"()\\" c)
  in
  let domain_char = function
    | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '.' | '-' ->
        true
    | _ ->
        false
  in
  match String.split_on_char '@' address with
  | [local; domain] ->
      local <> ""
      && String.for_all local_char local
      && String.contains domain '.'
      && String.for_all domain_char domain
  | _ ->
      false

(* A user name has only the letters a to z and the digits. A real application
   takes the user from its session. *)
let valid_user u =
  u <> ""
  && String.for_all (function 'a' .. 'z' | '0' .. '9' -> true | _ -> false) u

let reason_text : D.reason -> string = function
  | Failed (e, _) ->
      "failed with " ^ Printexc.to_string e
  | Discarded (e, _) ->
      "discarded after " ^ Printexc.to_string e
  | Undecodable (e, _) ->
      "undecodable: " ^ Printexc.to_string e
  | Abandoned ->
      "abandoned"

let failure_text (f : D.failure) =
  Printf.sprintf "%s job %s: %s, attempts: %d" f.name f.id
    (reason_text f.reason) f.attempts

let counts_text tick (s : D.stats) =
  let u = Time.to_utc tick in
  Printf.sprintf "%02d:%02d:%02d succeeded: %d, running: %d" u.hour u.minute
    u.second s.succeeded s.running

let jobs_text jobs =
  String.concat ""
    (List.map
       (fun (id, name, attempts, running) ->
         Printf.sprintf "%s %s, attempts: %d%s\n" id name attempts
           (if running then ", running" else "") )
       jobs )

(* The value of [name] in the urlencoded form of [req]. *)
let field req name =
  match Request.form ~max_bytes:1024 req with
  | Error e ->
      Error
        (Response.string ~status:(Form.error_status e)
           (Format.asprintf "%a\n" Form.pp_error e) )
  | Ok form ->
      Ok (Option.value (Form.find name form) ~default:"")

let bad_request text = Response.string ~status:Status.bad_request text

type t = {queue: D.t; recurring: Recurring.t; handler: Handler.t}

(* [outbox mail] sends a mail. It can raise: then the job gets a retry, with the
   default policy (5 attempts). [log] gets the counts at each tick. [report]
   gets a line for each job that did not succeed. *)
let make ~store ~outbox ~log ~report =
  (* The payload of a job is a string in the store. The welcome, tips and export
     jobs keep their argument as it is. *)
  let tips =
    D.job ~name:"tips" ~encode:Fun.id ~decode:Fun.id (fun _ address ->
        outbox (tips_text address) )
  in
  (* The welcome job needs the queue, and the queue needs the welcome job. *)
  let cell = ref None in
  (* After the welcome mail, the job adds the tips job with Durable.enqueue. The
     store keeps the tips job, also over a restart. The job starts 10 s later,
     at the next poll of the queue (each 1 s). A real application waits longer,
     for example one day. *)
  let welcome =
    D.job ~name:"welcome" ~encode:Fun.id ~decode:Fun.id
      (fun (info : D.info) address ->
        outbox (welcome_text ~id:info.id address) ;
        ignore (D.enqueue (Option.get !cell) ~delay:tips_delay tips address) )
  in
  let export =
    D.job ~name:"export" ~encode:Fun.id ~decode:Fun.id
      (fun (info : D.info) user ->
        (* The task of the export, in the example a wait. *)
        Time.sleep Mono (Span.s 5) ;
        outbox (export_text ~id:info.id user) )
  in
  (* The argument of the report is its tick, in POSIX seconds in the store. *)
  let report_job =
    D.job ~name:"nightly_report"
      ~encode:(fun tick -> Printf.sprintf "%.0f" (Time.posix_to_unix_s tick))
      ~decode:(fun s -> Time.posix_of_unix_s (float_of_string s))
      (fun _ tick -> outbox (report_text tick))
  in
  (* At a stop, the running jobs get a grace. Then the queue cancels them, and
     gives them back to the store: the stop uses none of their attempts, and the
     next start of a queue on the store runs them. *)
  let queue =
    D.create ~workers:2
      ~on_error:(fun f -> report (failure_text f))
      ~on_store_error:(fun e -> report ("store: " ^ Printexc.to_string e))
      ~jobs:[Job welcome; Job tips; Job export; Job report_job]
      (Store.store store)
  in
  cell := Some queue ;
  (* Each process runs each entry. An entry of Recurring.durable makes one job
     for each tick, over all the processes of the store: its name must be unique
     in the store. *)
  let recurring =
    Recurring.create
      [ Recurring.entry ~name:"counts" counts_schedule (fun tick ->
            log (counts_text tick (D.stats queue)) )
      ; Recurring.durable ~name:"example.nightly_report" nightly queue
          report_job Fun.id ]
  in
  (* Durable.row gives the row of the job. The store adds the user and the job
     in one step. After the step, wake makes the queue claim the job at once,
     with no wait for the poll. *)
  let signup req =
    match field req "email" with
    | Error resp ->
        resp
    | Ok address when not (valid_address address) ->
        bad_request "The form needs a valid email.\n"
    | Ok address -> (
      match Store.sign_up store address (D.row welcome address) with
      | Some _ ->
          D.wake queue ;
          Response.string ~status:Status.accepted
            (Printf.sprintf "Welcome, %s. We will send a mail.\n" address)
      | None ->
          Response.string ~status:Status.conflict
            (Printf.sprintf "%s has an account.\n" address) )
  in
  (* The key of the job is the user. The store keeps the key from the push of
     the job to its end, thus a user has at most one export in the queue. After
     the end, a new export can start. *)
  let export_route req =
    match field req "user" with
    | Error resp ->
        resp
    | Ok user when not (valid_user user) ->
        bad_request "The form needs a user: letters a to z and digits.\n"
    | Ok user -> (
      match D.enqueue_unique queue ~key:user export user with
      | Some _ ->
          Response.string ~status:Status.accepted
            (Printf.sprintf "The export of %s is in the queue.\n" user)
      | None ->
          Response.string ~status:Status.accepted
            (Printf.sprintf "The export of %s is already in the queue.\n" user)
      )
  in
  (* The store keeps each job, thus each client gets a rate limit: 5 posts, then
     one more each 12 s. A sign-up sends a mail to the address of the form: a
     real sign-up mail asks the person to confirm the address. *)
  let posts =
    Caravan_middleware.Rate_limit.middleware ~burst:5 ~every:(Span.s 12) ()
      (fun req ->
        if Request.path_only req = "/signup" then signup req
        else export_route req )
  in
  let routes req =
    match (Request.meth req, Request.path_only req) with
    | POST, ("/signup" | "/export") ->
        posts req
    | GET, "/jobs" ->
        Response.string (jobs_text (Store.jobs store))
    | _ ->
        Handler.not_found req
  in
  {queue; recurring; handler= routes}
