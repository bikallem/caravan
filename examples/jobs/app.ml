(* Background jobs in the process: a sign-up gives the welcome mail to a queue,
   and the response does not wait for the mail. The queue is in the memory of
   the process: see the durable_jobs example for jobs that survive a restart.

   Run dune exec examples/jobs/main.exe, then send these requests:

   curl -i -d email=ann@example.com 127.0.0.1:8080/signup

   The response is 202 at once. Then the program prints the welcome mail, and 10
   s later a mail with tips.

   curl -i 127.0.0.1:8080/stats

   The counts of the queue. After the two mails, succeeded is 2.

   A form with no email gets 400:

   curl -i -d name=ann 127.0.0.1:8080/signup *)

open Weft
open Caravan
module J = Caravan_jobs

(* The mail service refused the address. Another attempt does not help. *)
exception Rejected of string

let welcome_text address = Printf.sprintf "To: %s\nWelcome!\n" address

let tips_text address =
  Printf.sprintf "To: %s\nThree tips for a good start.\n" address

(* The welcome mail gets at most 3 attempts. After a failure, the next attempt
   starts after 2 s, then after 4 s, less a jitter of up to 15 %. A Rejected
   address gets no more attempt. *)
let welcome_retry =
  J.retry ~attempts:3 ~base:(Span.s 2) ~max:(Span.s 60)
    ~on:(function Rejected _ -> false | _ -> true)
    ()

(* The tips mail starts 10 s after the welcome mail. A real application waits
   longer, for example one day. *)
let tips_delay = Span.s 10

let reason_text : J.reason -> string = function
  | Failed (e, _) ->
      "failed with " ^ Printexc.to_string e
  | Discarded (e, _) ->
      "discarded after " ^ Printexc.to_string e
  | Lost `Not_started ->
      "lost before its start"
  | Lost `Running ->
      "lost during its run"

let failure_text (f : J.failure) =
  Printf.sprintf "%s job %d: %s, attempts: %d" f.name f.id
    (reason_text f.reason) f.attempts

(* The main counts of the queue. J.stats gives more. *)
let stats_text (s : J.stats) =
  Printf.sprintf
    "waiting: %d\n\
     delayed: %d\n\
     running: %d\n\
     succeeded: %d\n\
     retried: %d\n\
     failed: %d\n"
    s.waiting s.delayed s.running s.succeeded s.retried s.failed

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

type t = {queue: J.t; handler: Handler.t}

(* [outbox mail] sends a mail. It can raise: then the job gets a retry. [report]
   gets a line for each job that did not succeed.

   The queue runs at most 2 jobs at the same time. At most 100 jobs wait or are
   delayed: a sign-up after them gets 503. *)
let make ~outbox ~report =
  let queue =
    J.create ~workers:2 ~capacity:100
      ~on_error:(fun f -> report (failure_text f))
      ()
  in
  (* One attempt: a tip that does not arrive is not important. *)
  let tips =
    J.job ~name:"tips" ~retry:J.no_retry (fun address ->
        outbox (tips_text address) )
  in
  (* on_failure gets the argument of the job, before on_error of the queue. A
     job can enqueue a job: during the stop of the queue, it gets `Stopped. *)
  let welcome =
    J.job ~name:"welcome" ~retry:welcome_retry
      ~on_failure:(fun address _ -> report ("no welcome mail for " ^ address))
      (fun address ->
        outbox (welcome_text address) ;
        match J.enqueue queue ~delay:tips_delay tips address with
        | Ok () ->
            ()
        | Error (`Full | `Stopped) ->
            report ("no tips mail for " ^ address) )
  in
  let signup req =
    match Request.form ~max_bytes:1024 req with
    | Error e ->
        Response.string ~status:(Form.error_status e)
          (Format.asprintf "%a\n" Form.pp_error e)
    | Ok form -> (
      match Form.find "email" form with
      | Some address when valid_address address -> (
        match J.enqueue queue welcome address with
        | Ok () ->
            Response.string ~status:Status.accepted
              (Printf.sprintf "We will send a welcome mail to %s.\n" address)
        | Error (`Full | `Stopped) ->
            Response.string ~status:Status.service_unavailable
              "Try again later.\n" )
      | _ ->
          Response.string ~status:Status.bad_request
            "The form needs a valid email.\n" )
  in
  let routes req =
    match (Request.meth req, Request.path_only req) with
    | POST, "/signup" ->
        signup req
    | GET, "/stats" ->
        Response.string (stats_text (J.stats queue))
    | _ ->
        Handler.not_found req
  in
  {queue; handler= routes}
