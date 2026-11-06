(* The tests of the jobs example. Each test runs the handler through the real
   server and client of caravan, in the simulator of weft: the delays and the
   backoff need no real time. Each test starts the queue in a scope, and stops
   it with no grace before the end of the scope. The outbox of a test keeps the
   mails, or makes them fail. *)

open Windtrap
open Weft
open Caravan
module T = Caravan_testing
module J = Caravan_jobs
module App = Example_jobs.App

let status = Testable.make ~pp:Status.pp ~equal:Status.equal

let span = Testable.make ~pp:Span.pp ~equal:Span.equal

let reply ?(code = Status.ok) body (r : T.reply) =
  equal ~msg:"status" status code r.status ;
  equal ~msg:"body" string body r.body

(* The request of curl -d: a form in the body. *)
let post_form t path form =
  T.fetch t
    (Request.make
       ~headers:
         (Headers.of_list
            [("content-type", "application/x-www-form-urlencoded")] )
       ~body:(Body.of_string form) POST path )

let signup t address = post_form t "/signup" ("email=" ^ address)

let accepted address =
  reply ~code:Status.accepted
    (Printf.sprintf "We will send a welcome mail to %s.\n" address)

(* [with_app ~outbox f] runs [f t queue] with a new application, and gives its
   value and the lines of [report], also those of the stop. A cell holds the
   handler. The function of the run makes the application, thus each run starts
   with new state. *)
let with_app ?manual ?(outbox = ignore) f =
  let reports = ref [] in
  let report line = reports := line :: !reports in
  let app = ref Handler.not_found in
  let v =
    T.run ?manual
      (fun req -> !app req)
      (fun t ->
        let a = App.make ~outbox ~report in
        app := a.handler ;
        Fiber.scope (fun sw ->
            J.start a.queue sw ;
            let v = f t a.queue in
            J.stop ~grace:Span.zero a.queue ;
            v ) )
  in
  (v, List.rev !reports)

(* An outbox that keeps the mails in [mails], in order. *)
let keep mails mail = mails := !mails @ [mail]

let welcome = App.welcome_text "ann@example.com"

let tips = App.tips_text "ann@example.com"

(* [count t name] is the value of the line [name] of /stats. *)
let count t name =
  String.split_on_char '\n' (T.get t "/stats").body
  |> List.find_map (fun line ->
      match String.split_on_char ':' line with
      | [n; v] when n = name ->
          int_of_string_opt (String.trim v)
      | _ ->
          None )

(* [is_between lo hi d] checks that [d] is from [lo] to [hi]. *)
let is_between lo hi d =
  is_true
    ~msg:(Format.asprintf "%a is from %a to %a" Span.pp d Span.pp lo Span.pp hi)
    (Span.compare lo d <= 0 && Span.compare d hi <= 0)

let () =
  exit
  @@ Windtrap.run "jobs"
       [ test "POST /signup: 202 at once, then the two mails" (fun () ->
             let mails = ref [] in
             let (), reports =
               with_app ~outbox:(keep mails) (fun t q ->
                   accepted "ann@example.com" (signup t "ann@example.com") ;
                   J.drain q )
             in
             equal (list string) [welcome; tips] !mails ;
             equal (list string) [] reports )
       ; test "the tips mail starts 10 s after the welcome mail" (fun () ->
             let times = ref [] in
             let outbox _ = times := !times @ [Time.now Mono] in
             let (), _ =
               with_app ~manual:true ~outbox (fun t _ ->
                   accepted "ann@example.com" (signup t "ann@example.com") ;
                   T.advance t (Span.s 9) ;
                   equal ~msg:"after 9 s" int 1 (List.length !times) ;
                   T.advance t (Span.s 1) )
             in
             match !times with
             | [a; b] ->
                 equal span (Span.s 10) (Time.diff Mono b a)
             | _ ->
                 fail "not two mails" )
       ; test "GET /stats gives the counts: 2 mails succeeded" (fun () ->
             let (), _ =
               with_app (fun t q ->
                   ignore (signup t "ann@example.com") ;
                   J.drain q ;
                   reply
                     "waiting: 0\n\
                      delayed: 0\n\
                      running: 0\n\
                      succeeded: 2\n\
                      retried: 0\n\
                      failed: 0\n"
                     (T.get t "/stats") )
             in
             () )
       ; test "a form with no email gets 400" (fun () ->
             let (), _ =
               with_app (fun t _ ->
                   reply ~code:Status.bad_request
                     "The form needs a valid email.\n"
                     (post_form t "/signup" "name=ann") )
             in
             () )
       ; test "a list of addresses gets 400" (fun () ->
             let (), _ =
               with_app (fun t _ ->
                   reply ~code:Status.bad_request
                     "The form needs a valid email.\n"
                     (signup t "a@x.com,b@y.com") )
             in
             () )
       ; test "valid_address: one @, a local part, a domain with a dot"
           (fun () ->
             List.iter
               (fun a -> is_true ~msg:a (App.valid_address a))
               ["ann@example.com"; "a.b+c@mail-1.example.org"] ;
             List.iter
               (fun a -> is_false ~msg:(String.escaped a) (App.valid_address a))
               [ "a@x.com,b@y.com"
               ; "a,b@example.com"
               ; "a;b@example.com"
               ; "<a>@example.com"
               ; {|"a"@example.com|}
               ; "a(b)@example.com"
               ; {|a\b@example.com|}
               ; "a b@example.com"
               ; "ann@example.com\nBcc:eve@example.com"
               ; "a\001b@example.com"
               ; "a\127b@example.com"
               ; "ann"
               ; "@example.com"
               ; "ann@example"
               ; "ann@exa_mple.com" ] )
       ; test "a welcome mail that fails twice runs 3 times, 2 s then 4 s apart"
           (fun () ->
             let times = ref [] and mails = ref [] in
             let outbox mail =
               times := !times @ [Time.now Mono] ;
               if List.length !times <= 2 then failwith "the service is down" ;
               keep mails mail
             in
             let (), reports =
               with_app ~manual:true ~outbox (fun t q ->
                   accepted "ann@example.com" (signup t "ann@example.com") ;
                   (* The attempts 2 and 3, and the tips mail 10 s later. *)
                   T.advance t (Span.s 20) ;
                   equal (option int) (Some 2) (count t "retried") ;
                   equal (option int) (Some 2) (count t "succeeded") ;
                   ignore q )
             in
             equal (list string) [welcome; tips] !mails ;
             equal (list string) [] reports ;
             match !times with
             | [a1; a2; a3; _] ->
                 (* 2 s and 4 s, less a jitter of up to 15 %. *)
                 is_between (Span.ms 1700) (Span.s 2) (Time.diff Mono a2 a1) ;
                 is_between (Span.ms 3400) (Span.s 4) (Time.diff Mono a3 a2)
             | _ ->
                 fail "not 4 calls of the outbox" )
       ; test "a welcome mail that fails 3 times: on_failure, then on_error"
           (fun () ->
             let calls = ref 0 in
             let outbox _ = incr calls ; failwith "down" in
             let (), reports =
               with_app ~outbox (fun t q ->
                   ignore (signup t "ann@example.com") ;
                   J.drain q )
             in
             equal int 3 !calls ;
             equal (list string)
               [ "no welcome mail for ann@example.com"
               ; {|welcome job 0: failed with Failure("down"), attempts: 3|} ]
               reports )
       ; test "a Rejected address gets no more attempt" (fun () ->
             let calls = ref 0 in
             let outbox _ =
               incr calls ;
               raise (App.Rejected "ann@example.com")
             in
             let (), reports =
               with_app ~outbox (fun t q ->
                   ignore (signup t "ann@example.com") ;
                   J.drain q )
             in
             equal int 1 !calls ;
             equal (list string)
               [ "no welcome mail for ann@example.com"
               ; "welcome job 0: discarded after \
                  Example_jobs.App.Rejected(\"ann@example.com\"), attempts: 1"
               ]
               reports )
       ; test "a tips mail that fails gets one attempt" (fun () ->
             let calls = ref 0 in
             let outbox mail =
               if mail = tips then (incr calls ; failwith "down")
             in
             let (), reports =
               with_app ~outbox (fun t q ->
                   ignore (signup t "ann@example.com") ;
                   J.drain q )
             in
             equal int 1 !calls ;
             equal (list string)
               [{|tips job 1: failed with Failure("down"), attempts: 1|}]
               reports )
       ; test "2 jobs run and 100 wait: the next sign-up gets 503" (fun () ->
             let gate = Ivar.create () in
             let outbox _ = sync (Ivar.read gate) in
             let (), reports =
               with_app ~outbox (fun t _ ->
                   for i = 1 to 102 do
                     let address = Printf.sprintf "u%d@example.com" i in
                     accepted address (signup t address) ;
                     T.settle t
                   done ;
                   reply ~code:Status.service_unavailable "Try again later.\n"
                     (signup t "late@example.com") ;
                   reply
                     "waiting: 100\n\
                      delayed: 0\n\
                      running: 2\n\
                      succeeded: 0\n\
                      retried: 0\n\
                      failed: 0\n"
                     (T.get t "/stats") )
             in
             (* The stop with no grace: on_failure and on_error get each of the
                102 jobs. *)
             equal int 204 (List.length reports) )
       ; test "a stop during the welcome mail: the tips mail gets `Stopped"
           (fun () ->
             let gate = Ivar.create () and mails = ref [] in
             let outbox mail =
               sync (Ivar.read gate) ;
               keep mails mail
             in
             let (), reports =
               with_app ~outbox (fun t q ->
                   accepted "ann@example.com" (signup t "ann@example.com") ;
                   T.settle t ;
                   let stop =
                     T.background t (fun () -> J.stop ~grace:(Span.s 5) q)
                   in
                   T.settle t ; Ivar.fill gate () ; Promise.await stop )
             in
             equal (list string) [welcome] !mails ;
             equal (list string) ["no tips mail for ann@example.com"] reports )
       ]
