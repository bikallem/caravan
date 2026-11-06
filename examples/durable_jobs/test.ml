(* The tests of the durable_jobs example. Each test runs the handler through the
   real server and client of caravan, in the simulator of weft. At the start,
   the POSIX clock of the simulator is 2024-03-01 12:00:00 UTC, or the time of
   the test. A test starts the queue and Recurring in a scope, and stops them
   before the end of the scope. The tests of the store call its functions
   directly. *)

open Windtrap
open Weft
open Caravan
module T = Caravan_testing
module D = Caravan_jobs.Durable
module Schedule = Caravan_jobs.Schedule
module Recurring = Caravan_jobs.Recurring
module App = Example_durable_jobs.App
module Store = Example_durable_jobs.Store

let status = Testable.make ~pp:Status.pp ~equal:Status.equal

let reply ?(code = Status.ok) body (r : T.reply) =
  equal ~msg:"status" status code r.status ;
  equal ~msg:"body" string body r.body

(* [at "2024-03-01 12:00:00"] is this time in UTC. *)
let at text =
  Scanf.sscanf text "%d-%d-%d %d:%d:%d"
    (fun year month day hour minute second ->
      Option.get
        (Time.of_utc {year; month; day; hour; minute; second; weekday= 0}) )

let show_time p =
  let u = Time.to_utc p in
  Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d" u.year u.month u.day u.hour
    u.minute u.second

let time =
  Testable.make
    ~pp:(fun ppf p -> Format.pp_print_string ppf (show_time p))
    ~equal:(fun a b -> Span.equal (Time.diff Posix a b) Span.zero)

let noon = "2024-03-01 12:00:00"

(* The request of curl -d: a form in the body. *)
let post_form t path form =
  T.fetch t
    (Request.make
       ~headers:
         (Headers.of_list
            [("content-type", "application/x-www-form-urlencoded")] )
       ~body:(Body.of_string form) POST path )

let signup t address = post_form t "/signup" ("email=" ^ address)

let export t user = post_form t "/export" ("user=" ^ user)

let jobs t = (T.get t "/jobs").body

(* [started app f] starts the queue and Recurring of [app] in a scope, runs [f
   ()], then stops them. *)
let started (a : App.t) f =
  Fiber.scope (fun sw ->
      D.start a.queue sw ;
      Recurring.start a.recurring sw ;
      let v = f () in
      Recurring.stop a.recurring ;
      D.stop ~grace:Span.zero a.queue ;
      v )

(* [with_app ~store ~outbox ~log ~report f] runs [f t app] with a new
   application on [store]. A cell holds the handler: the application is made in
   the run (rule 3). *)
let with_app ?manual ?(time = noon) ?(store = Store.create ())
    ?(outbox = ignore) ?(log = ignore) ?(report = ignore) f =
  let app = ref Handler.not_found in
  T.run ?manual
    (fun req -> !app req)
    (fun t ->
      Weft_sim.set_posix (T.sim t) (at time) ;
      let a = App.make ~store ~outbox ~log ~report in
      app := a.handler ;
      started a (fun () -> f t a) )

(* A list of lines, in order. *)
let lines () = ref []

let add l line = l := !l @ [line]

let welcome = App.welcome_text ~id:"0" "ann@example.com"

let tips = App.tips_text "ann@example.com"

let export_mail = App.export_text ~id:"0" "ann"

let app_tests =
  group "The application"
    [ test "POST /signup: 202, and the welcome mail at once (row and wake)"
        (fun () ->
          let mails = lines () and reports = lines () in
          with_app ~manual:true ~outbox:(add mails) ~report:(add reports)
            (fun t _ ->
              reply ~code:Status.accepted
                "Welcome, ann@example.com. We will send a mail.\n"
                (signup t "ann@example.com") ;
              T.settle t ;
              (* No advance of the time: wake makes the queue claim at once. *)
              equal (list string) [welcome] !mails ) ;
          equal (list string) [] !reports )
    ; test "the tips mail comes 10 s to 11.1 s after the welcome mail"
        (fun () ->
          let mails = lines () in
          with_app ~manual:true ~outbox:(add mails) (fun t _ ->
              ignore (signup t "ann@example.com") ;
              T.advance t (Span.ms 9999) ;
              equal (list string) [welcome] !mails ;
              (* The poll of 1 s, with a jitter of up to 10 %. *)
              T.advance t (Span.ms 1101) ;
              equal (list string) [welcome; tips] !mails ) ;
          equal string
            "To: ann@example.com\n\
             Idempotency-Key: tips-ann@example.com\n\n\
             Three tips.\n"
            tips )
    ; test "the tips job is in the store: another process sends the mail"
        (fun () ->
          let mails = lines () in
          let store = Store.create () in
          with_app ~store ~outbox:(add mails) (fun t a ->
              ignore (signup t "ann@example.com") ;
              T.settle t ;
              D.stop ~grace:Span.zero a.queue ;
              equal string "1 tips, attempts: 0\n" (jobs t) ;
              let b =
                App.make ~store ~outbox:(add mails) ~log:ignore ~report:ignore
              in
              started b (fun () -> D.drain b.queue) ) ;
          equal (list string) [welcome; tips] !mails )
    ; test "a second sign-up of the address: 409, and no second mail" (fun () ->
          let mails = lines () in
          with_app ~outbox:(add mails) (fun t a ->
              ignore (signup t "ann@example.com") ;
              reply ~code:Status.conflict "ann@example.com has an account.\n"
                (signup t "ann@example.com") ;
              D.drain a.queue ) ;
          equal (list string) [welcome; tips] !mails )
    ; test "a list of addresses gets 400" (fun () ->
          with_app (fun t _ ->
              reply ~code:Status.bad_request "The form needs a valid email.\n"
                (signup t "a@x.com,b@y.com") ) )
    ; test "valid_address: one @, a local part, a domain with a dot" (fun () ->
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
    ; test "a welcome mail that fails one time runs again with the same key"
        (fun () ->
          let calls = lines () and reports = lines () in
          let outbox mail =
            add calls mail ;
            if List.length !calls = 1 then failwith "down"
          in
          with_app ~outbox ~report:(add reports) (fun t a ->
              ignore (signup t "ann@example.com") ;
              D.drain a.queue ;
              equal int 1 (D.stats a.queue).retried ) ;
          equal (list string) [welcome; welcome; tips] !calls ;
          equal (list string) [] !reports )
    ; test "a welcome mail that always fails: 5 attempts, then a report"
        (fun () ->
          let calls = lines () and reports = lines () in
          let outbox mail = add calls mail ; failwith "down" in
          with_app ~outbox ~report:(add reports) (fun t a ->
              ignore (signup t "ann@example.com") ;
              D.drain a.queue ;
              equal string "" (jobs t) ) ;
          equal int 5 (List.length !calls) ;
          equal (list string)
            [{|welcome job 0: failed with Failure("down"), attempts: 5|}]
            !reports )
    ; test "POST /export two times: one job and one mail, then a new export"
        (fun () ->
          let mails = lines () in
          with_app ~outbox:(add mails) (fun t a ->
              reply ~code:Status.accepted "The export of ann is in the queue.\n"
                (export t "ann") ;
              reply ~code:Status.accepted
                "The export of ann is already in the queue.\n" (export t "ann") ;
              T.settle t ;
              reply "0 export, attempts: 1, running\n" (T.get t "/jobs") ;
              D.drain a.queue ;
              equal (list string) [export_mail] !mails ;
              (* complete deleted the job, and freed its key. *)
              equal string "" (jobs t) ;
              reply ~code:Status.accepted "The export of ann is in the queue.\n"
                (export t "ann") ) )
    ; test "the export takes 5 s" (fun () ->
          let mails = lines () in
          with_app ~manual:true ~outbox:(add mails) (fun t _ ->
              ignore (export t "ann") ;
              T.advance t (Span.ms 4999) ;
              equal (list string) [] !mails ;
              T.advance t (Span.ms 1) ;
              equal (list string) [export_mail] !mails ) ;
          equal string
            "To: ann\nIdempotency-Key: export-0\n\nYour export is ready.\n"
            export_mail )
    ; test "a user name with other characters gets 400" (fun () ->
          with_app (fun t _ ->
              List.iter
                (fun user ->
                  reply ~code:Status.bad_request
                    "The form needs a user: letters a to z and digits.\n"
                    (export t user) )
                [""; "Ann"; "a+b"; "a%0Ab"] ) )
    ; test "a stop gives the running export back, with no attempt used"
        (fun () ->
          let mails = lines () and reports = lines () in
          let store = Store.create () in
          with_app ~store ~outbox:(add mails) ~report:(add reports) (fun t a ->
              ignore (export t "ann") ;
              T.settle t ;
              equal string "0 export, attempts: 1, running\n" (jobs t) ;
              D.stop ~grace:(Span.s 1) a.queue ;
              equal string "0 export, attempts: 0\n" (jobs t) ;
              equal int 1 (D.stats a.queue).released ;
              equal (list string) [] !mails ;
              (* A second process on the store runs the job. *)
              let b =
                App.make ~store ~outbox:(add mails) ~log:ignore ~report:ignore
              in
              started b (fun () -> D.drain b.queue) ;
              equal (list string) [export_mail] !mails ;
              equal string "" (jobs t) ) ;
          equal (list string) [] !reports )
    ; test "a client gets 5 posts, then 429, then one more each 12 s" (fun () ->
          with_app ~manual:true (fun t _ ->
              ignore (signup t "ann@example.com") ;
              List.iter (fun u -> ignore (export t u)) ["a"; "b"; "c"; "d"] ;
              let r = signup t "bob@example.com" in
              equal status Status.too_many_requests r.status ;
              equal (option string) (Some "12")
                (Headers.find "retry-after" r.headers) ;
              equal status Status.too_many_requests (export t "u5").status ;
              T.advance t (Span.s 12) ;
              equal status Status.accepted (export t "u5").status ;
              equal status Status.too_many_requests (export t "u6").status ) )
    ]

let schedule_tests =
  group "Schedules and Recurring"
    [ test "the counts entry runs 3 times in 30 s" (fun () ->
          let logs = lines () and reports = lines () in
          with_app ~manual:true ~log:(add logs) ~report:(add reports)
            (fun t _ -> T.advance t (Span.s 30) ) ;
          equal (list string)
            [ "12:00:10 succeeded: 0, running: 0"
            ; "12:00:20 succeeded: 0, running: 0"
            ; "12:00:30 succeeded: 0, running: 0" ]
            !logs ;
          equal (list string) [] !reports )
    ; test "the counts tick at :00, :10, :20, ... of each minute" (fun () ->
          let next p = Option.get (Schedule.next App.counts_schedule (at p)) in
          equal time (at "2024-03-01 12:00:10") (next "2024-03-01 12:00:00") ;
          equal time (at "2024-03-01 12:00:10") (next "2024-03-01 12:00:05") ;
          equal time (at "2024-03-01 12:01:00") (next "2024-03-01 12:00:55") )
    ; test "Schedule.next of the nightly schedule gives 03:00 UTC" (fun () ->
          let next p = Option.get (Schedule.next App.nightly (at p)) in
          equal time (at "2024-03-02 03:00:00") (next "2024-03-01 12:00:30") ;
          equal time (at "2024-03-03 03:00:00") (next "2024-03-02 03:00:00") ;
          equal time (at "2024-03-02 03:00:00") (next "2024-03-02 02:59:59") )
    ; test "at 03:00 UTC, two processes on one store send one nightly report"
        (fun () ->
          let mails = lines () in
          let store = Store.create () in
          with_app ~manual:true ~time:"2024-03-02 02:59:55" ~store
            ~outbox:(add mails) (fun t _ ->
              let b =
                App.make ~store ~outbox:(add mails) ~log:ignore ~report:ignore
              in
              started b (fun () ->
                  T.advance t (Span.s 4) ;
                  equal (list string) [] !mails ;
                  T.advance t (Span.s 1) ;
                  T.settle t ) ) ;
          equal (list string)
            ["To: admin@example.com\nThe nightly report of 2024-03-02.\n"]
            !mails ) ]

(* The tests of the store: each test calls the functions of the job store in a
   run, at 12:00:00 and after. *)
let with_store f =
  T.run ~manual:true Handler.not_found (fun t ->
      Weft_sim.set_posix (T.sim t) (at noon) ;
      let st = Store.create () in
      (* [set s] makes the clock jump to [s] seconds after 12:00:00. *)
      let set s =
        Weft_sim.set_posix (T.sim t) (Option.get (Time.add Posix (at noon) s))
      in
      f st (Store.store st) set )

let after s = Option.get (Time.add Posix (at noon) s)

let row ?(name = "a") ?(due = Span.zero) payload : D.row =
  {name; payload; due= after due}

let claims (cs : D.claim list) =
  String.concat " "
    (List.map
       (fun (c : D.claim) -> Printf.sprintf "%s:%s:%d" c.id c.payload c.attempts)
       cs )

let claim ?(names = ["a"]) ?(lease = Span.s 10) (s : D.store) max =
  s.claim ~names ~max ~lease

let one = function [c] -> c | _ -> fail "not one claim"

let store_tests =
  group "The store"
    [ test "claim: the due jobs of the names, by due time, then by push"
        (fun () ->
          with_store (fun _ s _ ->
              let _ = s.push (row "x") in
              let _ = s.push (row ~due:(Span.s (-1)) "y") in
              let _ = s.push (row ~name:"b" "z") in
              let _ = s.push (row ~due:(Span.s 5) "later") in
              let _ = s.push (row "w") in
              equal string "1:y:1 0:x:1" (claims (claim s 2)) ;
              equal string "4:w:1" (claims (claim s 5)) ;
              equal string "" (claims (claim s 5)) ) )
    ; test "a live lease hides the job, and the old token changes nothing"
        (fun () ->
          with_store (fun _ s set ->
              let id = s.push (row "x") in
              let c1 = one (claim s 1) in
              set (Span.s 9) ;
              equal string "" (claims (claim s 1)) ;
              set (Span.s 10) ;
              let c2 = one (claim s 1) in
              equal string "0:x:2" (claims [c2]) ;
              is_true ~msg:"a new token" (c1.lease <> c2.lease) ;
              let lease = c1.lease in
              is_false ~msg:"extend" (s.extend ~id ~lease (Span.s 1)) ;
              is_false ~msg:"retry" (s.retry ~id ~lease ~due:(after Span.zero)) ;
              is_false ~msg:"release" (s.release ~id ~lease) ;
              is_false ~msg:"fail" (s.fail ~id ~lease ~error:"e") ;
              is_false ~msg:"complete" (s.complete ~id ~lease) ;
              is_true ~msg:"complete" (s.complete ~id ~lease:c2.lease) ;
              is_false ~msg:"a second complete" (s.complete ~id ~lease:c2.lease) ) )
    ; test "extend moves the end of the lease" (fun () ->
          with_store (fun _ s set ->
              let id = s.push (row "x") in
              let c = one (claim s 1) in
              set (Span.s 5) ;
              is_true ~msg:"extend" (s.extend ~id ~lease:c.lease (Span.s 10)) ;
              set (Span.s 14) ;
              equal string "" (claims (claim s 1)) ;
              set (Span.s 15) ;
              equal string "0:x:2" (claims (claim s 1)) ) )
    ; test "retry sets the due time, and release gives the attempt back"
        (fun () ->
          with_store (fun _ s set ->
              let id = s.push (row "x") in
              let c = one (claim s 1) in
              is_true ~msg:"retry"
                (s.retry ~id ~lease:c.lease ~due:(after (Span.s 3))) ;
              equal string "" (claims (claim s 1)) ;
              set (Span.s 3) ;
              let c = one (claim s 1) in
              equal string "0:x:2" (claims [c]) ;
              is_true ~msg:"release" (s.release ~id ~lease:c.lease) ;
              equal string "0:x:2" (claims (claim s 1)) ) )
    ; test "push_unique: one job of a name and a key, until its end" (fun () ->
          with_store (fun _ s _ ->
              let push_unique = Option.get s.push_unique in
              let id = Option.get (push_unique (row "x") ~key:"k") in
              equal (option string) None (push_unique (row "y") ~key:"k") ;
              is_true ~msg:"another name"
                (Option.is_some (push_unique (row ~name:"b" "z") ~key:"k")) ;
              let c = one (claim s 1) in
              is_true ~msg:"fail" (s.fail ~id ~lease:c.lease ~error:"e") ;
              equal string "" (claims (claim s 1)) ;
              is_true ~msg:"after the end"
                (Option.is_some (push_unique (row "w") ~key:"k")) ) )
    ; test "push_unique: a retry and a release keep the key" (fun () ->
          with_store (fun _ s _ ->
              let push_unique = Option.get s.push_unique in
              let id = Option.get (push_unique (row "x") ~key:"k") in
              let c = one (claim s 1) in
              is_true ~msg:"retry"
                (s.retry ~id ~lease:c.lease ~due:(after Span.zero)) ;
              equal (option string) None (push_unique (row "y") ~key:"k") ;
              let c = one (claim s 1) in
              is_true ~msg:"release" (s.release ~id ~lease:c.lease) ;
              equal (option string) None (push_unique (row "y") ~key:"k") ) )
    ; test "push_tick: a tick after the last one, at most 5 min ahead"
        (fun () ->
          with_store (fun _ s _ ->
              let push_tick = Option.get s.push_tick in
              let tick s = push_tick (row "t") ~entry:"e" ~tick:(after s) in
              let some msg v = is_true ~msg (Option.is_some v) in
              let none msg v = is_true ~msg (Option.is_none v) in
              none "6 min ahead" (tick (Span.s 360)) ;
              some "the first tick" (tick (Span.s 60)) ;
              none "the same tick" (tick (Span.s 60)) ;
              none "an earlier tick" (tick (Span.s 30)) ;
              some "5 min ahead" (tick (Span.s 300)) ;
              some "another entry"
                (push_tick (row "t") ~entry:"f" ~tick:(after (Span.s 60))) ;
              equal int 3 (List.length (claim s 10)) ) )
    ; test "next_due: the first due time, or the end of a live lease" (fun () ->
          with_store (fun _ s _ ->
              let next_due () =
                Option.get s.next_due ~names:["a"] |> Option.map show_time
              in
              equal (option string) None (next_due ()) ;
              let _ = s.push (row ~due:(Span.s 20) "x") in
              let _ = s.push (row ~name:"b" "y") in
              equal (option string) (Some "2024-03-01 12:00:20") (next_due ()) ;
              let _ = s.push (row "z") in
              let _ = one (claim s 1) in
              equal (option string) (Some "2024-03-01 12:00:10") (next_due ()) ) )
    ; test "sign_up adds the user and the job, and nothing for a known user"
        (fun () ->
          with_store (fun st s _ ->
              equal (option string) (Some "0")
                (Store.sign_up st "ann@example.com" (row "welcome")) ;
              equal (option string) None
                (Store.sign_up st "ann@example.com" (row "again")) ;
              equal string "0:welcome:1" (claims (claim s 10)) ) ) ]

let () =
  exit @@ Windtrap.run "durable_jobs" [app_tests; schedule_tests; store_tests]
