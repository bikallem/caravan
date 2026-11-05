(* The tests of Caravan_jobs.Schedule: known answers, the errors of the parser,
   and a property against a second matcher, by brute force. *)

open Windtrap
open Weft
module Sc = Caravan_jobs.Schedule

(* "YYYY-MM-DD HH:MM:SS" in UTC. *)
let at text =
  Scanf.sscanf text "%d-%d-%d %d:%d:%d"
    (fun year month day hour minute second ->
      Option.get
        (Time.of_utc {year; month; day; hour; minute; second; weekday= 0}) )

let show p =
  let u = Time.to_utc p in
  Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d" u.year u.month u.day u.hour
    u.minute u.second

let show_next s after =
  Option.fold ~none:"None" ~some:show (Sc.next s (at after))

let invalid f =
  match f () with
  | _ ->
      fail "no Invalid_argument"
  | exception Invalid_argument _ ->
      ()

let every_tests =
  group "every"
    [ test "the ticks are multiples of the span since the epoch" (fun () ->
          let s = Sc.every (Span.s 300) in
          equal string "2024-03-01 12:05:00" (show_next s "2024-03-01 12:03:10") ;
          (* Strictly after. *)
          equal string "2024-03-01 12:10:00" (show_next s "2024-03-01 12:05:00") ;
          let day = Sc.every (Span.s 86_400) in
          equal string "2024-03-02 00:00:00"
            (show_next day "2024-03-01 00:00:00") ;
          let one = Sc.every (Span.s 1) in
          equal string "2024-03-01 12:00:01"
            (Option.fold ~none:"None" ~some:show
               (Sc.next one
                  (Option.get
                     (Time.add Posix (at "2024-03-01 12:00:00") (Span.ms 500)) ) ) ) ;
          (* 7 min does not start at :00 of each hour. *)
          equal string "2024-03-01 12:03:00"
            (show_next (Sc.every (Span.s 420)) "2024-03-01 12:00:00") )
    ; test "times before 1970" (fun () ->
          let h = Sc.every (Span.s 3600) in
          equal string "1970-01-01 00:00:00" (show_next h "1969-12-31 23:30:00") ;
          equal string "1969-12-31 23:00:00" (show_next h "1969-12-31 22:00:00") )
    ; test "the start of the range of the clock (1677)" (fun () ->
          let first =
            Option.get
              (Time.add Posix (Time.posix_of_unix_s 0.)
                 (Span.of_ns Int64.min_int) )
          in
          let show_opt = Option.fold ~none:"None" ~some:show in
          equal string "1677-09-21 00:13:00"
            (show_opt (Sc.next (Sc.every (Span.s 60)) first)) ;
          equal string "1677-09-21 00:13:00"
            (show_opt (Sc.next (Sc.cron "* * * * *") first)) ;
          equal string "1677-09-21 13:00:00"
            (show_next (Sc.cron "0 13 * * *") "1677-09-21 12:00:00") ;
          equal string "1677-09-22 00:00:00"
            (show_next (Sc.every (Span.s 86_400)) "1677-09-21 12:00:00") )
    ; test "the span: whole seconds from 1 s to 1 day" (fun () ->
          invalid (fun () -> Sc.every (Span.ms 999)) ;
          invalid (fun () -> Sc.every (Span.ms 1500)) ;
          invalid (fun () -> Sc.every (Span.s 86_401)) ;
          ignore (Sc.every (Span.s 1)) ;
          ignore (Sc.every (Span.s 86_400)) )
    ; test "after the end of the range of the clock: None" (fun () ->
          let last =
            Option.get
              (Time.add Posix (Time.posix_of_unix_s 0.)
                 (Span.of_ns (Int64.sub Int64.max_int 1L)) )
          in
          is_true ~msg:"every" (Sc.next (Sc.every (Span.s 1)) last = None) ;
          (* The last tick in the range: [m] is the last multiple of the
             span. *)
          let epoch = Time.posix_of_unix_s 0. in
          let pos ns = Option.get (Time.add Posix epoch (Span.of_ns ns)) in
          let last_multiple d = Int64.mul (Int64.div Int64.max_int d) d in
          let check d s =
            let m = last_multiple d in
            equal (option int64) ~msg:"the last tick" (Some m)
              (Option.map
                 (fun p -> Span.to_ns (Time.diff Posix p epoch))
                 (Sc.next s (pos (Int64.sub m d))) )
          in
          check 1_000_000_000L (Sc.every (Span.s 1)) ;
          check 60_000_000_000L (Sc.cron "* * * * *") ;
          is_true ~msg:"cron" (Sc.next (Sc.cron "* * * * *") last = None) ;
          (* The last day of the range has no next day. *)
          let late = Time.posix_of_unix_s 9_223_300_000. in
          is_true ~msg:"cron of a later day"
            (Sc.next (Sc.cron "0 0 1 1 *") late = None) ) ]

let cron_cases =
  [ (* expression, after, next *)
    ("* * * * *", "2024-03-01 12:03:10", "2024-03-01 12:04:00")
  ; ("0 3 * * *", "2024-03-01 03:00:00", "2024-03-02 03:00:00")
  ; ("0 3 * * *", "2024-03-01 02:59:59", "2024-03-01 03:00:00")
  ; ("*/15 9-17 * * 1-5", "2024-03-01 17:46:00", "2024-03-04 09:00:00")
  ; ("*/15 9-17 * * 1-5", "2024-03-01 10:01:00", "2024-03-01 10:15:00")
  ; ("30 */6 * * *", "2024-03-01 18:30:00", "2024-03-02 00:30:00")
  ; ("0 12 1-10/3 * *", "2024-03-02 00:00:00", "2024-03-04 12:00:00")
    (* The Vixie rule: both restricted is OR; a star (also */2) is AND. *)
  ; ("0 0 1,15 * 1", "2024-03-01 00:00:00", "2024-03-04 00:00:00")
  ; ("0 0 1,15 * 1", "2024-03-11 00:00:00", "2024-03-15 00:00:00")
  ; ("0 0 1-31 * 1", "2024-03-01 00:00:00", "2024-03-02 00:00:00")
  ; ("0 0 */2 * 1", "2024-03-01 00:00:00", "2024-03-11 00:00:00")
  ; ("0 0 * * 1", "2024-03-01 00:00:00", "2024-03-04 00:00:00")
  ; ("0 0 13 * 5", "2024-03-01 00:00:00", "2024-03-08 00:00:00")
    (* Day 7 is Sunday, also in a range. *)
  ; ("0 0 * * 7", "2024-03-01 00:00:00", "2024-03-03 00:00:00")
  ; ("0 0 * * 5-7", "2024-03-01 00:00:00", "2024-03-02 00:00:00")
  ; ("0 0 * * fri-SAT", "2024-03-03 00:00:00", "2024-03-08 00:00:00")
    (* Month ends, 29 February, the year end. *)
  ; ("0 0 31 * *", "2024-04-01 00:00:00", "2024-05-31 00:00:00")
  ; ("0 0 29 2 *", "2024-03-01 00:00:00", "2028-02-29 00:00:00")
  ; ("0 0 29 2 *", "2096-03-01 00:00:00", "2104-02-29 00:00:00")
  ; ("59 23 31 12 *", "2024-12-31 23:59:00", "2025-12-31 23:59:00")
  ; ("0 0 1 jan *", "2024-12-31 23:59:59", "2025-01-01 00:00:00")
  ; ("0 0 1 JUN-aug/2 *", "2024-06-02 00:00:00", "2024-08-01 00:00:00")
    (* Macros. *)
  ; ("@hourly", "2024-03-01 12:00:00", "2024-03-01 13:00:00")
  ; ("@daily", "2024-03-01 12:00:00", "2024-03-02 00:00:00")
  ; ("@midnight", "2024-03-01 12:00:00", "2024-03-02 00:00:00")
  ; ("@weekly", "2024-03-01 12:00:00", "2024-03-03 00:00:00")
  ; ("@monthly", "2024-03-01 12:00:00", "2024-04-01 00:00:00")
  ; ("@yearly", "2024-03-01 12:00:00", "2025-01-01 00:00:00")
  ; ("@annually", "2024-03-01 12:00:00", "2025-01-01 00:00:00")
  ; ("@Daily", "2024-03-01 12:00:00", "2024-03-02 00:00:00")
    (* Spaces, tabs, leading zeros, a step larger than the range. *)
  ; ("  05\t*  * *   * ", "2024-03-01 12:03:00", "2024-03-01 12:05:00")
  ; ("*/100 * * * *", "2024-03-01 12:03:00", "2024-03-01 13:00:00")
  ; ("000000005 * * * *", "2024-03-01 12:03:00", "2024-03-01 12:05:00")
  ; ("0 0 1 1 *", "1969-06-01 00:00:00", "1970-01-01 00:00:00") ]

let error_cases =
  [ ("60 * * * *", "minute: 60 is out of the range 0-59")
  ; ("* 24 * * *", "hour: 24 is out of the range 0-23")
  ; ("* * 0 * *", "day of the month: 0 is out of the range 1-31")
  ; ("* * 32 * *", "day of the month: 32 is out of the range 1-31")
  ; ("* * * 13 *", "month: 13 is out of the range 1-12")
  ; ("* * * * 8", "day of the week: 8 is out of the range 0-7")
  ; ("* * * *", "\"* * * *\": a cron expression has 5 fields, or is a macro")
  ; ( "* * * * * *"
    , "\"* * * * * *\": a cron expression has 5 fields, or is a macro" )
  ; ("@reboot", "\"@reboot\": a cron expression has 5 fields, or is a macro")
  ; ("*/0 * * * *", "minute: the step of */0 is zero")
  ; ("5/2 * * * *", "minute: a step needs * or a range: 5/2")
  ; ("5-2 * * * *", "minute: the range 5-2 goes down")
  ; ("1,,2 * * * *", "minute: an empty item in \"1,,2\"")
  ; ("0x1 * * * *", "minute: \"0x1\" is not a number")
  ; ("1_0 * * * *", "minute: \"1_0\" is not a number")
  ; ("+5 * * * *", "minute: \"+5\" is not a number")
  ; ("1000000000 * * * *", "minute: 1000000000 is too large")
  ; ("*/x * * * *", "minute: \"x\" is not a number")
  ; ("* * * foo *", "month: \"foo\" is not a number")
  ; ("* * * * mon-", "day of the week: \"\" is not a number")
  ; ("? * * * *", "minute: \"?\" is not a number")
  ; ("* * * * sat-sun", "day of the week: the range sat-sun goes down")
  ; ("0 0 30 2 *", "the expression matches no date")
  ; ("0 0 31 4,6,9,11 *", "the expression matches no date") ]

let cron_tests =
  group "cron"
    [ test "known answers" (fun () ->
          List.iter
            (fun (e, after, next) ->
              equal string ~msg:e next (show_next (Sc.cron e) after) )
            cron_cases )
    ; test "errors" (fun () ->
          List.iter
            (fun (e, m) ->
              ( match Sc.of_string e with
              | Ok _ ->
                  fail ("no error: " ^ e)
              | Error got ->
                  equal string ~msg:e m got ) ;
              match Sc.cron e with
              | _ ->
                  fail ("no exception: " ^ e)
              | exception Invalid_argument got ->
                  equal string ~msg:e ("Caravan_jobs.Schedule.cron: " ^ m) got )
            error_cases )
    ; test "a restricted day that some month has: no error" (fun () ->
          (* Both restricted: OR, the days of the week match. *)
          ignore (Sc.cron "0 0 30 2 1") ;
          ignore (Sc.cron "0 0 29 2 *") ;
          ignore (Sc.cron "0 0 31 1-12 *") ) ]

(* The property: a second matcher, from the sets that the generator chose. *)
type gen = {text: string; sets: bool array array; stars: bool * bool}

let ranges = [|(0, 59); (0, 23); (1, 31); (1, 12); (0, 6)|]

let gen_field rng i =
  let lo, hi = ranges.(i) in
  let set = Array.make 60 false in
  let pick () = lo + Random.State.int rng (hi - lo + 1) in
  let star = Random.State.int rng 4 = 0 in
  let text =
    if star then (
      let step = 1 + Random.State.int rng 5 in
      let v = ref lo in
      while !v <= hi do
        set.(!v) <- true ;
        v := !v + step
      done ;
      if step = 1 then "*" else Printf.sprintf "*/%d" step )
    else
      String.concat ","
        (List.init
           (1 + Random.State.int rng 3)
           (fun _ ->
             let a = pick () and b = pick () in
             let a, b = (min a b, max a b) in
             if Random.State.bool rng then (
               set.(a) <- true ;
               string_of_int a )
             else
               let step = 1 + Random.State.int rng 3 in
               let v = ref a in
               while !v <= b do
                 set.(!v) <- true ;
                 v := !v + step
               done ;
               Printf.sprintf "%d-%d/%d" a b step ) )
  in
  (text, set, star)

(* One case of four has a day of the month that some months do not have: the
   long searches and the expressions that match no date. *)
let rare_date rng =
  let pick l = List.nth l (Random.State.int rng (List.length l)) in
  let dom = pick [29; 30; 31] and month = pick [2; 4; 6; 9; 11] in
  let only v = Array.init 60 (fun i -> i = v) in
  ( Printf.sprintf "0 0 %d %d *" dom month
  , [|only 0; only 0; only dom; only month; Array.make 60 true|] )

let gen rng =
  if Random.State.int rng 4 = 0 then
    let text, sets = rare_date rng in
    {text; sets; stars= (false, true)}
  else
    let fields = List.init 5 (gen_field rng) in
    let text = String.concat " " (List.map (fun (t, _, _) -> t) fields) in
    let sets = Array.of_list (List.map (fun (_, s, _) -> s) fields) in
    let star i = List.nth fields i |> fun (_, _, s) -> s in
    {text; sets; stars= (star 2, star 4)}

(* The first matching minute after [t], by brute force: day by day, then each
   minute of a day that matches. At most 45 years: [0 0 29 2 */7] waits 40
   years. *)
let brute g t =
  let dom_star, dow_star = g.stars in
  let start =
    Int64.succ (Int64.of_float (floor (Time.posix_to_unix_s t /. 60.)))
  in
  let rec day d n =
    if n > 16_500 then None
    else
      let p = Time.posix_of_unix_s (Int64.to_float d *. 86_400.) in
      let u = Time.to_utc p in
      let day_ok =
        if dom_star || dow_star then
          g.sets.(2).(u.day) && g.sets.(4).(u.weekday)
        else g.sets.(2).(u.day) || g.sets.(4).(u.weekday)
      in
      let first = Int64.mul d 1440L in
      let rec minute m =
        if m >= 1440 then None
        else
          let abs = Int64.add first (Int64.of_int m) in
          if
            Int64.compare abs start >= 0
            && g.sets.(1).(m / 60)
            && g.sets.(0).(m mod 60)
          then Some (Time.posix_of_unix_s (Int64.to_float abs *. 60.))
          else minute (m + 1)
      in
      match if g.sets.(3).(u.month) && day_ok then minute 0 else None with
      | Some p ->
          Some p
      | None ->
          day (Int64.succ d) (n + 1)
  in
  day (Int64.div start 1440L) 0

let property_tests =
  group "Property"
    [ test "next is the first matching minute after the time" (fun () ->
          let rng = Random.State.make [|42|] in
          for _ = 1 to 3000 do
            let g = gen rng in
            let t =
              Time.posix_of_unix_s (Random.State.float rng 4_000_000_000. +. 0.5)
            in
            let expected = Option.map show (brute g t) in
            match Sc.of_string g.text with
            | Error m ->
                (* Only an expression that matches no date. *)
                equal string ~msg:g.text "the expression matches no date" m ;
                is_true ~msg:g.text (expected = None)
            | Ok s ->
                equal (option string)
                  ~msg:(g.text ^ " after " ^ show t)
                  expected
                  (Option.map show (Sc.next s t))
          done ) ]

let () =
  exit (Windtrap.run "schedule" [every_tests; cron_tests; property_tests])
