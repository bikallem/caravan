(* The tests of Dev_key. Each test sets a seeded generator first: the tests run
   on one domain, thus the Fortuna generator is correct here. The tests change
   the variable SESSION_KEY of this process. Unix.putenv cannot remove a
   variable, thus an empty value stands for no value. *)

open Windtrap
open Example_dev
module Key = Caravan_session.Key
module Signed = Caravan_session.Signed

let seed () =
  Mirage_crypto_rng.set_default_generator
    (Mirage_crypto_rng.create ~seed:"caravan dev key tests"
       (module Mirage_crypto_rng.Fortuna) )

(* [session text] is Dev_key.session with SESSION_KEY set to [text], and the
   lines of its log. *)
let session text =
  seed () ;
  Unix.putenv "SESSION_KEY" text ;
  let lines = ref [] in
  let key = Dev_key.session ~log:(fun l -> lines := l :: !lines) () in
  (key, List.rev !lines)

(* [same a b] is true if the key [b] verifies a value that the key [a] signs. *)
let same a b =
  let s = Signed.sign ~purpose:"test" a "value" in
  Signed.verify ~purpose:"test" [b] s = Some "value"

let is_invalid = function Invalid_argument _ -> true | _ -> false

let () =
  exit
  @@ Windtrap.run "dev_key"
       [ test "a SESSION_KEY in base64url gives its key, with no log line"
           (fun () ->
             let k = Key.of_string (String.make 32 'k') in
             let key, lines = session (Key.to_base64url k) in
             is_true (same k key) ;
             equal (list string) [] lines )
       ; test "no SESSION_KEY gives a new key and one line" (fun () ->
             let key, lines = session "" in
             is_true (same key key) ;
             equal int 1 (List.length lines) ;
             let line = List.hd lines in
             contains ~sub:"the sessions end at a restart" line ;
             contains ~sub:"SESSION_KEY=$(openssl rand -base64 32" line )
       ; test "the line does not show the new key" (fun () ->
             let key, lines = session "" in
             not_contains ~sub:(Key.to_base64url key) (List.hd lines) )
       ; test "each start with no SESSION_KEY gives another key" (fun () ->
             (* One seed: the second key comes from more bytes of the
                generator. *)
             seed () ;
             Unix.putenv "SESSION_KEY" "" ;
             let a = Dev_key.session ~log:ignore () in
             let b = Dev_key.session ~log:ignore () in
             is_false (same a b) )
       ; test "a SESSION_KEY that is not base64url stops the start" (fun () ->
             raises_match is_invalid (fun () -> ignore (session "not a key!")) )
       ; test "a SESSION_KEY of less than 32 bytes stops the start" (fun () ->
             (* 40 characters of base64url are 30 bytes. *)
             let k = Key.to_base64url (Key.of_string (String.make 32 'k')) in
             let short = String.sub k 0 40 in
             raises_match is_invalid (fun () -> ignore (session short)) ) ]
