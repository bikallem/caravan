(* A small counter service. Its tests in test.ml are the example: they show each
   function of caravan.testing.

   Run dune exec examples/testing/main.exe, then send these requests:

   curl -i -X POST 127.0.0.1:8080/count

   curl -i 127.0.0.1:8080/count

   /slow gives the count after 5 s. /boom raises an exception, thus the server
   gives 500:

   curl -i 127.0.0.1:8080/slow

   curl -i 127.0.0.1:8080/boom *)

open Weft
open Caravan

(* The counter. An increment reads the value, waits 1 ms (a stand-in for the
   round trip to a database), then writes the value. The mutex makes the read
   and the write one step: without it, two increments at the same time read the
   same value, and one of them is lost.

   With ~protect:true, a client that leaves does not stop an increment between
   the read and the write. Without it, the cancel of the handler raises in the
   sleep. That exception poisons the mutex, and each later request of /count
   gets 500. *)
type counter = {lock: Fiber.Mutex.t; mutable value: int}

let increment c =
  Fiber.Mutex.use_rw ~protect:true c.lock (fun () ->
      let v = c.value in
      Time.sleep Mono (Span.ms 1) ;
      c.value <- v + 1 ;
      c.value )

let value c = Fiber.Mutex.use_ro c.lock (fun () -> c.value)

let count_text n = Printf.sprintf "count: %d\n" n

(* [make ()] is the handler, with a new counter at 0. *)
let make () =
  let c = {lock= Fiber.Mutex.create (); value= 0} in
  fun req ->
    match (Request.meth req, Request.path_only req) with
    | POST, "/count" ->
        Response.string (count_text (increment c))
    | GET, "/count" ->
        Response.string (count_text (value c))
    | GET, "/slow" ->
        Time.sleep Mono (Span.s 5) ;
        Response.string (count_text (value c))
    | GET, "/boom" ->
        failwith "boom"
    | _ ->
        Handler.not_found req
