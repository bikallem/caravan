(* A stateful test of HTTP/1.1 connections: generated programs of pipelined
   requests and response reads on several connections, against a model of what
   the server must answer (RFC 9112, RFC 9110).

   Each program runs in its own run of the simulator, with a new server on
   [loopback]: a property draws the program with [Stateful.program], and
   [Stateful.execute] runs it inside the run. Thus each program gets a new
   world, and a shrink candidate runs again from no state. *)

open Windtrap
open Weft
open Caravan
open Server_support

(* Request kinds. Chunked bodies are HTTP/1.1 only. /ignore also gets a body
   larger than the server's buffer, so that some of it is still unread in the
   socket when the server drains it or closes over it. *)
let large = String.make 40_000 'x'

let kinds =
  let open List in
  let bodies v10 path =
    [No_body; Length "data"]
    @ (if v10 then [] else [Chunked "data!"])
    @ if path = "/ignore" then [Length large] else []
  in
  concat_map
    (fun v10 ->
      concat_map
        (fun path ->
          concat_map
            (fun conn ->
              let methods =
                [("GET", No_body); ("HEAD", No_body)]
                @ map
                    (fun b -> ("POST", b))
                    (filter (fun b -> b <> No_body) (bodies v10 path))
              in
              map (fun (meth, body) -> {meth; path; v10; conn; body}) methods )
            [None; Some "close"; Some "keep-alive"] )
        ["/hello"; "/echo"; "/empty"; "/close"; "/stream"; "/ignore"] )
    [false; true]

(* [pp_kind] prints a kind in one line. A long body prints as its length. *)
let pp_kind ppf k =
  Format.fprintf ppf "%s %s %s%s%s" k.meth k.path
    (if k.v10 then "1.0" else "1.1")
    (match k.conn with None -> "" | Some c -> " conn:" ^ c)
    ( match k.body with
    | No_body ->
        ""
    | Length s when String.length s > 16 ->
        Printf.sprintf " len:%d" (String.length s)
    | Length s ->
        Printf.sprintf " len:%S" s
    | Chunked s ->
        Printf.sprintf " chunked:%S" s )

(* What the server must answer to [k], and whether it then closes. *)
let expected k =
  let head = k.meth = "HEAD" in
  let closes_by_request =
    if k.v10 then k.conn <> Some "keep-alive" else k.conn = Some "close"
  in
  let body_text =
    match k.body with No_body -> "" | Length s | Chunked s -> s
  in
  let code, body, closes_by_response =
    match k.path with
    | "/hello" ->
        (200, "hello", false)
    | "/echo" ->
        (200, body_text, false)
    | "/empty" ->
        (204, "", false)
    | "/close" ->
        (200, "bye", true)
    | "/stream" ->
        (200, "abcd", k.v10 && not head) (* close-delimited for 1.0 *)
    | _ ->
        (200, "ignored", false)
  in
  let closes = closes_by_request || closes_by_response in
  (summary code (if head then "" else body) closes, closes)

(* Requests that make the server close over a body still unread in the socket
   are rare among the kinds, so they get a share of their own. *)
let kind_gen =
  let closes_over_unread k = k.body = Length large && snd (expected k) in
  Gen.frequency
    [ (4, Gen.of_list ~pp:pp_kind kinds)
    ; (1, Gen.of_list ~pp:pp_kind (List.filter closes_over_unread kinds)) ]

(* The model of a connection. *)
module Model = struct
  type t =
    { mutable owed: string list (* responses still to read, in order *)
    ; mutable closing: bool (* a request that closes the connection was sent *)
    ; mutable closed: bool (* the client closed it *) }

  let connect () = {owed= []; closing= false; closed= false}

  let send m k =
    cover "HEAD" (k.meth = "HEAD") ;
    cover "chunked body" (match k.body with Chunked _ -> true | _ -> false) ;
    cover "HTTP/1.0 kept alive" (k.v10 && k.conn = Some "keep-alive") ;
    let response, closes = expected k in
    let unread = k.path = "/ignore" && k.body <> No_body in
    cover "unread body drained" (unread && not closes) ;
    cover "unread body lingered over" (unread && closes) ;
    m.owed <- m.owed @ [response] ;
    if closes then m.closing <- true

  let receive m =
    cover "two or more responses pipelined" (List.length m.owed >= 2) ;
    cover "end of a closed connection" (m.owed = [] && m.closing) ;
    match m.owed with
    | r :: rest ->
        m.owed <- rest ;
        r
    | [] ->
        "EOF"

  let close m = m.closed <- true

  let can_send m _ = (not m.closing) && not m.closed

  let can_receive m = (not m.closed) && (m.owed <> [] || m.closing)

  let can_close m = not m.closed
end

(* A connection of the simulator, with the kinds sent but not yet answered, to
   parse HEAD responses. *)
module System = struct
  type t = {flow: Flow.t; reader: Buf_reader.t; sent: kind Queue.t}

  (* The server of the program that runs at this time. *)
  let server = ref None

  let connect () =
    let flow = Tcp.connect (Server.local_addr (Option.get !server)) in
    {flow; reader= Buf_reader.of_flow flow; sent= Queue.create ()}

  let send c k =
    Queue.push k c.sent ;
    Flow.write_string c.flow (serialize k)

  let receive c =
    match Queue.take_opt c.sent with
    | Some k ->
        read_response ~head:(k.meth = "HEAD") c.reader
    | None ->
        if Buf_reader.peek_eof c.reader then "EOF" else "unexpected bytes"

  let close c = Flow.close c.flow
end

(* The commands come from [Stateful] of windtrap, so that [Stateful.program] and
   [Stateful.execute] take them. The flat [command] of windtrap gives an
   abstract type that these two functions do not take. [Private] has no
   stability guarantee. The flake pins windtrap: after an update of windtrap, do
   a check of this test. *)
module Stateful = Windtrap.Private.Stateful

(* The calls of each command in the system, for the check "never called" of
   [stateful] of windtrap. The release of a connection calls [System.close] but
   not the system function of the command "close". *)
let names = ["connect"; "send"; "receive"; "close"]

let calls = List.map (fun name -> (name, Atomic.make 0)) names

let counted name f =
  let n = List.assoc name calls in
  fun x -> Atomic.incr n ; f x

let counted2 name f =
  let n = List.assoc name calls in
  fun x y -> Atomic.incr n ; f x y

let conn =
  Stateful.abstract "c"
    ~pp:(fun ppf (m : Model.t) ->
      Format.fprintf ppf "owed %d%s" (List.length m.owed)
        (if m.closing then ", closing" else "") )
    ~release:System.close

let commands =
  let open Stateful in
  [ command "connect"
      (Gen.unit @-> makes conn)
      Model.connect
      (counted "connect" System.connect)
  ; command "send" ~pre:Model.can_send
      (conn ^-> kind_gen @-> returns unit)
      Model.send
      (counted2 "send" System.send)
  ; command "receive" ~pre:Model.can_receive
      (conn ^-> returns string)
      Model.receive
      (counted "receive" System.receive)
  ; command "close" ~pre:Model.can_close
      (conn ^-> returns unit)
      Model.close
      (counted "close" System.close) ]

(* 300 programs by default. The mutation check runs this test again for each
   mutant, and each program opens many connections: it sets a lower count with
   CARAVAN_STATEFUL_COUNT. *)
let count =
  Option.value ~default:300
    (Option.bind (Sys.getenv_opt "CARAVAN_STATEFUL_COUNT") int_of_string_opt)

(* [execute p] runs the program [p] in a new run of the simulator, against a new
   server. At the end, the server aborts and [with_server] waits for it. In a
   program that passes, the virtual time does not move. The guard is longer than
   the timeouts of [Config.default] (30 s at most). Thus a program that waits
   for a timeout of the server fails in the model with its table, and not at the
   guard. *)
let execute p =
  Sim_support.run ~guard:(Span.s 600)
  @@ fun _ ->
  with_server
  @@ fun server ->
  System.server := Some server ;
  Fun.protect
    ~finally:(fun () -> System.server := None)
    (fun () -> Stateful.execute p)

(* A property over the programs. It keeps these parts of [stateful] of windtrap:
   the tags "prop" and "stateful", a failure that prints the record of the run
   of the program with [summary] as its one-line form, and the check "never
   called" of each command (with [calls]). [stateful] itself runs each program
   in the fiber of the test, thus it cannot give each program a new run. *)
let stateful_tests =
  group "HTTP/1.1 connections"
    [ test ~tags:["prop"; "stateful"] "behave like the model" (fun () ->
          Windtrap.Private.Run.property ~count ~summary:Stateful.summary
            ~prints_run:true
            (Stateful.program ~steps:25 commands)
            execute ;
          match List.filter (fun (_, n) -> Atomic.get n = 0) calls with
          | [] ->
              ()
          | never ->
              failf "never called: %s (over %d passing programs)"
                (String.concat ", " (List.map fst never))
                count ) ]

let () = exit (Windtrap.run "server-stateful" [stateful_tests])
