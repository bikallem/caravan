(* A fake HTTP/2 client for the tests of the engine. See fake_h2_client.mli. *)

open Weft
open Caravan_http2.Private
module Flow = Caravan.Flow
module Reader = Weft_flow.Reader
module Writer = Weft_flow.Writer
module Mutex = Fiber.Mutex
module Condition = Fiber.Condition

let failf fmt = Printf.ksprintf (fun m -> Sim_support.Failed m) fmt

(* The data of the PING of [connect]. The frames of stream 0 before its ACK are
   the start frames of the server. *)
let start_ping = "start000"

(* The state of a stream that the client opened. [given] is the credit that the
   client gave to the server, and [got] is the credit that the server gave to
   the client. [client_ended]: the server got the END_STREAM flag of the client.
   It is true at once for the HEADERS frame that opens the stream. For a later
   frame, it is true at the ACK of the PING after it ([end_barrier]): before, a
   RST_STREAM NO_ERROR of the server can cross the END_STREAM legally. *)
type stream =
  { mutable given: int
  ; mutable got: int
  ; mutable ended: bool (* the END_STREAM flag of the server came *)
  ; mutable client_ended: bool
  ; mutable end_barrier: string option
  ; mutable server_reset: bool (* a RST_STREAM frame of the server came *)
  ; mutable barrier: string option (* the data of the PING after a reset *)
  ; mutable past_barrier: bool (* the PING ACK of the barrier came *) }

(* A frame in the mailbox. [fields] is the decoded block for the frame that ends
   a header block. *)
type entry = {frame: Frame.t; fields: (string * string) list option}

(* The fields after [m] are under [m]. The reader fiber alone uses [reader],
   [decoder] and [block]. The writes go under [wm]. *)
type t =
  { reader: Reader.t
  ; writer: Writer.t
  ; flow: Flow.t
  ; wm: Mutex.t
  ; decoder: Hpack.Decoder.t
  ; mutable block: (int * bool * Buffer.t) option
        (* the open header block: its stream, its END_STREAM flag, its
           fragments *)
  ; max_frame: int
  ; m: Mutex.t
  ; changed: Condition.t
  ; mailbox: entry Queue.t
  ; streams: (int, stream) Hashtbl.t
  ; mutable given_conn: int
  ; mutable given_initial: int (* SETTINGS_INITIAL_WINDOW_SIZE of the client *)
  ; pending_acks: int option Queue.t
        (* for each SETTINGS frame of the client that waits for its ACK: a
           smaller SETTINGS_INITIAL_WINDOW_SIZE, if any *)
  ; mutable got_conn: int
  ; mutable got_initial: int
  ; mutable server_settings: Frame.setting list option
  ; mutable starting: bool (* the ACK of the PING of [connect] did not come *)
  ; mutable pings: int
  ; mutable paused: bool
  ; mutable eof: bool
  ; mutable failure: exn option
  ; mutable reader_fiber: unit Promise.t option }

let locked c f = Mutex.use_rw ~protect:false c.m f

let write c f =
  Mutex.use_rw ~protect:false c.wm (fun () ->
      f c.writer ; Writer.flush c.writer )

let setting id l = List.assoc_opt id l

(* {1 The checks} *)

let stream_of c id = Hashtbl.find_opt c.streams id

(* The checks of a frame on a stream other than 0, with the state of the stream.
   The result is the text of a failure, or [None]. *)
let check_stream c id (f : Frame.t) =
  match stream_of c id with
  | None ->
      Some
        (Printf.sprintf "a frame on stream %d, which the client did not open" id)
  | Some s -> (
      if s.server_reset then
        Some
          (Printf.sprintf "a frame on stream %d after its RST_STREAM frame" id)
      else if s.past_barrier then
        Some
          (Printf.sprintf
             "a frame on stream %d after the barrier of its reset: %s" id
             (H2_support.show f) )
      else
        match f with
        | Rst_stream _ when s.ended && s.client_ended ->
            Some
              (Printf.sprintf
                 "a RST_STREAM frame on stream %d, which ended in the two \
                  directions"
                 id )
        | Rst_stream _ | Window_update _ | Priority _ ->
            None
        | _ when s.ended ->
            Some (Printf.sprintf "a frame on stream %d after END_STREAM" id)
        | Data {flow_length; _} ->
            if flow_length > c.given_conn then
              Some
                (Printf.sprintf
                   "DATA of %d bytes on stream %d over the connection window \
                    of %d"
                   flow_length id c.given_conn )
            else if flow_length > s.given then
              Some
                (Printf.sprintf
                   "DATA of %d bytes on stream %d over the stream window of %d"
                   flow_length id s.given )
            else None
        | _ ->
            None )

(* The effects of a frame that passed the checks, on the state. *)
let apply c (f : Frame.t) =
  match f with
  | Data {stream; end_stream; flow_length; _} ->
      let s = Hashtbl.find c.streams stream in
      c.given_conn <- c.given_conn - flow_length ;
      s.given <- s.given - flow_length ;
      if end_stream then s.ended <- true
  | Rst_stream {stream; _} ->
      (Hashtbl.find c.streams stream).server_reset <- true
  | Window_update {stream= 0; increment} ->
      c.got_conn <- c.got_conn + increment
  | Window_update {stream; increment} ->
      let s = Hashtbl.find c.streams stream in
      s.got <- s.got + increment
  | _ ->
      ()

(* The END_STREAM flag of a header block applies at its END_HEADERS. *)
let end_block c stream end_stream =
  if end_stream then (Hashtbl.find c.streams stream).ended <- true

let barrier_of c data =
  Hashtbl.fold
    (fun _ s found ->
      match s.barrier with Some d when d = data -> Some s | _ -> found )
    c.streams None

let end_barrier_of c data =
  Hashtbl.fold
    (fun _ s found ->
      match s.end_barrier with Some d when d = data -> Some s | _ -> found )
    c.streams None

(* The settings of the server: SETTINGS_INITIAL_WINDOW_SIZE changes the credit
   of each stream. *)
let apply_server_settings c l =
  match setting 0x4 l with
  | Some v ->
      let delta = v - c.got_initial in
      c.got_initial <- v ;
      Hashtbl.iter (fun _ s -> s.got <- s.got + delta) c.streams
  | None ->
      ()

let is_settings = function Frame.Settings {ack= false; _} -> true | _ -> false

(* What the reader does with a frame, under [m]: [`Keep] puts it in the mailbox,
   [`Drop] does not, [`Ack] answers a SETTINGS frame. *)
let on_frame c (f : Frame.t) =
  match f with
  | _ when c.server_settings = None && not (is_settings f) ->
      Error
        ("the first frame of the server is not SETTINGS: " ^ H2_support.show f)
  | Settings {ack= false; settings} ->
      apply_server_settings c settings ;
      if c.server_settings = None then c.server_settings <- Some settings ;
      Ok `Ack
  | Settings {ack= true; _} ->
      ( match Queue.take_opt c.pending_acks with
      | Some (Some v) ->
          let delta = v - c.given_initial in
          c.given_initial <- v ;
          Hashtbl.iter (fun _ s -> s.given <- s.given + delta) c.streams
      | Some None | None ->
          () ) ;
      Ok `Drop
  | Window_update {stream= 0; _} when c.starting ->
      apply c f ; Ok `Drop
  | Ping {ack= true; data} when data = start_ping ->
      c.starting <- false ;
      Ok `Drop
  | Ping {ack= true; data} -> (
    match (barrier_of c data, end_barrier_of c data) with
    | Some s, _ ->
        s.past_barrier <- true ;
        s.barrier <- None ;
        Ok `Drop
    | None, Some s ->
        s.client_ended <- true ;
        s.end_barrier <- None ;
        Ok `Drop
    | None, None ->
        Ok `Keep )
  | Window_update {stream= 0; _} ->
      apply c f ; Ok `Keep
  | Ping {ack= false; _} | Goaway _ ->
      Ok `Keep
  | Push_promise _ ->
      Error "the server sent PUSH_PROMISE"
  | Unknown {kind; _} ->
      Error
        (Printf.sprintf "the server sent a frame of the unknown type 0x%x" kind)
  | Malformed {reason; _} ->
      Error ("the server sent a malformed frame: " ^ reason)
  | Data {stream; _}
  | Headers {stream; _}
  | Priority {stream; _}
  | Rst_stream {stream; _}
  | Window_update {stream; _}
  | Continuation {stream; _} -> (
    match check_stream c stream f with
    | Some m ->
        Error m
    | None ->
        apply c f ; Ok `Keep )

(* The contiguity of header blocks, and their decode. It runs in the reader
   fiber, before [on_frame]. The result is the decoded fields of the frame that
   ends a block, or the text of a failure. *)
let block_step c (f : Frame.t) =
  let finish stream end_stream buf =
    c.block <- None ;
    match
      Hpack.Decoder.decode c.decoder ~max_list_size:max_int ~max_fields:max_int
        (Buffer.contents buf)
    with
    | `Fields l ->
        Ok (Some (stream, end_stream, l))
    | `Too_large ->
        Error "a header block over the limits of the client"
    | exception Error.Hpack m ->
        Error ("a header block that does not decode: " ^ m)
  in
  match (c.block, f) with
  | Some (s, end_stream, buf), Continuation {stream; end_headers; block}
    when stream = s ->
      Buffer.add_string buf block ;
      if end_headers then finish stream end_stream buf else Ok None
  | Some (s, _, _), _ ->
      Error
        (Printf.sprintf "a frame in the header block of stream %d: %s" s
           (H2_support.show f) )
  | None, Continuation {stream; _} ->
      Error
        (Printf.sprintf "a CONTINUATION frame on stream %d with no open block"
           stream )
  | None, Headers {stream; end_stream; end_headers; block; _} ->
      let buf = Buffer.create (String.length block) in
      Buffer.add_string buf block ;
      if end_headers then finish stream end_stream buf
      else begin
        c.block <- Some (stream, end_stream, buf) ;
        Ok None
      end
  | None, _ ->
      Ok None

(* The failure of a check: the state keeps it, and each waiter wakes. *)
let fail c e =
  locked c (fun () ->
      if Option.is_none c.failure then c.failure <- Some e ;
      Condition.broadcast c.changed ) ;
  raise e

(* The reader fiber. *)
let rec read_loop c =
  locked c (fun () ->
      while c.paused do
        Condition.wait c.changed c.m
      done ) ;
  match Frame.read c.reader ~max_size:c.max_frame with
  | exception (End_of_file | Flow.Closed) ->
      locked c (fun () ->
          c.eof <- true ;
          Condition.broadcast c.changed )
  | exception Frame.Connection_error {reason; _} ->
      fail c (failf "the server sent a bad frame: %s" reason)
  | f -> (
      (* A frame on a stream checks the stream first: a frame in a header block
         on a stream that the client did not open fails as such. *)
      let block = block_step c f in
      let result =
        locked c (fun () ->
            match on_frame c f with
            | Error m ->
                Error m
            | Ok action -> (
              match block with
              | Error m ->
                  Error m
              | Ok ended ->
                  Option.iter (fun (s, e, _) -> end_block c s e) ended ;
                  let fields = Option.map (fun (_, _, l) -> l) ended in
                  if action = `Keep then Queue.push {frame= f; fields} c.mailbox ;
                  Condition.broadcast c.changed ;
                  Ok action ) )
      in
      match result with
      | Error m ->
          fail c (Sim_support.Failed m)
      | Ok `Ack ->
          write c (fun w -> Frame.write w (Settings {ack= true; settings= []})) ;
          read_loop c
      | Ok (`Keep | `Drop) ->
          read_loop c )

(* Waits under [m] until [ready ()] gives [Some (Ok v)], and gives [v]. [Some
   (Error e)] raises [e]. A failure of a check, or the end of the stream of the
   server, raises. [ready] must not raise: it runs under [m]. *)
let wait_for c what ready =
  let result =
    locked c (fun () ->
        let rec loop () =
          match ready () with
          | Some r ->
              r
          | None -> (
            match c.failure with
            | Some e ->
                Error e
            | None ->
                if c.eof then
                  Error
                    (failf "the server closed the connection before %s" what)
                else begin
                  Condition.wait c.changed c.m ;
                  loop ()
                end )
        in
        loop () )
  in
  match result with Ok v -> v | Error e -> raise e

(* {1 Sends} *)

let new_stream c ~client_ended =
  { given= c.given_initial
  ; got= c.got_initial
  ; ended= false
  ; client_ended
  ; end_barrier= None
  ; server_reset= false
  ; barrier= None
  ; past_barrier= false }

(* Under [m]: the PING that follows the END_STREAM flag of the client on an open
   stream, in the same write. *)
let end_ping c stream =
  match stream_of c stream with
  | Some s ->
      c.pings <- c.pings + 1 ;
      let data = Printf.sprintf "ends%04d" c.pings in
      s.end_barrier <- Some data ;
      [Frame.Ping {ack= false; data}]
  | None ->
      []

(* The effects of a frame of the client on the state of the checks. The result
   is the frames to write after it. *)
let on_send c (f : Frame.t) =
  locked c (fun () ->
      match f with
      | Headers {stream; end_stream; _} when not (Hashtbl.mem c.streams stream)
        ->
          Hashtbl.replace c.streams stream
            (new_stream c ~client_ended:end_stream) ;
          []
      | Headers {stream; end_stream= true; _}
      | Data {stream; end_stream= true; _} ->
          end_ping c stream
      | Window_update {stream= 0; increment} ->
          c.given_conn <- c.given_conn + increment ;
          []
      | Window_update {stream; increment} ->
          Option.iter
            (fun s -> s.given <- s.given + increment)
            (stream_of c stream) ;
          []
      | Settings {ack= false; settings} ->
          ( match setting 0x4 settings with
          | Some v when v >= c.given_initial ->
              let delta = v - c.given_initial in
              c.given_initial <- v ;
              Hashtbl.iter (fun _ s -> s.given <- s.given + delta) c.streams ;
              Queue.push None c.pending_acks
          | Some v ->
              Queue.push (Some v) c.pending_acks
          | None ->
              Queue.push None c.pending_acks ) ;
          []
      | _ ->
          [] )

(* The frames [l], each with the frames that [on_send] gives after it. *)
let with_barriers c l = List.concat_map (fun f -> f :: on_send c f) l

let send c f =
  let l = with_barriers c [f] in
  write c (fun w -> List.iter (Frame.write w) l)

let send_all c l =
  let l = with_barriers c l in
  write c (fun w -> List.iter (Frame.write w) l)

let send_raw c s = write c (fun w -> Writer.write_string w s)

let request ?(end_stream = true) c ~stream fields =
  let buf = Buffer.create 256 in
  Hpack.encode buf fields ;
  let after =
    on_send c
      (Headers {stream; end_stream; end_headers= true; priority= None; block= ""}
      )
  in
  write c (fun w ->
      Frame.write_header_block w ~stream ~end_stream ~max_size:16384
        (Buffer.contents buf) ;
      List.iter (Frame.write w) after )

let credit c ~stream n =
  send c (Window_update {stream; increment= n}) ;
  send c (Window_update {stream= 0; increment= n})

let data ?(end_stream = false) c ~stream s =
  let len = String.length s in
  let rec loop off =
    let n =
      wait_for c "the credit for DATA" (fun () ->
          match stream_of c stream with
          | None ->
              Some (Error (failf "DATA on stream %d, which is not open" stream))
          | Some st when st.server_reset ->
              Some
                (Error (failf "the server reset stream %d during a send" stream))
          | Some st ->
              let room = min c.got_conn st.got in
              let n = min (min 16384 (len - off)) room in
              if n > 0 || len = 0 then begin
                c.got_conn <- c.got_conn - n ;
                st.got <- st.got - n ;
                Some (Ok n)
              end
              else None )
    in
    let last = off + n = len in
    let after =
      if end_stream && last then locked c (fun () -> end_ping c stream) else []
    in
    write c (fun w ->
        Frame.write_data w ~stream ~end_stream:(end_stream && last)
          (Bytes.unsafe_of_string s) off n ;
        List.iter (Frame.write w) after ) ;
    if not last then loop (off + n)
  in
  loop 0

let reset ?(also = []) c ~stream code =
  let data =
    locked c (fun () ->
        c.pings <- c.pings + 1 ;
        let data = Printf.sprintf "barr%04d" c.pings in
        Option.iter (fun s -> s.barrier <- Some data) (stream_of c stream) ;
        data )
  in
  let also = with_barriers c also in
  write c (fun w ->
      Frame.write w (Rst_stream {stream; code}) ;
      List.iter (Frame.write w) also ;
      Frame.write w (Ping {ack= false; data}) )

let pause_reading c = locked c (fun () -> c.paused <- true)

let resume_reading c =
  locked c (fun () ->
      c.paused <- false ;
      Condition.broadcast c.changed )

let close c = Flow.shutdown_send c.flow

(* {1 Receives} *)

let next c =
  (wait_for c "the next frame" (fun () ->
       Option.map Result.ok (Queue.take_opt c.mailbox) ) )
    .frame

let frames_until c p =
  let rec loop acc =
    let f = next c in
    if p f then List.rev (f :: acc) else loop (f :: acc)
  in
  loop []

let take_all c =
  locked c (fun () ->
      let l =
        List.of_seq (Seq.map (fun e -> e.frame) (Queue.to_seq c.mailbox))
      in
      Queue.clear c.mailbox ; l )

type response =
  { interim: (string * string) list list
  ; head: (string * string) list
  ; body: string
  ; trailers: (string * string) list
  ; reset: Frame.code option }

let is_interim fields =
  match List.assoc_opt ":status" fields with
  | Some s ->
      String.length s = 3 && s.[0] = '1'
  | None ->
      false

let response c ~stream =
  let interim = ref [] and head = ref None and body = Buffer.create 256 in
  let trailers = ref [] and reset = ref None in
  (* [take e] consumes a frame of the stream. The result is [true] at the end of
     the stream. The END_STREAM flag of a HEADERS frame applies at the end of
     its block. *)
  let block_end = ref false in
  let block fields end_stream =
    ( match fields with
    | Some l when !head = None && is_interim l ->
        interim := l :: !interim
    | Some l when !head = None ->
        head := Some l
    | Some l ->
        trailers := l
    | None ->
        () ) ;
    end_stream
  in
  let take e =
    match e.frame with
    | Data {data; end_stream; _} ->
        Buffer.add_string body data ;
        end_stream
    | Headers {end_stream; end_headers; _} ->
        if end_headers then block e.fields end_stream
        else begin
          block_end := end_stream ;
          false
        end
    | Continuation {end_headers; _} ->
        end_headers && block e.fields !block_end
    | Rst_stream {code; _} ->
        reset := Some code ;
        true
    | _ ->
        false
  in
  let mine (f : Frame.t) =
    match f with
    | Data {stream= s; _}
    | Headers {stream= s; _}
    | Continuation {stream= s; _}
    | Rst_stream {stream= s; _} ->
        s = stream
    | _ ->
        false
  in
  wait_for c (Printf.sprintf "the end of stream %d" stream) (fun () ->
      let keep = Queue.create () and finished = ref false in
      Queue.iter
        (fun e ->
          if (not !finished) && mine e.frame then finished := take e
          else Queue.push e keep )
        c.mailbox ;
      Queue.clear c.mailbox ;
      Queue.transfer keep c.mailbox ;
      if !finished then Some (Ok ()) else None ) ;
  { interim= List.rev !interim
  ; head= Option.value !head ~default:[]
  ; body= Buffer.contents body
  ; trailers= !trailers
  ; reset= !reset }

let server_settings c = Option.value c.server_settings ~default:[]

let finish c =
  Option.iter Promise.await c.reader_fiber ;
  match locked c (fun () -> c.failure) with Some e -> raise e | None -> ()

(* {1 Connect} *)

let connect ?(settings = []) s flow =
  let given_initial = Option.value (setting 0x4 settings) ~default:65535 in
  let c =
    { reader= Reader.create flow
    ; writer= Writer.create ~size:65536 flow
    ; flow
    ; wm= Mutex.create ()
    ; decoder= Hpack.Decoder.create ~max_table_size:4096
    ; block= None
    ; max_frame= Option.value (setting 0x5 settings) ~default:16384
    ; m= Mutex.create ()
    ; changed= Condition.create ()
    ; mailbox= Queue.create ()
    ; streams= Hashtbl.create 16
    ; given_conn= 65535
    ; given_initial
    ; pending_acks= Queue.create ()
    ; got_conn= 65535
    ; got_initial= 65535
    ; server_settings= None
    ; starting= true
    ; pings= 0
    ; paused= false
    ; eof= false
    ; failure= None
    ; reader_fiber= None }
  in
  Queue.push None c.pending_acks ;
  write c (fun w ->
      Writer.write_string w Frame.preface ;
      Frame.write w (Settings {ack= false; settings}) ;
      Frame.write w (Ping {ack= false; data= start_ping}) ) ;
  c.reader_fiber <-
    Some
      (Fiber.fork_promise ~name:"fake_h2_client.reader" s (fun () ->
           read_loop c ) ) ;
  match
    wait_for c "its start frames" (fun () ->
        if c.starting then None else Some (Ok ()) )
  with
  | () ->
      c
  | exception e ->
      (* The reader fiber failed or ended: await it, so that its failure is not
         left. *)
      finish c ; raise e
