module Writer = struct
  type t =
    { write: bytes -> int -> int -> unit
    ; flush: unit -> unit
    ; mutable trailers: Headers.t
    ; mutable live: bool (* false once the producer that got it returned *) }

  let make ~write ~flush = {write; flush; trailers= Headers.empty; live= true}

  let check w name =
    if not w.live then
      invalid_arg (name ^ ": the producer of the body has returned")

  let write w buf off len =
    check w "Body.Writer.write" ;
    if off < 0 || len < 0 || off > Bytes.length buf - len then
      invalid_arg "Body.Writer.write: invalid range" ;
    if len > 0 then w.write buf off len

  let write_string w s = write w (Bytes.unsafe_of_string s) 0 (String.length s)

  let flush w =
    check w "Body.Writer.flush" ;
    w.flush ()

  let set_trailers w h =
    check w "Body.Writer.set_trailers" ;
    w.trailers <- h

  let trailers w = w.trailers
end

exception Too_large

exception Truncated

type source =
  { read: bytes -> int -> int -> int
  ; source_trailers: unit -> Headers.t
  ; source_close: unit -> unit }

type kind = Empty | Source of source | Producer of (Writer.t -> unit)

type outcome = Ended | Failed of exn | Abandoned

(* [once] is set when [run] starts. [bytes] counts the bytes that reached the
   consumer since [on_end]. *)
type hook =
  | No_hook
  | Hook of
      { once: bool Atomic.t
      ; mutable bytes: int
      ; run: outcome -> bytes:int -> unit }

type t =
  { kind: kind
  ; length: int option
  ; closed: bool Atomic.t (* also set once a producer has run *)
  ; mutable finished: bool
  ; mutable trailers: Headers.t
  ; mutable hook: hook }

let make ?length kind =
  ( match length with
  | Some n when n < 0 ->
      invalid_arg "Body: negative length"
  | _ ->
      () ) ;
  { kind
  ; length
  ; closed= Atomic.make false
  ; finished= false
  ; trailers= Headers.empty
  ; hook= No_hook }

let empty = make ~length:0 Empty

let of_source ?length ?(trailers = fun () -> Headers.empty) ?(close = ignore)
    read =
  make ?length (Source {read; source_trailers= trailers; source_close= close})

let of_string s =
  let pos = ref 0 in
  let read buf off len =
    let n = Int.min len (String.length s - !pos) in
    Bytes.blit_string s !pos buf off n ;
    pos := !pos + n ;
    n
  in
  of_source ~length:(String.length s) read

let of_writer ?length produce = make ?length (Producer produce)

let length b = b.length

let trailers b = b.trailers

(* The hook *)

let count b n =
  match b.hook with No_hook -> () | Hook h -> h.bytes <- h.bytes + n

(* [fire b how] runs the hook of [b] with [how], if it did not run. An exception
   of the hook goes to the caller. *)
let fire b how =
  match b.hook with
  | No_hook ->
      ()
  | Hook h ->
      if not (Atomic.exchange h.once true) then h.run how ~bytes:h.bytes

(* [failed b e bt] runs the hook of [b] for [Failed e], if it did not run, then
   raises [e] with [bt]. An exception of the hook is dropped. *)
let failed b e bt =
  (try fire b (Failed e) with _ -> ()) ;
  Printexc.raise_with_backtrace e bt

(* [both first second] runs the two hooks. On [Ended] and [Abandoned], the first
   exception goes on after the two ran. *)
let both first second how ~bytes =
  match first how ~bytes with
  | () ->
      second how ~bytes
  | exception e ->
      let bt = Printexc.get_raw_backtrace () in
      (try second how ~bytes with _ -> ()) ;
      Printexc.raise_with_backtrace e bt

let on_end f b =
  (* [empty] is shared: its hook goes on a new empty body. *)
  let b = if b == empty then make ~length:0 Empty else b in
  let run =
    match b.hook with
    | No_hook ->
        f
    | Hook old ->
        let inner how ~bytes =
          if not (Atomic.exchange old.once true) then
            old.run how ~bytes:(old.bytes + bytes)
        in
        both inner f
  in
  b.hook <- Hook {once= Atomic.make false; bytes= 0; run} ;
  b

(* Consumption *)

(* [source_read b s] reads the source [s] of [b], with no hook. *)
let source_read b s buf off len =
  if b.finished then 0
  else
    let n = s.read buf off len in
    if n = 0 then (
      b.finished <- true ;
      b.trailers <- s.source_trailers () ) ;
    n

let read b buf off len =
  if off < 0 || len <= 0 || off > Bytes.length buf - len then
    invalid_arg "Body.read: invalid range" ;
  match b.kind with
  | Empty ->
      fire b Ended ; 0
  | Producer _ ->
      invalid_arg "Body.read: a writer body can only be written out"
  | Source s -> (
      if Atomic.get b.closed then invalid_arg "Body.read: closed body" ;
      match b.hook with
      | No_hook ->
          source_read b s buf off len
      | Hook _ -> (
        match source_read b s buf off len with
        | 0 ->
            fire b Ended ; 0
        | n ->
            count b n ; n
        | exception e ->
            failed b e (Printexc.get_raw_backtrace ()) ) )

let chunk_size = 16384

(* [copy b w] writes the rest of [b] into [w], and counts the writes that
   returned. *)
let copy b w =
  match b.kind with
  | Empty ->
      ()
  | Source s ->
      (* No larger than the body, if its length is known. *)
      let size =
        match b.length with
        | Some n ->
            Int.max 1 (Int.min n chunk_size)
        | None ->
            chunk_size
      in
      let buf = Bytes.create size in
      let rec loop () =
        if Atomic.get b.closed then invalid_arg "Body.read: closed body" ;
        match source_read b s buf 0 size with
        | 0 ->
            ()
        | n ->
            Writer.write w buf 0 n ; count b n ; Writer.flush w ; loop ()
      in
      loop () ;
      if not (Headers.is_empty b.trailers) then Writer.set_trailers w b.trailers
  | Producer produce ->
      (* The producer gets its own writer, which stops when it returns: a write
         after that must not reach [w], whose sink can serve a later message of
         the connection. It writes through the checks of [w], so that it also
         stops when [w] stops (a nested producer). *)
      let write =
        match b.hook with
        | No_hook ->
            fun buf off len -> Writer.write w buf off len
        | Hook _ ->
            fun buf off len -> Writer.write w buf off len ; count b len
      in
      let own = Writer.make ~write ~flush:(fun () -> Writer.flush w) in
      ( match produce own with
      | () ->
          own.live <- false
      | exception e ->
          own.live <- false ;
          raise e ) ;
      (* As for a source: trailers replace those of [w] only if there are
         some. *)
      if not (Headers.is_empty own.trailers) then
        Writer.set_trailers w own.trailers ;
      b.finished <- true ;
      b.trailers <- own.trailers

let write_to b w =
  Writer.check w "Body.write_to" ;
  ( match b.kind with
  | Producer _ ->
      if Atomic.exchange b.closed true then
        invalid_arg "Body.write_to: body already written out or closed"
  | Empty | Source _ ->
      (* The copy loop refuses a closed source. *)
      () ) ;
  match b.hook with
  | No_hook ->
      copy b w
  | Hook _ -> (
    match copy b w with
    | () ->
        fire b Ended
    (* Another fiber closed the source during the copy: its close runs the hook
       with [Abandoned]. *)
    | exception (Invalid_argument _ as e)
      when Atomic.get b.closed
           && match b.kind with Source _ -> true | Empty | Producer _ -> false
      ->
        raise e
    | exception e ->
        failed b e (Printexc.get_raw_backtrace ()) )

let to_string ?(max = max_int) b =
  let out = Buffer.create 256 in
  let write buf off len =
    if Buffer.length out + len > max then raise Too_large ;
    Buffer.add_subbytes out buf off len
  in
  write_to b (Writer.make ~write ~flush:ignore) ;
  Buffer.contents out

let close b =
  match b.kind with
  | Source s when not (Atomic.exchange b.closed true) -> (
    match s.source_close () with
    | () ->
        fire b Abandoned
    | exception e ->
        let bt = Printexc.get_raw_backtrace () in
        (try fire b Abandoned with _ -> ()) ;
        Printexc.raise_with_backtrace e bt )
  | Source _ | Empty ->
      fire b Abandoned
  | Producer _ ->
      Atomic.set b.closed true ; fire b Abandoned
