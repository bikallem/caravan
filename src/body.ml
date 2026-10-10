module Writer = struct
  type t =
    { write: bytes -> int -> int -> unit
    ; flush: unit -> unit
    ; mutable trailers: Headers.t
    ; mutable live: bool (* false once the producer that got it returned *) }

  let make ~write ~flush = {write; flush; trailers= Headers.empty; live= true}

  let check w name =
    if not w.live then
      invalid_arg
        ("Body.Writer." ^ name ^ ": the producer of the body has returned")

  let write w buf off len =
    check w "write" ;
    if off < 0 || len < 0 || off > Bytes.length buf - len then
      invalid_arg "Body.Writer.write: invalid range" ;
    if len > 0 then w.write buf off len

  let write_string w s = write w (Bytes.unsafe_of_string s) 0 (String.length s)

  let flush w = check w "flush" ; w.flush ()

  let set_trailers w h =
    check w "set_trailers" ;
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

type t =
  { kind: kind
  ; length: int option
  ; closed: bool Atomic.t (* also set once a producer has run *)
  ; mutable finished: bool
  ; mutable trailers: Headers.t }

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
  ; trailers= Headers.empty }

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

let read b buf off len =
  if off < 0 || len <= 0 || off > Bytes.length buf - len then
    invalid_arg "Body.read: invalid range" ;
  match b.kind with
  | Empty ->
      0
  | Producer _ ->
      invalid_arg "Body.read: a writer body can only be written out"
  | Source s ->
      if Atomic.get b.closed then invalid_arg "Body.read: closed body" ;
      if b.finished then 0
      else begin
        let n = s.read buf off len in
        if n = 0 then begin
          b.finished <- true ;
          b.trailers <- s.source_trailers ()
        end ;
        n
      end

let chunk_size = 16384

let write_to b w =
  match b.kind with
  | Empty ->
      ()
  | Source _ ->
      (* No larger than the body, if its length is known. *)
      let size =
        match b.length with
        | Some n ->
            Int.max 1 (Int.min n chunk_size)
        | None ->
            chunk_size
      in
      let buf = Bytes.create size in
      let rec copy () =
        match read b buf 0 size with
        | 0 ->
            ()
        | n ->
            Writer.write w buf 0 n ; Writer.flush w ; copy ()
      in
      copy () ;
      if not (Headers.is_empty b.trailers) then Writer.set_trailers w b.trailers
  | Producer produce ->
      Writer.check w "write_to" ;
      if Atomic.exchange b.closed true then
        invalid_arg "Body.write_to: body already written out or closed" ;
      (* The producer gets its own writer, which stops when it returns: a write
         after that must not reach [w], whose sink can serve a later message of
         the connection. It writes through the checks of [w], so that it also
         stops when [w] stops (a nested producer). It starts with the trailers
         of [w]. *)
      let own =
        { (Writer.make
             ~write:(fun buf off len -> Writer.write w buf off len)
             ~flush:(fun () -> Writer.flush w) )
          with
          trailers= w.trailers }
      in
      ( match produce own with
      | () ->
          own.live <- false
      | exception e ->
          own.live <- false ;
          raise e ) ;
      w.trailers <- own.trailers ;
      b.finished <- true ;
      b.trailers <- own.trailers

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
  | Empty ->
      ()
  | Source s ->
      if not (Atomic.exchange b.closed true) then s.source_close ()
  | Producer _ ->
      Atomic.set b.closed true
