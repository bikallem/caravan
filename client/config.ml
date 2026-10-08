module Mtime = Affect_unix.Mtime

type t =
  { connect_timeout: Mtime.Span.t
  ; response_timeout: Mtime.Span.t
  ; read_timeout: Mtime.Span.t
  ; write_timeout: Mtime.Span.t
  ; idle_timeout: Mtime.Span.t
  ; max_idle_per_host: int
  ; max_status_line: int
  ; max_header_bytes: int
  ; max_header_count: int
  ; close_grace: Mtime.Span.t
  ; mono_time: Caravan.Time.mono }

let default =
  { connect_timeout= Mtime.Span.(10 * s)
  ; response_timeout= Mtime.Span.(30 * s)
  ; read_timeout= Mtime.Span.(30 * s)
  ; write_timeout= Mtime.Span.(30 * s)
  ; idle_timeout= Mtime.Span.(5 * s)
  ; max_idle_per_host= 8
  ; max_status_line= 8192
  ; max_header_bytes= 65536
  ; max_header_count= 200
  ; close_grace= Mtime.Span.(5 * s)
  ; mono_time= Caravan.Time.mono }

let limits c =
  { Caravan.Private.H1_parse.max_request_line= c.max_status_line
  ; max_header_bytes= c.max_header_bytes
  ; max_header_count= c.max_header_count }
