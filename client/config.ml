open Weft

type t =
  { connect_timeout: Span.t
  ; response_timeout: Span.t
  ; read_timeout: Span.t
  ; write_timeout: Span.t
  ; idle_timeout: Span.t
  ; max_idle_per_host: int
  ; max_status_line: int
  ; max_header_bytes: int
  ; max_header_count: int
  ; close_grace: Span.t }

let default =
  { connect_timeout= Span.s 10
  ; response_timeout= Span.s 30
  ; read_timeout= Span.s 30
  ; write_timeout= Span.s 30
  ; idle_timeout= Span.s 5
  ; max_idle_per_host= 8
  ; max_status_line= 8192
  ; max_header_bytes= 65536
  ; max_header_count= 200
  ; close_grace= Span.s 5 }

let limits c =
  { Caravan.Private.H1_parse.max_request_line= c.max_status_line
  ; max_header_bytes= c.max_header_bytes
  ; max_header_count= c.max_header_count }
