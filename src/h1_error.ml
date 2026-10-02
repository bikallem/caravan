type t =
| Bad_request of string
| Uri_too_long
| Header_fields_too_large
| Content_too_large
| Not_implemented of string
| Version_not_supported

exception E of t

let status = function
| Bad_request _ -> Status.bad_request
| Uri_too_long -> Status.uri_too_long
| Header_fields_too_large -> Status.request_header_fields_too_large
| Content_too_large -> Status.content_too_large
| Not_implemented _ -> Status.not_implemented
| Version_not_supported -> Status.http_version_not_supported

let pp ppf e =
  match e with
  | Bad_request detail | Not_implemented detail ->
      Format.fprintf ppf "%a (%s)" Status.pp (status e) detail
  | _ -> Status.pp ppf (status e)
