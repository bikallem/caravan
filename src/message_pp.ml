let pp ppf start h m =
  Format.fprintf ppf "@[<v>%a" start () ;
  if not (Headers.is_empty h) then Format.fprintf ppf "@,%a" Headers.pp h ;
  if not (Attrs.is_empty m) then Format.fprintf ppf "@,attrs: %a" Attrs.pp m ;
  Format.fprintf ppf "@]"
