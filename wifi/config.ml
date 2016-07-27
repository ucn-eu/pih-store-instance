open Mirage

let stack =
  if_impl Key.is_xen
    (direct_stackv4_with_default_ipv4 (netif "0"))
    (socket_stackv4 [Ipaddr.V4.any])

let https = http_server @@ conduit_direct ~tls:true stack

let resolver_impl =
  if_impl Key.is_xen (resolver_dns stack) resolver_unix_system

let conduit_tls = conduit_direct ~tls:true stack

let keys = crunch "tls"
let fs = crunch "files"

let main =
  foreign "Unikernel.Main"
    (http @-> resolver @-> conduit @-> kv_ro @-> kv_ro @-> clock @-> job)

let tracing = mprof_trace ~size:1000000 ()

let () =
  let libraries = [
      "logs";
      "mirage-logs";
      "pih-store"
    ] in
  register ~libraries "wifi" [
    main $ https $ resolver_impl $ conduit_tls $ fs $ keys $ default_clock
  ]
