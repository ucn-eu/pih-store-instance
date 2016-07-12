open Mirage

let stack =
  let open Key in
  if_impl is_xen
    (direct_stackv4_with_dhcp tap0)
    (socket_stackv4 [Ipaddr.V4.any])

let keys = generic_kv_ro "tls"
let conduit_tls = conduit_direct ~tls:true stack
let resolver_dns = resolver_dns stack

let https = http_server @@ conduit_tls

let main =
  foreign "Unikernel.Main"
    (http @-> kv_ro @-> resolver @-> conduit @-> clock @-> job)

let tracing = mprof_trace ~size:1000000 ()

let () =
  let libraries = [
      "logs";
      "mirage-logs";
      "pih-store";
      "ezjsonm"
    ] in
  register ~libraries "presence" [
    main $ https $ keys $ resolver_dns $ conduit_tls $ default_clock
  ]
