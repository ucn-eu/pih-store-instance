open Mirage

let addr = Ipaddr.V4.of_string_exn

let persist_host =
  let default = addr "10.0.0.1" in
  Key.create "persist-host" @@ Key.Arg.opt Key.Arg.ipv4 default (Key.Arg.info ["persist-host"])

let persist_port =
  let default = 10002 in
  Key.create "persist-port" @@ Key.Arg.opt Key.Arg.int default (Key.Arg.info ["persist-port"])

let keys = Key.[
  abstract persist_host;
  abstract persist_port; ]

let stack =
  if_impl Key.is_xen
    (direct_stackv4_with_default_ipv4 (netif "0"))
    (socket_stackv4 [Ipaddr.V4.any])

let https = http_server @@ conduit_direct ~tls:true stack

let resolver_impl =
  if_impl Key.is_xen (resolver_dns stack) resolver_unix_system

let conduit_tls = conduit_direct ~tls:true stack

let tls = crunch "tls"

let main =
  foreign "Unikernel.Main"
    (http @-> resolver @-> conduit @-> kv_ro @-> pclock @-> job)

let () =
  let libraries = [
      "logs";
      "mirage-logs";
      "pih-store"
    ] in
  register ~libraries ~keys "review" [
    main $ https $ resolver_impl $ conduit_tls $ tls $ default_posix_clock
  ]
