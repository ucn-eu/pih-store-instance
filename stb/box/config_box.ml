open Mirage

let persist_host =
  Key.create "persist-host" @@ Key.Arg.required Key.Arg.ipv4 (Key.Arg.info ["persist-host"])

let persist_port =
  Key.create "persist-port" @@ Key.Arg.required Key.Arg.int (Key.Arg.info ["persist-port"])

let keys = Key.[
  abstract persist_host;
  abstract persist_port; ]

(*let stack =
  if_impl Key.is_xen
    (direct_stackv4_with_default_ipv4 (netif "0"))
    (socket_stackv4 [Ipaddr.V4.any])

let https = http_server @@ conduit_direct ~tls:true stack*)

let stack = generic_stackv4 (netif "0")

let resolver_impl =
  if_impl Key.is_xen (resolver_dns stack) resolver_unix_system

let conduit_tls = conduit_direct stack

let tls = crunch "tls"

let main =
  foreign "Box.Main"
    (stackv4 @-> resolver @-> conduit @-> kv_ro @-> pclock @-> job)

let () =
  let libraries = [
      "logs";
      "mirage-logs";
      "pih-store";
      "multipart-form-data";
    ] in
  register ~libraries ~keys "STB.Box" [
    main $ stack $ resolver_impl $ conduit_tls $ tls $ default_posix_clock
  ]
