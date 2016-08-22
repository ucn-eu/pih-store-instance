open Mirage

let persist_host =
  Key.create "persist-host" @@ Key.Arg.required Key.Arg.string (Key.Arg.info ["persist-host"])

let persist_port =
  Key.create "persist-port" @@ Key.Arg.required Key.Arg.int (Key.Arg.info ["persist-port"])

(* in seconds *)
let persist_period =
  Key.create "persist-period" @@ Key.Arg.required Key.Arg.int (Key.Arg.info ["persist-period"])

let keys = Key.[
  abstract persist_host;
  abstract persist_port;
  abstract persist_period; ]

let ip_config =
  let addr = Ipaddr.V4.of_string_exn in
  { address  = addr "192.168.252.10";
    netmask  = addr "255.255.255.0";
    gateways = [addr "192.168.252.2"]; }

let stack =
  if_impl Key.is_xen
    (direct_stackv4_with_static_ipv4 (netif "0") ip_config)
    (socket_stackv4 [Ipaddr.V4.any])

let https = http_server @@ conduit_direct ~tls:true stack

let resolver_impl =
  if_impl Key.is_xen (resolver_dns stack) resolver_unix_system

let conduit_tls = conduit_direct ~tls:true stack

let tls = crunch "tls"
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
  register ~libraries ~keys "wifi" [
    main $ https $ resolver_impl $ conduit_tls $ fs $ tls $ default_clock
  ]
