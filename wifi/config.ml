open Mirage

let stack =
  match get_mode () with
  | `Xen -> generic_stackv4 (netif "0")
  | _ -> socket_stackv4  [Ipaddr.V4.any]

let keys = crunch "tls"
let fs = crunch "files"

let https = http_server @@ conduit_direct ~tls:true stack

let main =
  foreign "Unikernel.Main"
    (http @-> kv_ro @-> kv_ro @-> clock @-> job)

let tracing = mprof_trace ~size:1000000 ()

let () =
  let libraries = [
      "logs";
      "mirage-logs";
      "pih-store"
    ] in
  register ~libraries "wifi" [
    main $ https $ fs $ keys $ default_clock
  ]
