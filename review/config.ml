open Mirage

let stack =
  match get_mode () with
  | `Xen -> generic_stackv4 default_console (netif "0")
  | _ -> socket_stackv4 default_console [Ipaddr.V4.any]

let keys = crunch "tls"

let https = http_server @@ conduit_direct ~tls:true stack

let main =
  foreign "Unikernel.Main"
    (http @-> kv_ro @-> clock @-> job)

let tracing = mprof_trace ~size:1000000 ()

let () =
  let libraries = [
      "logs";
      "mirage-logs";
      "git";
      "pih-store"
    ] in
  register ~libraries "review" [
    main $ https $ keys $ default_clock
  ]
