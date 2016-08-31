open Lwt
open Irmin_unix

module Server = Cohttp_lwt_unix.Server

let config = Irmin_git.config ~root:"." ()
module Store = Irmin_git.FS(Irmin.Contents.String)(Irmin.Ref.String)(Irmin.Hash.SHA1)
module S = Irmin_http_server.Make(Store)


let main port =
  Store.Repo.create config >>= Store.master task >>= fun t  ->
  let s = S.http_spec ~strict:false (t "to get server sepc") in
  let mode = `TCP (`Port port) in
  Printf.printf "irmin server listens on port %d...\n%!" port;
  Server.create ~mode s


let () =
  let cnt = Array.length Sys.argv in
  if cnt < 2 then
    Printf.printf "Wroing number of arguments %d (!=2)\n%!" cnt
  else
    let port = Sys.argv.(1) |> int_of_string in
    Lwt_main.run @@ main port
