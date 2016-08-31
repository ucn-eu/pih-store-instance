open Cohttp
open Cohttp_lwt_unix

let (>>=) = Lwt.bind

let (/) = Printf.sprintf "%s/%s"
let docroot = "ucn.bak"
let counters : (string * int) list ref = ref []


let write_to_file subdir buf =
  let open Lwt_unix in
  let dir = docroot / subdir in
  file_exists dir >>= (function
  | false -> mkdir dir 0o700
  | true -> Lwt.return_unit) >>= fun () ->
  let file =
    try
      let count = List.assoc subdir !counters in
      let () = counters := List.map (fun (s,c) ->
        if s = subdir then s, c + 1 else s, c) !counters in
      string_of_int count
    with Not_found ->
      let () = counters := (subdir, 1) :: !counters in
      "0" in
  let path = dir / file in
  openfile path [O_WRONLY; O_CREAT; O_TRUNC] 0o600 >>= fun fd ->
  let len = Bytes.length buf in
  Lwt_unix.write fd buf 0 len >>= fun _ ->
  Lwt.return_unit


let list_files subdir =
  let open Lwt_unix in
  let dir = docroot / subdir in
  file_exists dir >>= (function
  | false -> mkdir dir 0o700
  | true -> Lwt.return_unit) >>= fun () ->
  let s = files_of_directory dir in
  Lwt_stream.to_list s >>= fun l ->
  List.filter (fun name -> name <> "." && name <> "..") l
  |> Lwt.return


let callback (_, conn) req body =
  let meth = Cohttp.Request.meth req in
  let uri = Cohttp.Request.uri req in
  let path = Uri.path uri |> Astring.String.cuts ~empty:false ~sep:"/" in
  Printf.printf "connection %s for %s\n%!"
    (Cohttp.Connection.to_string conn) (Uri.path uri);
  let action = path |> List.rev |> List.hd in
  let dir = List.hd path in
  match meth with
  | `GET ->
     if action <> "list" then
       let fname = Server.resolve_local_file docroot uri in
       Server.respond_file ~fname ()
     else
       list_files dir >>= fun l ->
       let s = Ezjsonm.(list string l |> to_string) in
       let body = Cohttp_lwt_body.of_string s in
       Server.respond ~status:`OK ~body ()
  | `POST ->
     Cohttp_lwt_body.to_string body >>= fun buf ->
     Lwt.catch
       (fun () ->
         write_to_file dir buf >>= fun () ->
         let msg = Printf.sprintf "write to %s: OK" (Uri.to_string uri) in
         Printf.printf "%s\n%!" msg;
         Server.respond ~status:`OK ~body:Cohttp_lwt_body.empty ())
       (fun exn ->
         let msg = Printf.sprintf "error write to %s: %s"
           (Uri.path uri) (Printexc.to_string exn) in
         Printf.printf "%s\n%!" msg;
         Server.respond_error ~status:`Bad_request ~body:msg ())
  | _ -> Server.respond_error ~status:`Not_implemented ~body:"" ()


let serve () =
  let conn_closed (_, conn) =
    Printf.printf "connection %s closed\n%!"
      (Cohttp.Connection.to_string conn) in
  let s = Server.make ~conn_closed ~callback () in
  Conduit_lwt_unix.init ~src:"10.0.0.1" () >>= fun conduit_ctx ->
  let ctx = Cohttp_lwt_unix_net.init ~ctx:conduit_ctx () in
  Server.create ~ctx ~mode:(`TCP (`Port 10000)) s


let init_counter () =
  let open Lwt_unix in
  let s = files_of_directory docroot in
  Lwt_stream.to_list s >>= fun l ->
  Lwt_list.fold_left_s (fun acc subdir ->
    let dir = docroot / subdir in
    let s = files_of_directory dir in
    Lwt_stream.to_list s >>= fun files ->
    let count = List.length files - 2 (* . and .. *) in
    Lwt.return ((subdir, count) :: acc)) [] l >>= fun c ->
  counters := c;
  Lwt.return_unit

let () =
  Lwt.bind (init_counter ()) serve
  |> Lwt_main.run
