open V1
open V1_LWT
open Lwt
open Result

let log_src = Logs.Src.create "ucn.wifi"
module Log = (val Logs.src_log log_src : Logs.LOG)

module Wifi_Store = struct

  module S = Data_store

  let to_meta s = ""

  let with_ok_unit t = t >>= function
    | Ok () -> return (Ok "")
    | Error _ as e -> return e

  let with_ok_list t = t >>= function
    | Ok lst ->
       let r = Ezjsonm.(list string lst |> to_string) in
       return (Ok r)
    | Error _ as e -> return e


  let read_list store ?src key =
    S.list store ?src ~parent:key () >>= function
    | Error _ as e -> return e
    | Ok id_lst ->
       Lwt_list.fold_left_s (fun acc id ->
         match acc with
         | Error _ as e -> return e
         | Ok v_lst ->
            S.read store ?src (key @ [id]) >>= function
            | Ok v -> return (Ok (v :: v_lst))
            | Error _ as e -> return e) (Ok []) id_lst
       >>= function
       | Error _ as e -> return e
       | Ok v_lst ->
          try
            Ezjsonm.(list (fun v -> from_string v |> value) v_lst)
            |> Ezjsonm.to_string
            |> fun s -> return (Ok s)
          with e -> return (Error e)


  let dispatch store ?src body meth key =
    match meth, key with
    | `GET, key ->
       Lwt.catch (fun () ->
         let name = List.rev key |> List.hd in
         if name = "list" then
           let key = List.rev key |> List.tl |> List.rev in
           with_ok_list (S.list store ?src ~parent:key ())
         else if name = "all" then
           let key = List.rev key |> List.tl |> List.rev in
           read_list store ?src key
         else
           S.read store ?src key) (fun exn -> return (Error exn))
    | `POST, key ->
       with_ok_unit (S.update store ?src key body)
    | _ -> return (Error Not_found)

  let init ~time () =
    S.make ~owner:"ucn.wifi" ~time ()
end


module Dispatcher
    (Http: Cohttp_lwt.Server)
    (FS: KV_RO) = struct

  let headers = Cohttp.Header.of_list [
    "Strict-Transport-Security", "max-age=31536000";
    "Access-Control-Allow-Origin", "*"]
  let empty_body = Cohttp_lwt_body.empty

  let file_dispatcher fs name =
    let name = if name = "/" then "/index.html" else name in
    FS.size fs name >>= function
    | `Error (FS.Unknown_key _) -> return (Error Not_found)
    | `Ok size ->
       FS.read fs name 0 (Int64.to_int size) >>= function
       | `Error (FS.Unknown_key _) -> return (Error Not_found)
       | `Ok bufs -> return (Ok (Cstruct.copyv bufs))

  (* TODO: get src information *)
  let wifi_dispatcher fs store ?src uri req body =
    let p = Uri.path uri in
    let steps = Astring.String.cuts ~empty:false ~sep:"/" p in
    let meth = Cohttp.Request.meth req in
    Cohttp_lwt_body.to_string body >>= fun v ->
    Log.app (fun f -> f "%s %s" (Cohttp.Code.string_of_method meth) p);
    Wifi_Store.dispatch store ?src v meth steps
    >>= function
    | Ok "" -> Http.respond ~headers ~status:`OK ~body:empty_body ()
    | Ok json ->
       let headers = Cohttp.Header.add headers
         "content-type" "application/json" in
       let body = Cohttp_lwt_body.of_string json in
       Http.respond ~headers ~status:`OK ~body ()
    | Error e ->
       file_dispatcher fs p >>= function
       | Error e ->
          let body = Printexc.to_string e |> Cohttp_lwt_body.of_string in
          Http.respond ~headers ~status:`Not_found ~body ()
       | Ok f ->
          let body = Cohttp_lwt_body.of_string f in
          Http.respond ~headers ~status:`OK ~body ()


  let redirect ?src uri _req _body =
    let new_uri = Uri.with_scheme uri (Some "https") in
    let new_uri = Uri.with_port new_uri (Some 4433) in
    let headers =
      Cohttp.Header.add headers "location" (Uri.to_string new_uri)
    in
    Http.respond ~headers ~status:`Moved_permanently ~body:`Empty ()


  let serve dispatch =
    let callback (_, cid) request body =
      let src = None in
      let uri = Cohttp.Request.uri request in
      let cid = Cohttp.Connection.to_string cid in
      Log.info (fun f -> f  "[%s] serving %s." cid (Uri.to_string uri));
      dispatch ?src uri request body
    in
    let conn_closed (_,cid) =
      let cid = Cohttp.Connection.to_string cid in
      Log.info (fun f -> f "[%s] closing" cid);
    in
    Http.make ~conn_closed ~callback ()
end


module Main
     (Http: Cohttp_lwt.Server)
     (FS: KV_RO)
     (Keys: KV_RO)
     (Clock: V1.CLOCK) = struct

  module X509 = Tls_mirage.X509(Keys)(Clock)
  module Logs_reporter = Mirage_logs.Make(Clock)
  module D = Dispatcher(Http)(FS)

  let time () = Clock.(
    let t = time () |> gmtime in
    Printf.sprintf "%d:%d:%d:%d:%d:%d"
      t.tm_year t.tm_mon t.tm_mday t.tm_hour t.tm_min t.tm_sec)

  let tls_init kv =
    X509.certificate kv `Default >>= fun cert ->
    let conf = Tls.Config.server ~certificates:(`Single cert) () in
    Lwt.return conf

  let start http fs keys _clock =
    Logs.(set_level (Some Info));
    let console_threshold src =
      if src = log_src then Logs.Info
      else Logs.Error in
    Logs_reporter.(create ~console_threshold () |> run) @@ fun () ->

    Wifi_Store.init ~time () >>= fun s ->

    let conf = Server_config.read () in
    let tls_port = try List.assoc "tls_port" conf with Not_found -> 4433 in
    let http_port = try List.assoc "http_port" conf with Not_found -> 8088 in

    tls_init keys >>= fun cfg ->
    let tcp = `TCP tls_port in
    let tls = `TLS (cfg, tcp) in

    Lwt.join [
      http tls @@ D.serve (D.wifi_dispatcher fs s);
      http (`TCP http_port) @@ D.serve D.redirect
    ]
end
