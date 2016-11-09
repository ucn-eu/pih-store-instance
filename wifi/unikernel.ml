open V1
open V1_LWT
open Lwt
open Result

let log_src = Logs.Src.create "ucn.wifi"
module Log = (val Logs.src_log log_src : Logs.LOG)
module Client = Cohttp_mirage.Client

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


  let init_with_dump ctx (host, port) path store =
    let (/) d f = Printf.sprintf "%s/%s" d f in
    let uri = Uri.make ~scheme:"http" ~host ~port () in
    let list_uri = Uri.with_path uri (path / "list") in
    Client.get ~ctx list_uri >>= fun (res, body) ->
    let status = Cohttp.Response.status res in
    if status <> `OK then begin
      Log.err (fun f -> f "init_with_dump list: %s"
        (Cohttp.Code.string_of_status status));
      return_none end
    else
      Cohttp_lwt_body.to_string body >>= fun s ->
      return Ezjsonm.(s |> from_string |> value |> get_list get_string)
      >>= fun l ->
      Log.info (fun f -> f "init with %d files" (List.length l));
      let l =
        List.map int_of_string l
        |> List.sort compare |> List.map string_of_int in
      Lwt_list.fold_left_s (fun acc file ->
        let file_uri = Uri.with_path uri (path / file) in
        Client.get ~ctx file_uri >>= fun (res, dump) ->
        let s = Cohttp.Response.status res in
        if s <> `OK then return_none else
          Cohttp_lwt_body.to_string dump >>= fun dump ->
          S.import dump store >>= function
          | Error e ->
             Log.err (fun f -> f "error init_with_dump: %s"
               (Printexc.to_string e));
             return_none
          | Ok h ->
             Log.info (fun f -> f "init_with_dump: import %s" file);
             return_some [h]) None l


  (*let persist_t ctx uri s min period =
    let rec aux ?min () =
      OS.Time.sleep period >>= fun () ->
      S.export ?min s >>= function
      | Error e ->
         Log.err (fun f -> f "wifi persist: %s" (Printexc.to_string e));
         aux ?min ()
      | Ok None ->
         aux ?min ()
      | Ok (Some (head, dump)) ->
         let body = Cohttp_lwt_body.of_string dump in
         Client.post ~ctx ~body uri >>= fun (res, _) ->
         let status =
           Cohttp.Response.status res
           |> Cohttp.Code.string_of_status in
         Log.info (fun f -> f "persist to %s: %s" (Uri.to_string uri) status);
         aux ~min:[head] ()
    in
    aux ?min ()*)


  let init remote_conf ~time () =
    let owner = "ucn.wifi" in
    let backend = `Http (remote_conf, owner) in
    S.make ~backend ~time ()

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
  let wifi_dispatcher (fs, store) ?src uri req body =
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
    let new_uri = Uri.with_port new_uri (Some 8443) in
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
         (*(Http: Cohttp_lwt.Server)*)
     (Stack:STACKV4)
     (Resolver: Resolver_lwt.S)
     (Conduit: Conduit_mirage.S)
     (FS: KV_RO)
     (Keys: KV_RO)
     (Clock: V1.PCLOCK) = struct

  module TLS = Tls_mirage.Make(Stack.TCPV4)
  module Http = Cohttp_mirage.Server(Stack.TCPV4)
  module Https = Cohttp_mirage.Server(TLS)
  
  module X509 = Tls_mirage.X509(Keys)(Clock)
  module Logs_reporter = Mirage_logs.Make(Clock)
  module D = Dispatcher(Https)(FS)

  let with_tls tls_conf conf f =
    TLS.server_of_flow tls_conf f >>= function
    | `Error e ->
       Log.err (fun f -> f "upgrade: %s" (TLS.error_message e));
       return_unit
    | `Eof ->
       Log.err (fun f -> f "upgrade: EOF");
       return_unit
    | `Ok f ->
       let callback (_, cid) request body =
         let src = None in
         let uri = Cohttp.Request.uri request in
         let cid = Cohttp.Connection.to_string cid in
         Log.info (fun f -> f  "[%s] serving %s." cid (Uri.to_string uri));
         D.wifi_dispatcher conf ?src uri request body
       in
       let conn_closed (_,cid) =
         let cid = Cohttp.Connection.to_string cid in
         Log.info (fun f -> f "[%s] closing" cid);
       in
       let t = Https.make ~conn_closed ~callback () in
       Https.(listen t f)

  let tls_init kv =
    X509.certificate kv `Default >>= fun cert ->
    let conf = Tls.Config.server ~certificates:(`Single cert) () in
    Lwt.return conf

  let async_hook exn =
    Log.err (fun f -> f "async hook: %s" (Printexc.to_string exn))

  let start stack resolver conduit fs keys _clock =
    Lwt.async_exception_hook := async_hook;

    tls_init keys >>= fun cfg ->

    (*let tcp = `TCP  8443 in*)
    (*let tls = `TLS (cfg, tcp) in*)

    let persist_host = Key_gen.persist_host () |> Ipaddr.V4.to_string in
    let persist_port = Key_gen.persist_port () in
    let persist_uri = Uri.make ~scheme:"http" ~host:persist_host ~port:persist_port () in

    let time () = Clock.now_d_ps _clock |> Ptime.v |> Ptime.to_rfc3339 ~space:true in
    Wifi_Store.init (resolver, conduit, persist_uri) ~time () >>= fun s ->

    Stack.listen_tcpv4 stack ~port:8443 (with_tls cfg (fs, s)); 
    Stack.listen stack
    
    (*Lwt.pick [
      http tls @@ D.serve (D.wifi_dispatcher fs s);
      http (`TCP 8080) @@ D.serve D.redirect;
    ]*)
end
