open V1
open V1_LWT
open Lwt
open Result

let log_src = Logs.Src.create "ucn.review"
module Log = (val Logs.src_log log_src : Logs.LOG)
module Client = Cohttp_mirage.Client

module Review_Store = struct

  module S = Data_store

  let check s =
    try
      let v = Ezjsonm.from_string s in
      match v with
      | `O _ as d ->
         let l = Ezjsonm.get_dict d in
         (List.mem_assoc "id" l
          && List.mem_assoc "title" l
          && List.mem_assoc "rating" l
          && List.mem_assoc "comment" l)
      | _ -> false
    with _ -> false

  let to_meta s = ""

  let with_ok_unit t = t >>= function
    | Ok () -> return (Ok "")
    | Error _ as e -> return e

  let with_ok_list t = t >>= function
    | Ok lst ->
       let r = Ezjsonm.(list string lst |> to_string) in
       return (Ok r)
    | Error _ as e -> return e

  let dispatch store ?src body steps =
    match steps with
    | ["create"; id] ->
       with_ok_unit (S.create store ?src [id] body)
    | ["read"; id] ->
       S.read store ?src [id]
    | ["update"; id] ->
       with_ok_unit (S.update store ?src [id] body)
    | ["delete"; id] ->
       with_ok_unit (S.remove store ?src [id])
    | ["meta"; id] ->
       S.get_meta store ?src [id] to_meta
    | ["list"] ->
       with_ok_list (S.list store ?src ())
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


  let persist_t ctx uri s min period =
    let rec aux ?min () =
      OS.Time.sleep period >>= fun () ->
      S.export ?min s >>= function
      | Error e ->
         Log.err (fun f -> f "review persist: %s" (Printexc.to_string e));
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
    aux ?min ()


  let init remote_conf ~time () =
    let owner = "ucn.review" in
    let backend = `Http (remote_conf, owner) in
    S.make ~backend ~time ()

end


module Dispatcher
    (Http: Cohttp_lwt.Server) = struct

  let headers = Cohttp.Header.init_with
    "Strict-Transport-Security" "max-age=31536000"
  let empty_body = Cohttp_lwt_body.empty

  (* TODO: get src information *)
  let review_dispatcher store ?src uri req body =
    let p = Uri.path uri in
    let steps = Astring.String.cuts ~empty:false ~sep:"/" p in
    Cohttp_lwt_body.to_string body >>= fun v ->
    Review_Store.dispatch store ?src v steps
    >>= function
    | Ok "" -> Http.respond ~headers ~status:`OK ~body:empty_body ()
    | Ok json ->
       let headers = Cohttp.Header.add headers
         "content-type" "application/json" in
       let body = Cohttp_lwt_body.of_string json in
       Http.respond ~headers ~status:`OK ~body ()
    | Error e ->
       let body = Printexc.to_string e |> Cohttp_lwt_body.of_string in
       Http.respond ~headers ~status:`Not_found ~body ()


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
     (Http: Cohttp_lwt.Server)
     (Resolver: Resolver_lwt.S)
     (Conduit: Conduit_mirage.S)
     (Keys: KV_RO)
     (Clock: V1.CLOCK) = struct

  module X509 = Tls_mirage.X509(Keys)(Clock)
  module Logs_reporter = Mirage_logs.Make(Clock)
  module D = Dispatcher(Http)

  let time () = Clock.(
    let t = time () |> gmtime in
    Printf.sprintf "%d:%d:%d:%d:%d:%d"
      t.tm_year t.tm_mon t.tm_mday t.tm_hour t.tm_min t.tm_sec)

  let tls_init kv =
    X509.certificate kv `Default >>= fun cert ->
    let conf = Tls.Config.server ~certificates:(`Single cert) () in
    Lwt.return conf

  let async_hook exn =
    Log.err (fun f -> f "async hook: %s" (Printexc.to_string exn))

  let start http resolver conduit keys _clock =
    Lwt.async_exception_hook := async_hook;
    Logs_reporter.(create () |> run) @@ fun () ->

    tls_init keys >>= fun cfg ->

    let tcp = `TCP 8443 in
    let tls = `TLS (cfg, tcp) in

    let persist_host = Key_gen.persist_host () |> Ipaddr.V4.to_string in
    let persist_port = Key_gen.persist_port () in
    let persist_uri = Uri.make ~scheme:"http" ~host:persist_host ~port:persist_port () in

    Review_Store.init (resolver, conduit, persist_uri) ~time () >>= fun s ->

    Lwt.join [
      http tls @@ D.serve (D.review_dispatcher s);
      http (`TCP 8080) @@ D.serve D.redirect;
    ]
end
