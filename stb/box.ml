open V1
open V1_LWT
open Lwt
open Result

let log_src = Logs.Src.create "ucn.stb.box"
module Log = (val Logs.src_log log_src : Logs.LOG)
module Client = Cohttp_mirage.Client

module Box_Store = struct

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


  let dispatch store ?src headers body meth steps =
    let path = String.concat "/" steps in
    match meth, steps with
    | `GET, steps ->
       let len = List.length steps in
       if len = 0 then (* PIH discovery endpoint *)
         return @@ Ok "<xml>true</xmd>"
       else if len = 3 then (* it's a directory reading for yy/mm/dd *)
         S.list store ?src ~parent:steps () >>= function
         | Error e ->
            Log.err (fun f -> f "list %s" path);
            return @@ Error e
         | Ok ts_lst ->
            Lwt_list.fold_right_s (fun ts acc ->
              let key = steps @ [ts] in
              S.read store ?src key >>= function
              | Error _ ->
                 Log.err (fun f -> f "no value for %s/%s" path ts);
                 return acc
              | Ok v -> return @@ v :: acc) ts_lst [] >>= fun v_lst ->
            Ezjsonm.(list (fun v -> from_string v |> value) v_lst)
            |> Ezjsonm.to_string
            |> fun v -> return @@ Ok v
       else if len = 4 then (* it's a data entry reading for yy/mm/dd/tunetime *)
         S.read store ?src steps
       else begin
           Log.err (fun f -> f "unmatched `GET req: %s" path);
           return (Error Not_found) end

    | `POST, ["endpoint"] ->
       with_ok_unit @@ S.update store ?src ["endpoint"] body

    | `POST, steps ->
       assert (4 = List.length steps); (* entry writing for yy/mm/dd/tunetime *)
       let dev =
         match Cohttp.Header.get headers "UA-DeviceId" with
         | None -> ""
         | Some id -> id in
       let stream = Lwt_stream.of_list [body] in
       let content_type =
         match Cohttp.Header.get headers "Content-Type" with
         | None -> failwith "no Content-Type"
         | Some typ -> typ in
       let callback = fun ~name ~filename _ -> return_unit in
       Multipart.parse ~stream ~content_type ~callback >>= fun lst ->
       let v = Ezjsonm.(
         ["dev", dev] @ lst
         |> List.map (fun (k, v) -> k, string v)
         |> dict
         |> to_string) in
       with_ok_unit @@ S.update store ?src steps v

    | _ -> return @@ Error Not_found


  let init remote_conf ~time () =
    let owner = "ucn.stb.box" in
    let backend = `Http (remote_conf, owner) in
    S.make ~backend ~time ()

 end


module Dispatcher
    (Http: Cohttp_lwt.Server) = struct

  let headers = Cohttp.Header.of_list [
    "Strict-Transport-Security", "max-age=31536000";
    "Access-Control-Allow-Origin", "*"]
  let empty_body = Cohttp_lwt_body.empty

  (* TODO: get src information *)
  let box_dispatcher store ?src uri req body =
    let p = Uri.path uri in
    let steps = Astring.String.cuts ~empty:false ~sep:"/" p in
    let meth = Cohttp.Request.meth req in
    let req_headers = Cohttp.Request.headers req in
    Cohttp_lwt_body.to_string body >>= fun body ->
    Log.app (fun f -> f "%s %s" (Cohttp.Code.string_of_method meth) p);
    Box_Store.dispatch store ?src req_headers body meth steps
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

    let tcp = `TCP  8443 in
    let tls = `TLS (cfg, tcp) in

    let persist_host = Key_gen.persist_host () |> Ipaddr.V4.to_string in
    let persist_port = Key_gen.persist_port () in
    let persist_uri = Uri.make ~scheme:"http" ~host:persist_host ~port:persist_port () in

    Box_Store.init (resolver, conduit, persist_uri) ~time () >>= fun s ->

    Lwt.pick [
      http tls @@ D.serve (D.box_dispatcher s);
      http (`TCP 8080) @@ D.serve D.redirect;
    ]
end
