open V1
open V1_LWT
open Lwt
open Result

let log_src = Logs.Src.create "ucn.review"
module Log = (val Logs.src_log log_src : Logs.LOG)

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
       with_ok_unit (S.create store ?src id body)
    | ["read"; id] ->
       S.read store ?src id
    | ["update"; id] ->
       with_ok_unit (S.update store ?src id body)
    | ["delete"; id] ->
       with_ok_unit (S.remove store ?src id)
    | ["meta"; id] ->
       S.get_meta store ?src id to_meta
    | ["list"] ->
       with_ok_list (S.list store ?src ())
    | _ -> return (Error Not_found)

  let init ~time () =
    S.make ~owner:"ucn.review" ~time ~check ()
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

  let start http keys _clock =
    Logs.(set_level (Some Info));
    Logs_reporter.(create () |> run) @@ fun () ->

    Review_Store.init ~time () >>= fun s ->

    tls_init keys >>= fun cfg ->
    let tcp = `TCP 4433 in
    let tls = `TLS (cfg, tcp) in

    Lwt.join [
      http tls @@ D.serve (D.review_dispatcher s);
      http (`TCP 8081) @@ D.serve D.redirect
    ]
end
