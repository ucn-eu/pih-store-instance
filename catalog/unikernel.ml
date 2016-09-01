open V1
open V1_LWT
open Lwt
open Result

let src = Logs.Src.create "ucn.catalog"
module Log = (val Logs.src_log src : Logs.LOG)


module Catalog_Store = struct

  module S = Data_store
  module ConvenerMap = Map.Make(String)

  let with_ok_list t = t >>= function
    | Ok lst ->
       let r = Ezjsonm.(list string lst |> to_string) in
       return (Ok r)
    | Error _ as e -> return e

  let with_ok_unit t = t >>= function
    | Ok () -> return (Ok "")
    | Error _ as e -> return e

  let delegation_operands body =
    Cohttp_lwt_body.to_string body >>= fun body -> Ezjsonm.(
    let v = from_string body |> value |> get_dict in
    let f = List.assoc "file_id" v |> get_string in
    let u = List.assoc "user_id" v |> get_string in
    return (f, u))

  let record_delegations store ?src ~file_id op =
    S.read store ?src [file_id] >>= function
    | Error _ as e -> return e
    | Ok v ->
       let l = Ezjsonm.(from_string v |> value |> get_list get_string) in
       match op with
       | `Delegate user_id ->
          let l = if not (List.mem user_id l) then user_id :: l else l in
          let v = Ezjsonm.(list string l |> to_string) in
          S.update store ?src [file_id] v
       | `Revoke user_id ->
          let l = List.filter (fun i -> not (i = user_id)) l in
          let v = Ezjsonm.(list string l |> to_string) in
          S.update store ?src [file_id] v


  let dispatch (store, muse, cmap) ?src body = function
    | ["users"] ->
       with_ok_list (Muse.get_users muse)
    | category :: steps ->
       let con = ConvenerMap.find category cmap in
       match steps with
       | ["upload"] ->
          Cohttp_lwt_body.to_string body >>= fun body -> Ezjsonm.(
          let v = from_string body |> value |> get_dict in
          let f = List.assoc "file_id" v |> get_string in
          let d = List.assoc "data" v |> get_string in
          Muse.upload muse ~file_id:f ~data:d)
          |> with_ok_unit
       | "read" :: "meta" :: [id] ->
          Convener.get_meta con id
       | "read" :: ["list"] ->
          with_ok_list (Convener.list con)
       | "read" :: "delegation" :: [file_id] ->
          S.read store ?src [file_id]
       | ["delegate"] ->
          delegation_operands body >>= fun (file_id, user_id) ->
          Muse.delegate muse ~file_id ~user_id
          >>= (function
          | Ok () -> record_delegations store ?src ~file_id (`Delegate user_id)
          | Error _ as e -> return e)
          |> with_ok_unit
       | ["revoke"] ->
          delegation_operands body >>= fun (file_id, user_id) ->
          Muse.revoke muse ~file_id ~user_id
          >>= (function
          | Ok () -> record_delegations store ?src ~file_id (`Revoke user_id)
          | Error _ as e -> return e)
          |> with_ok_unit

  let gen_cmap ctx l =
    let map = ConvenerMap.empty in
    Lwt_list.fold_left_s (fun acc (domain, uri) ->
      let root = Uri.of_string uri in
      Convener.create ~domain ~ctx ~root
      >>= fun con ->
      ConvenerMap.add domain con acc
      |> return) map l

  let check s = Ezjsonm.(
    try
      let v = from_string s |> value in
      let _  = get_list get_string v in
      true
    with _ -> false)

  let init (resolver, conduit, uri) muse domains ~time () =
    let owner = "ucn.catalog" in
    let backend = `Http ((resolver, conduit, uri), owner) in
    S.make ~backend ~time ~check () >>= fun s ->

    let ctx = Cohttp_mirage.Client.ctx resolver conduit in
    gen_cmap ctx domains >>= fun cmap ->

    let root = Uri.of_string muse in
    Muse.init ~ctx ~root >>= fun muse ->
    return (s, muse, cmap)
end


module Dispatcher
    (Http: Cohttp_lwt.Server) = struct

  let headers = Cohttp.Header.init_with
    "Strict-Transport-Security" "max-age=31536000"
  let empty_body = Cohttp_lwt_body.empty

  (* TODO: get src information *)
  let catalog_dispatcher store ?src uri req body =
    let p = Uri.path uri in
    let steps = Astring.String.cuts ~empty:false ~sep:"/" p in
    Catalog_Store.dispatch store ?src body steps
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


  let async_hook = function
    | exn -> Log.err (fun f -> f "async_hook %s" (Printexc.to_string exn))


  let start http resolver conduit keys _clock =
    let () = Lwt.async_exception_hook := async_hook in
    tls_init keys >>= fun tls_conf ->

    let persist_host = Key_gen.persist_host () |> Ipaddr.V4.to_string in
    let persist_port = Key_gen.persist_port () in
    let persist_uri = Uri.make ~scheme:"http" ~host:persist_host ~port:persist_port () in

    let muse = Dom_configs.muse_endpoint in
    let domains = Dom_configs.domain_lst in

    Catalog_Store.init (resolver, conduit, persist_uri) muse domains ~time () >>= fun s ->

    tls_init keys >>= fun cfg ->
    let tls = `TLS (cfg,`TCP 8443) in

    Lwt.join [
      http tls @@ D.serve (D.catalog_dispatcher s);
      http (`TCP 8080) @@ D.serve D.redirect
    ]
end


(*

module Main (Http: Cohttp_lwt.Server) (Stack: STACKV4) (Keys: KV_RO) (Clock: CLOCK) = struct

  module Review = struct
    let cnt = ref 0
    let get_id () =
      let id = !cnt in
      let () = incr cnt in
      "review" ^ (string_of_int id)

    let uri_root =
      Uri.make ~scheme:"http" ~host:"localhost" ~port:8443 ()

    let ctx = ref None
    let set_ctx c = ctx := Some c
    let get_ctx () =
      match !ctx with
      | Some c -> c
      | None -> failwith "ctx not initialized"

    let read_list_remote () =
      let uri = Uri.with_path uri_root "/list" in
      let ctx = get_ctx () in
      Cohttp_mirage.Client.get ~ctx uri
      >>= fun (resp, body) ->
      if Cohttp.Response.status resp <> `OK then begin
        let code = Cohttp.(Code.code_of_status (Response.status resp)) in
        Log.warn (fun f -> f "retrieve review data list failed: %d" code);
        return None end
      else
        Cohttp_lwt_body.to_string body
        >>= fun body_str ->
        try
          match Ezjsonm.from_string body_str with
          | `A l -> return_some (List.map Ezjsonm.get_string l)
          | _ -> begin
            Log.warn (fun f -> f "not expected json format: %s" body_str);
            return_none end
        with Ezjsonm.Parse_error (_,info) ->
          Log.warn (fun f -> f "parse error: %s" info);
          return_none

    let read_meta_remote id =
      let uri = Uri.with_path uri_root ("/meta/" ^ id) in
      let ctx = get_ctx () in
      Cohttp_mirage.Client.get ~ctx uri
      >>= fun (resp, body) ->
      if Cohttp.Response.status resp <> `OK then begin
        let code = Cohttp.(Code.code_of_status (Response.status resp)) in
        Log.warn (fun f -> f "get review %s meta data failed: %d" id code);
        return_none end
      else
        Cohttp_lwt_body.to_string body
        >>= fun body_str ->
        return_some ((get_id ()), body_str)


    let create_list store l =
      let path = ["review"; "list"] in
      let v = Ezjsonm.strings l in
      Store.update (store "create review id list") path v


    let rec read_list store =
      let path = ["review"; "list"] in
      match%lwt Store.read (store "read review list") path with
      | Some l -> return_some l
      | None ->
         match%lwt read_list_remote () with
         | None -> return_none
         | Some l ->
            create_list store l
            >>= fun () -> read_list store


    let create_meta store id file_id data =
      let path = ["review"; "meta"; id] in
      let v = Ezjsonm.(dict [
        "file_id", string file_id;
        "data", string data]) in
      Store.update (store ("create review meta " ^ id)) path v


    let has_meta store id =
      let path = ["review"; "meta"; id] in
      Store.read (store ("check for review meta" ^ id)) path

    let rec read_meta store rst =
      let id = List.hd rst in
      match%lwt has_meta store id with
      | Some v -> return_some v
      | None ->
         match%lwt read_meta_remote id with
         | None -> return_none
         | Some (file_id, data) ->
            Muse.encrypt ~file_id data
            >>= fun () ->
            create_meta store id file_id data
            >>= fun () -> read_meta store rst


    let read_delegations store file_id =
      let path = ["review"; "delegation"; file_id] in
      Store.read (store ("read delegations of " ^ file_id)) path


    let delegate store user_id file_id =
      read_delegations store file_id
      >>= (function
      | Some (`A l) -> return (List.map Ezjsonm.get_string l)
      | _ -> return [])
      >>= fun _l ->
      let l = if List.mem user_id _l then _l else user_id :: _l in
      let v = Ezjsonm.strings l in
      let msg = Printf.sprintf "delegate %s to %s" file_id user_id in
      let path = ["review"; "delegation"; file_id] in
      Store.update (store msg) path v


    let revoke store user_id file_id =
      read_delegations store file_id
      >>= (function
      | Some (`A l) -> return (List.map Ezjsonm.get_string l)
      | _ -> return [])
      >>= fun _l ->
      if not (List.mem user_id _l) then
        Log.warn (fun f ->
          f "revoke: %s seems not delegated to %s" file_id user_id);
      let l = List.filter (fun id -> id <> user_id) _l in
      let v = Ezjsonm.strings l in
      let msg = Printf.sprintf "revoke %s from %s" file_id user_id in
      let path = ["review"; "delegation"; file_id] in
      Store.update (store msg) path v


    let revoke_all store file_id =
      match%lwt read_delegations store file_id with
      | None -> return_unit
      | Some d ->
         let users = Ezjsonm.(value d |> get_strings) in
         Lwt_list.iter_s (fun user -> revoke store file_id user) users


    let sync_with_remote store =
      let%lwt local = match%lwt read_list store with
        | None -> return []
        | Some v -> return Ezjsonm.(get_strings (value v)) in
      let%lwt remote = match%lwt read_list_remote () with
        | None -> return []
        | Some l -> return l in
      let check_removed id =
        if not (List.mem id remote) then
          match%lwt read_meta store [id] with
          | None -> return_unit
          | Some v ->
             let file_id = Ezjsonm.(value v
               |> get_dict
               |> List.assoc "file_id"
               |> get_string) in
             revoke_all store file_id
        else return_unit in
      Lwt_list.iter_s check_removed local
      >>= fun () -> create_list store remote
      >>= fun () -> read_list store

    let headers =
      let hdr = Cohttp.Header.init () in
      Cohttp.Header.add hdr "Access-Control-Allow-Origin" "*" (* make chrome happy *)

    let respond_json ?status ?headers v =
      let body = Ezjsonm.to_string v in
      let status =
        match status with
        | None -> `OK
        | Some s -> s in
      let headers =
        let h = match headers with
          | None -> Cohttp.Header.init ()
          | Some h -> h in
        Cohttp.Header.add h "Content-type" "application/json" in
      Server.respond_string ~status ~headers ~body ()


    let json_of_body body =
      Cohttp_lwt_body.to_string body
      >>= fun body_str ->
      return (Ezjsonm.from_string body_str)


    let dispatch store req body = function
      | ["users"] ->
         Muse.get_users () >>= fun users ->
         Ezjsonm.strings users
         |> respond_json ~headers
      | "read" :: "meta" :: [id] ->
         (match%lwt read_meta store [id] with
         | None ->
            Server.respond_error ~headers ~status:`Not_found ~body:"" ()
         | Some v ->
            respond_json ~headers v)
      | "read" :: ["list"] ->
         (match%lwt read_list store with
         | None ->
            Server.respond_error ~headers ~status:`Not_found ~body:"" ()
         | Some v ->
            let l = Ezjsonm.(value v |> get_strings) in
            let%lwt v = Lwt_list.map_s (fun id ->
              match%lwt has_meta store id with
              | None -> return (id, Ezjsonm.dict [])
              | Some m ->
                 let file_id = Ezjsonm.(value m
                   |> get_dict
                   |> List.assoc "file_id"
                   |> get_string) in
                 let%lwt delegations =
                   match%lwt read_delegations store file_id with
                   | None -> return []
                   | Some d -> return Ezjsonm.(value d |> get_strings) in
                 let info = Ezjsonm.([
                   "file_id", string file_id;
                   "delegations", strings delegations]
                   |> dict) in
                 return (id, info)) l in
            let v = Ezjsonm.dict v in
            respond_json ~headers v)
      | "read" :: "delegation" :: [file_id] ->
         (match%lwt read_delegations store file_id with
         | None ->
            let v = Ezjsonm.strings [] in
            respond_json ~headers v
         | Some v ->
            respond_json ~headers v)
      | ["delegate"] ->
         json_of_body body >>= fun obj ->
         let dict = Ezjsonm.get_dict obj in
         (if not (List.mem_assoc "file_id" dict
                 && List.mem_assoc "user_id" dict) then
           Cohttp_lwt_body.to_string body
           >>= fun b ->
           Log.err (fun f -> f "bad request body format:%s" b);
           Server.respond_error ~headers ~status:`Bad_request ~body:"" ()
         else
           let file_id = List.assoc "file_id" dict |> Ezjsonm.get_string in
           let user_id = List.assoc "user_id" dict |> Ezjsonm.get_string in
           Muse.delegate ~file_id ~user_id
           >>= function
           | None ->
              Server.respond_error ~headers ~status:`Internal_server_error ~body:"" ()
           | Some _ ->
              delegate store user_id file_id
              >>= fun () ->
              Server.respond ~headers ~status:`OK ~body:Cohttp_lwt_body.empty ())
      | ["revoke"] ->
         json_of_body body >>= fun obj ->
         let dict = Ezjsonm.get_dict obj in
         (if not (List.mem_assoc "file_id" dict
                 && List.mem_assoc "user_id" dict) then
           Cohttp_lwt_body.to_string body
           >>= fun b ->
           Log.err (fun f -> f "bad request body format:%s" b);
           Server.respond_error ~headers ~status:`Bad_request ~body:"" ()
         else
           let file_id = List.assoc "file_id" dict |> Ezjsonm.get_string in
           let user_id = List.assoc "user_id" dict |> Ezjsonm.get_string in
           Muse.revoke ~file_id ~user_id
           >>= function
           | None ->
              Server.respond_error ~headers ~status:`Internal_server_error ~body:"" ()
           | Some _ ->
              revoke store user_id file_id
              >>= fun () ->
              Server.respond ~headers ~status:`OK ~body:Cohttp_lwt_body.empty ())
      | ["sync"] ->
         (match%lwt sync_with_remote store with
         | None ->
            Server.respond_error ~headers ~status:`Not_found ~body:"" ()
         | Some v ->
            respond_json ~headers v)
      | _ ->
         Server.respond_error ~headers ~status:`Not_found ~body:"not implemented" ()
  end


  let handle_request store conn req body =
    let flow = fst conn in
    let remote_ip, remote_port = TCP.get_dest flow in
    let () = Log.app (fun f -> f "connection from %s:%n"
      (Ipaddr.V4.to_string remote_ip) remote_port) in
    let path = Cohttp.Request.uri req in
    match sqlit_path path with
    | "review" :: tl ->
       Review.dispatch store req body tl
    | _ -> Server.respond_error ~status:`Not_found ~body:"not implemented" ()


  let init stack =
    let s = Conduit_mirage.stackv4 (module Stack) in
    Conduit_mirage.(with_tcp empty s stack)
    >>= fun conduit->
    let resolver = Resolver_mirage.localhost in
    let ctx = Cohttp_mirage.Client.ctx resolver conduit in
    Review.set_ctx ctx;
    Muse.init ctx () >>= fun () ->
    match !Muse.muse_client with
    | Some _ -> return_unit
    | None ->
       Log.err (fun f -> f "try with other user_id");
       fail_with "initialization failed"


  module Logs_reporter = Mirage_logs.Make(Clock)

  let tls_init kv =
    X509.certificate kv `Default >>= fun cert ->
    let conf = Tls.Config.server ~certificates:(`Single cert) () in
    Lwt.return conf


  let start http keys clock () =
    Logs.set_level (Some Info);
    Logs_reporter.(create () |> run) @@ fun () ->

    tls_init keys >>= fun cfg ->
    let tcp = `TCP 4433 in
    let tls = `TLS (cfg, tcp) in

    Store.init () >>= fun store ->

    init stack >>= fun () ->
    let http = Server.make ~conn_closed:ignore ~callback:(handle_request store) () in
    Stack.listen_tcpv4 stack ~port:8088 (Server.listen http);
    Stack.listen stack
end
 *)
