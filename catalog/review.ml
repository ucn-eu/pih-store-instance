open Lwt

module Client = Cohttp_mirage.Client


let cnt = ref 0
let get_id () =
  let id = !cnt in
  let () = incr cnt in
  "review" ^ (string_of_int id)

let uri_root =
  Uri.make ~scheme:"http" ~host:"localhost" ~port:8081 ()

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

let read_meta_opt s p =
  let id = p |> List.rev |> List.hd in
  let s = s ("try read meta " ^ id) in
  Store.

let rec read_meta s r tl =
  let id = List.hd tl in
  let p = r @ [id] in
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


let dispatch s r req body = function
  | "read" :: "meta" :: tl -> read_meta s r tl
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
