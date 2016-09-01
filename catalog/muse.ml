open Lwt
open Result

type t = {
    muse_id   : string;
    muse_root : Uri.t;
    ctx       : Cohttp_mirage.Client.ctx;
}

let src = Logs.Src.create "catalog.muse"
module Log = (val Logs.src_log src : Logs.LOG)


let get_req ~ctx ?headers uri cb =
  Cohttp_mirage.Client.get ~ctx ?headers uri
  >>= fun r ->
  cb r

let post_req ~ctx ?headers uri body cb =
  let body = Cohttp_lwt_body.of_string body in
  Cohttp_lwt_body.length body
  >>= fun (len, body) ->
  let headers = match headers with
    | None -> Cohttp.Header.init ()
    | Some h -> h in
  Cohttp_mirage.Client.post ~ctx ~headers ~body uri
  >>= fun r ->
  cb r

let error_of_response res body =
  let status = Cohttp.Response.status res in
  let s = Cohttp.Code.string_of_status status in
  Cohttp_lwt_body.to_string body >>= fun body ->
  let f = Printf.sprintf "%s:%s" s body in
  return (Error (Failure f))

let upload {muse_root; ctx; _} ~file_id ~data =
  let uri = Uri.with_path muse_root "/encrypt" in
  let body = Ezjsonm.(
    ["file_id", string file_id; "data", string data]
    |> dict
    |> to_string) in
  let cb (resp, body) =
    if Cohttp.Response.status resp <> `OK then begin
      Log.warn (fun f -> f "upload %s failed" data);
      error_of_response resp body end
    else return (Ok ()) in
  post_req ~ctx uri body cb


let delegate {muse_root; ctx; _} ~file_id ~user_id =
  let uri = Uri.with_path muse_root "/delegate" in
  let body = Ezjsonm.(
    ["file_id", string file_id; "user_id", string user_id]
    |> dict
    |> to_string) in
  let cb (resp, body) =
    if Cohttp.Response.status resp <> `OK then begin
      Log.warn (fun f -> f "delegate %s to %s failed" file_id user_id);
      error_of_response resp body end
    else return (Ok ()) in
  post_req ~ctx uri body cb


let revoke {muse_root; ctx; _} ~file_id ~user_id =
  let uri = Uri.with_path muse_root "/revoke" in
  let body = Ezjsonm.(
    ["file_id", string file_id; "user_id", string user_id]
    |> dict
    |> to_string) in
  let cb (resp, body) =
    if Cohttp.Response.status resp <> `OK then begin
      Log.warn (fun f -> f "revoke %s from %s failed" file_id user_id);
      error_of_response resp body end
    else return (Ok ()) in
  post_req ~ctx uri body cb


let search {muse_root; ctx; _} ~word =
  let uri = Uri.with_path muse_root "/search" in
  let body = Ezjsonm.(
    ["word", string word]
    |> dict
    |> to_string) in
  let cb (resp, body) =
    if Cohttp.Response.status resp <> `OK then begin
      Log.warn (fun f -> f "search for word %s failed" word);
      return [] end
    else
      Cohttp_lwt_body.to_string body
      >>= fun body_str ->
      try
        match Ezjsonm.from_string body_str with
        | `A l -> return (List.map Ezjsonm.get_string l)
        | _ ->
          Log.err (fun f -> f "not expected json format: %s" body_str);
          return []
      with Ezjsonm.Parse_error (v,err) ->
        Log.err (fun f -> f "while parsing %s:\n%s" body_str err);
        return [] in
  post_req ~ctx uri body cb


let get_users {muse_root; ctx; _} =
  let uri = Uri.with_path muse_root "/users" in
  let cb (resp, body) =
    if Cohttp.Response.status resp <> `OK then begin
      let code = Cohttp.(Code.code_of_status (Response.status resp)) in
      Log.warn (fun f -> f "get users faild: %d" code);
      error_of_response resp body end
    else
      Cohttp_lwt_body.to_string body
      >>= fun body_str -> body_str
      |> Ezjsonm.from_string
      |> fun obj ->
         let users = Ezjsonm.get_dict obj |> List.assoc "users" in
         let l = Ezjsonm.(get_list get_string users) in
         return (Ok l) in
  get_req ~ctx uri cb


let register ~ctx ~root =
  let uri = Uri.with_path root "/setup" in
  let rec aux user_id =
    if user_id = None then
      let id = Uuidm.(create `V4 |> to_string) in
      let body = Ezjsonm.(
        ["user_id", string id]
        |> dict
        |> to_string) in
      let cb (resp, body) =
        if Cohttp.Response.status resp <> `OK then begin
            Log.err (fun f -> f "register failed: %s" id);
            return_none end
        else return_some id in
      post_req ~ctx uri body cb >>= fun user_id ->
      aux user_id
    else
      match user_id with
      | Some u -> return u
      | None -> assert false
  in
  aux None


let init ~ctx ~root =
  register ~ctx ~root >>= fun muse_id ->
  return {muse_id; muse_root = root; ctx}
