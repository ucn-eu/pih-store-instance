open Result
module Client = Cohttp_mirage.Client

let return = Lwt.return
let (>>=) = Lwt.bind

type t = {
    c_domain : string;
    c_root     : Uri.t;
    ctx        : Cohttp_mirage.Client.ctx;
}

let create ~domain ~ctx ~root =
  return {c_domain = domain; c_root = root; ctx}

let to_id c id = Printf.sprintf "%s_%s" c id

let error_of_response status body =
  let s = Cohttp.Code.string_of_status status in
  Cohttp_lwt_body.to_string body >>= fun body ->
  let f = Printf.sprintf "%s:%s" s body in
  return (Error (Failure f))

let get_meta {c_domain; c_root; ctx} id =
  let uri = Uri.with_path c_root ("/meta/" ^ id) in
  Client.get ~ctx uri >>= fun (res, body) ->
  let status = Cohttp.Response.status res in
  if status = `OK then
    Cohttp_lwt_body.to_string body >>= fun data ->
    let file_id = to_id c_domain id in
    let meta = Ezjsonm.(dict [
      "file_id", string file_id;
      "data", string data]
      |> to_string) in
    return (Ok meta)
  else error_of_response status body


let list {c_root; ctx; _} =
  let uri = Uri.with_path c_root "/list" in
  Client.get ~ctx uri >>= fun (res, body) ->
  let status = Cohttp.Response.status res in
  if status = `OK then
    Cohttp_lwt_body.to_string body >>= fun str ->
    let lst = (Ezjsonm.from_string str :> Ezjsonm.value) in
    let lst = Ezjsonm.(get_list get_string lst) in
    return (Ok lst)
  else error_of_response status body
