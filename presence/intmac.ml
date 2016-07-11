module type E = sig
  val get_uri : unit -> string
  val get_headers: unit -> (string * string) list
end

module Endpoint : E = Req_endpoint
module Client = Cohttp_mirage.Client

let (>>=) = Lwt.bind
let return = Lwt.return

let req ctx =
  let uri = Endpoint.get_uri () |> Uri.of_string in
  let hds = Endpoint.get_headers () in
  let headers = Cohttp.Header.(add_list (init ()) hds) in
  Client.get ~ctx ~headers uri


let decode_json str = Ezjsonm.(
  let raw = from_string str  in
  let f = get_list get_dict in
  let l = get_list f raw |> List.flatten in
  return l
)

let filter_event l =
  let f obj =
    let alarm_no = List.assoc "AlarmNo" obj |> Ezjsonm.get_string in
    let desp = List.assoc "AlarmZoneDescription" obj |> Ezjsonm.get_string in
    let typ = List.assoc "AlarmType" obj |> Ezjsonm.get_string in
    let epoch = List.assoc "EventDateTime" obj |> Ezjsonm.get_string in
    if 0 <> int_of_string alarm_no && desp = "Test Camera"
    then Some (typ, epoch)
    else None in

  (*select the latest time*)
  let date_select e1 e2 = match e1, e2 with
    | None, (_ as r) | (_ as r), None -> r
    | Some (ty1, e1), Some (ty2, e2) ->
       let ep_of_str s =
         let re = Re_str.regexp "[0-9]+[\\+\\-][0-9]+" in
         let _ = Re_str.search_forward re s in
         Re_str.matched_string s in
       if compare e1 e2 >= 0 then Some (ty1, ep_of_str e1)
       else Some (ty2, ep_of_str e2) in
  List.fold_left (fun acc elm -> date_select acc (f elm)) None l
  |> return


let pull ctx fn =
  req ctx >>= fun (res, body) ->
  let status = Cohttp.Response.status res in
  let code = Cohttp.Code.code_of_status status in
  if code >= 400 then return (fn None) else
    Cohttp_lwt_body.to_string body
    >>= decode_json
    >>= filter_event
    >>= fun ev -> return (fn ev)

let polling ctx fn p =
  let rec loop () =
  OS.Time.sleep p >>= fun () ->
  pull ctx fn >>= fun () ->
  loop () in
  loop ()
