type t

val create : domain:string -> ctx:Cohttp_mirage.Client.ctx -> root:Uri.t -> t Lwt.t

(* return a serialized json object, containing two fields: file_id, data  *)
val get_meta : t -> (string, exn) Result.result Lwt.t

val list : t -> (string list, exn) Result.result Lwt.t
