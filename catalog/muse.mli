type t

val upload : t -> file_id:string -> data:string -> (unit, exn) Result.result Lwt.t

val delegate : t -> file_id:string -> user_id:string -> (unit, exn) Result.result Lwt.t

val revoke : t -> file_id:string -> user_id:string -> (unit, exn) Result.result Lwt.t

val get_users : t -> (string list, exn) Result.result Lwt.t

val init : ctx:Cohttp_mirage.Client.ctx -> root:Uri.t -> t Lwt.t
