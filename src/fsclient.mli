type fsserverinfo
type userinfo

val make_fsserverinfo : string -> fsserverinfo
val make_userinfo : fsserverinfo -> user_id: int -> userinfo

val get_file : userinfo -> int -> (Fileget.info, Fileget.error) result Async_kernel.Derred.t