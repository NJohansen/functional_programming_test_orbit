open Ezcurl

type user_rights = Bypass | ReadWrite | ReadOnly | None [@@deriving show]

type userEntity = {
  id: int;
  rights: user_rights;
} [@@deriving show]

type directory_permissions = Crud | Read [@@deriving show]

type directoryEntity = {
  id: int;
  name: string;
  path: string;
  version: int;
  permissions: (user_rights * directory_permissions) list;
  parent: int;
  is_checked_out: bool;
  is_default: bool;
} [@@deriving show]

type fileEntity = {
  id: int;
  version: int;
  versionChanged: int;
  name: string;
  parentId: int;
  timestamp: string;
} [@@deriving show]

type system = {
  users: userEntity list;
  directories: directoryEntity list;
  files: fileEntity list;
} [@@deriving show]

(* let get_file_list userId = *)

let initState = 
  {
    users = [{id = 0; rights = Bypass}; {id = 100; rights = ReadWrite}; {id = 101; rights = ReadOnly}; {id = 102; rights = None}];
    directories = 
      [{id = 17; name = "Project 1"; path = "server-files/Projects/Project 1"; version = 1; permissions = [(ReadWrite, Crud); (ReadOnly, Read)]; parent = 2; is_checked_out = true; is_default = true;};
       {id = 18; name = "Project 2"; path = "server-files/Projects/Project 2"; version = 1; permissions = [(ReadWrite, Crud); (ReadOnly, Read)]; parent = 2; is_checked_out = true; is_default = true;};
       {id = 16; name = "ro"; path = "server-files/Users/ro"; version = 1; permissions = [(ReadOnly, Crud)]; parent = 14; is_checked_out = true; is_default = true;};
       {id = 15; name = "rw"; path = "server-files/Users/rw"; version = 1; permissions = [(ReadWrite, Crud)]; parent = 14; is_checked_out = true; is_default = true;};];
    files = [{id = 1; version = 1; versionChanged = 1; name = "filename"; parentId = 1; timestamp = "13"}];
  }

let get_user (userId: int) (state: system) : userEntity option =
  let rec find_user (id: int) (users: userEntity list) : userEntity option =
    match users with 
    | [] -> None
    | f::r 
      -> if f.id = id then Some(f) else find_user id r in
  find_user userId state.users

let get_list_directory (userId: int) (state: system) : directoryEntity list =
  let userOption = get_user userId state in
  match userOption with
  | None -> []
  | Some user ->

    let rec can_read_directory (userRight: user_rights) (permissions: (user_rights * directory_permissions) list) : bool =
      match userRight, permissions with
      | _, [] -> false
      | Bypass, (Bypass, _)::_ -> true
      | ReadWrite, (ReadWrite, _)::_ -> true
      | ReadOnly, (ReadOnly, _)::_ -> true
      | None, (None, _)::_ -> true
      | u, f::r -> can_read_directory u r in

    List.filter (fun d -> can_read_directory user.rights d.permissions) state.directories
;;

