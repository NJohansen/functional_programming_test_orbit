open Ezcurl

type user_rights = Bypass | ReadWrite | Read | None [@@deriving show]

type userEntity = {
  id: int;
  rights: user_rights;
} [@@deriving show]

type directory_permissions = Create | Read | Update | Delete [@@deriving show]

type directoryParent = {
  id: int;
} [@@deriving show]

type directoryEntity = {
  id: int;
  name: string;
  path: string;
  version: int;
  permissions: (directory_permissions * bool) list;
  parent: directoryParent;
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

let addUser (state: system) (userId: int) (permissions: user_rights) = 
  let user = {id = userId; rights = permissions} in
  {
    users = user::state.users;
    directories = state.directories;
    files = state.files;
  } 

let initState = 
  {
    users = [{id = 0; rights = Bypass}; {id = 100; rights = ReadWrite}; {id = 101; rights = Read}; {id = 102; rights = None}];
    directories = [{id = 15; name = "rw"; path = "server-files/Users/rw/"; version = 1; permissions = [(Create, true); (Read, true); (Update, true); (Delete, true)]; parent = {id = 14}; is_checked_out = true; is_default = true;}];      
    files = [{id = 1; version = 1; versionChanged = 1; name = "filename"; parentId = 1; timestamp = "13"}];
  }
;;
