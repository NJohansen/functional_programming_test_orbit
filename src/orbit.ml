open Ezcurl

type user_rights = Bypass | ReadWrite | Write | None [@@deriving show]

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
  permissions: (directory_permissions * bool);
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
(* type (userEntity list, directoryEntity list, fileEntity list) t *)
(* type ('a, 'b, 'c) t *)
type 'a t

let initState = 
  {
    users = [];
    directories = [];
    files = []
  }

(* let get_file_list userId = *)
