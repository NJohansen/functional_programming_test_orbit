open Ezcurl

type user_rights = Bypass | ReadWrite | ReadOnly | None

type userEntity = {
  id: int;
  rights: user_rights;
}

type directory_permissions = Crud | Read | None
type directoryEntity = {
  id: int;
  name: string;
  path: string;
  version: int;
  permissions: (user_rights * directory_permissions) list;
  parent: int option;
  is_checked_out: bool;
  is_default: bool;
}

type fileEntity = {
  id: int;
  name: string;
  size: int;
  mimetype: string;
  parentId: int;
  version: int;
  createdAt: string;
  modifiedAt: string;
  msTimestamp: int;
  path: string;
  snapshotsEnabled: bool;
}

type system = {
  users: userEntity list;
  directories: directoryEntity list;
  files: fileEntity list;
}

val initState : system

val get_user (userId: int) (state: system) : userEntity option
val get_list_directory (userId: int) (state: system) : directoryEntity list
val get_file (fileId: int) (state: system) : fileEntity option
val get_list_files (userId: int) (state: system) : fileEntity list
val get_directory (directoryId: int) (state: system) : directoryEntity option