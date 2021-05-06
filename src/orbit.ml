open Ezcurl

type user_rights = Bypass | ReadWrite | ReadOnly | None [@@deriving show]

type userEntity = {
  id: int;
  rights: user_rights;
} [@@deriving show]

type directory_permissions = Crud | Read | None [@@deriving show]

type directoryEntity = {
  id: int;
  name: string;
  path: string;
  version: int;
  permissions: (user_rights * directory_permissions) list;
  parent: int option;
  is_checked_out: bool;
  is_default: bool;
} [@@deriving show]

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
      [{id = 1; name = "server-files"; path = "server-files/"; version = 1; permissions = []; parent = None; is_checked_out = true; is_default = true;};
       {id = 10; name = "Companies"; path = "server-files/Companies/"; version = 1; permissions = []; parent = Some 1; is_checked_out = true; is_default = true;};
       {id = 13; name = "File importer"; path = "server-files/File importer/"; version = 1; permissions = []; parent = Some 1; is_checked_out = true; is_default = true;};
       {id = 21; name = "Delete me too"; path = "server-files/File importer/Delete me too/"; version = 1; permissions = []; parent = Some 13; is_checked_out = true; is_default = true;};
       {id = 3; name = "Project deliverables"; path = "server-files/Project deliverables/"; version = 1; permissions = []; parent = Some 1; is_checked_out = true; is_default = true;};
       {id = 8; name = "Project emails"; path = "server-files/Project emails/"; version = 1; permissions = []; parent = Some 1; is_checked_out = true; is_default = true;};
       {id = 2; name = "Projects"; path = "server-files/Projects/"; version = 1; permissions = []; parent = Some 1; is_checked_out = true; is_default = true;};
       {id = 17; name = "Project 1"; path = "server-files/Projects/Project 1/"; version = 1; permissions = [(ReadWrite, Crud); (ReadOnly, Read)]; parent = Some 2; is_checked_out = true; is_default = true;};
       {id = 18; name = "Project 2"; path = "server-files/Projects/Project 2/"; version = 1; permissions = [(ReadWrite, Crud); (ReadOnly, Read)]; parent = Some 2; is_checked_out = true; is_default = true;};
       {id = 4; name = "Project Templates"; path = "server-files/Project Templates/"; version = 1; permissions = []; parent = Some 1; is_checked_out = true; is_default = true;};
       {id = 5; name = "Standard - 1"; path = "server-files/Project Templates/Standard - 1/"; version = 1; permissions = []; parent = Some 1; is_checked_out = true; is_default = true;};
       {id = 6; name = "deliverables"; path = "server-files/Project Templates/Standard - 1/deliverables/"; version = 1; permissions = []; parent = Some 5; is_checked_out = true; is_default = true;};
       {id = 7; name = "explorer_root"; path = "server-files/Project Templates/Standard - 1/explorer_root/"; version = 1; permissions = []; parent = Some 5; is_checked_out = true; is_default = true;};
       {id = 12; name = "Sales Activities"; path = "server-files/Sales Activities/"; version = 1; permissions = []; parent = Some 1; is_checked_out = true; is_default = true;};
       {id = 9; name = "Shared files"; path = "server-files/Shared files/"; version = 1; permissions = [(ReadWrite, Crud); (ReadOnly, Read)]; parent = Some 1; is_checked_out = false; is_default = false;};
       {id = 20; name = "Delete me"; path = "server-files/Shared files/Delete me/"; version = 1; permissions = []; parent = Some 9; is_checked_out = true; is_default = true;};
       {id = 11; name = "snapshots"; path = "server-files/snapshots/"; version = 1; permissions = []; parent = Some 1; is_checked_out = true; is_default = true;};
       {id = 14; name = "Users"; path = "server-files/Users/"; version = 1; permissions = []; parent = Some 1; is_checked_out = true; is_default = true;};
       {id = 19; name = "none"; path = "server-files/Users/none/"; version = 1; permissions = [(None, Crud)]; parent = Some 14; is_checked_out = true; is_default = true;};
       {id = 16; name = "ro"; path = "server-files/Users/ro/"; version = 1; permissions = [(ReadOnly, Crud)]; parent = Some 14; is_checked_out = true; is_default = true;};
       {id = 15; name = "rw"; path = "server-files/Users/rw/"; version = 1; permissions = [(ReadWrite, Crud)]; parent = Some 14; is_checked_out = true; is_default = true;};];
    files = 
      [{id = 2; name = "INTRO.txt"; size = 184; mimetype = "text/plain"; parentId = 9; version = 1; createdAt = "2021-02-19T15:20:35.704Z"; modifiedAt = "2021-02-19T15:20:35.704Z"; msTimestamp = 637479675580000000; path = "server-files/Shared files/INTRO.txt"; snapshotsEnabled = false; };
       {id = 2; name = "README.txt"; size = 78; mimetype = "text/plain"; parentId = 15; version = 1; createdAt = "2021-02-19T15:20:35.704Z"; modifiedAt = "2021-02-19T15:20:35.704Z"; msTimestamp = 637479675580000000; path = "server-files/Users/rw/README.txt"; snapshotsEnabled = false; };
       {id = 3; name = "README.txt"; size = 78; mimetype = "text/plain"; parentId = 16; version = 1; createdAt = "2021-02-19T15:20:35.704Z"; modifiedAt = "2021-02-19T15:20:35.704Z"; msTimestamp = 637479675580000000; path = "server-files/Users/ro/README.txt"; snapshotsEnabled = false; }];
  }

let get_user (userId: int) (state: system) : userEntity option =
  let rec find_user (id: int) (users: userEntity list) : userEntity option =
    match users with 
    | [] -> None
    | f::r 
      -> if f.id = id then Some(f) else find_user id r in
  find_user userId state.users
;;  

(**WIP *)
let rec create_user user_list i =
  match user_list with
    [] -> [i]
  | h :: t -> h :: (create_user t i)
;;

let get_list_directory (userId: int) (state: system) : directoryEntity list =
  let userOption = get_user userId state in
  match userOption with
  | None -> []
  | Some user ->

    let rec can_read_directory (userRight: user_rights) (permissions: (user_rights * directory_permissions) list) (isCheckedOut: bool) : bool =
      if isCheckedOut == false
      then false else (
        match userRight, permissions with
        | _, []-> false
        | Bypass, (Bypass, _)::_ -> true
        | ReadWrite, (ReadWrite, _)::_ -> true
        | ReadOnly, (ReadOnly, _)::_ -> true
        | None, (None, _)::_ -> true
        | u, f::r -> can_read_directory u r true) in

    List.filter (fun d -> can_read_directory user.rights d.permissions d.is_checked_out) state.directories
;;

let get_file (fileId: int) (state: system) : fileEntity option =
  let rec find_file (id: int) (files: fileEntity list) : fileEntity option =
    match files with 
    | [] -> None
    | f::r 
      -> if f.id = id then Some(f) else find_file id r in
  find_file fileId state.files
;;

let get_list_files (userId: int) (state: system) : fileEntity list =
  let availableDirectories = get_list_directory userId state in
  match availableDirectories with
  | [] -> []
  | list -> 
      
      let rec match_parent_id (dirList: directoryEntity list) (fileList: fileEntity list) : fileEntity list = 
      match dirList with 
      | [] -> fileList
      | head::tail -> let files = List.filter(fun d -> head.id = d.parentId ) state.files in  
      match_parent_id tail fileList@files in
    
    match_parent_id list []
;;

let get_directory (directoryId: int) (state: system) : directoryEntity option =
  let rec find_directory (id: int) (directorys: directoryEntity list) : directoryEntity option =
    match directorys with 
    | [] -> None
    | f::r 
      -> if f.id = id then Some(f) else find_directory id r in
  find_directory directoryId state.directories
;;