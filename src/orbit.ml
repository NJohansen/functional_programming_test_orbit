open Ezcurl
open Util

type user_rights = Bypass | ReadWrite | ReadOnly | None [@@deriving show]

type userEntity = {
  id: int;
  rights: user_rights;
  checkedOut: int list;
  defaults: int list;
} [@@deriving show]

type directory_permissions = Crud | Read | None [@@deriving show]

type directoryEntity = {
  id: int;
  name: string;
  path: string;
  version: int;
  permissions: (user_rights * directory_permissions) list;
  parent: int option;
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
  directoryIdCounter: int;
  fileIdCounter: int;
} [@@deriving show]

(* let get_file_list userId = *)

let initState = 
  {
    users = 
      [{id = 0; rights = Bypass; checkedOut = []; defaults = []}; 
       {id = 100; rights = ReadWrite; checkedOut = [17; 18; 15]; defaults = [15]}; 
       {id = 101; rights = ReadOnly; checkedOut = [17; 18; 16]; defaults = [16]}; 
       {id = 102; rights = None; checkedOut = [19]; defaults = [19]}];
    directories = 
      [{id = 1; name = "server-files"; path = "server-files/"; version = 1; permissions = []; parent = None};
       {id = 10; name = "Companies"; path = "server-files/Companies/"; version = 1; permissions = []; parent = Some 1};
       {id = 13; name = "File importer"; path = "server-files/File importer/"; version = 1; permissions = []; parent = Some 1};
       {id = 21; name = "Delete me too"; path = "server-files/File importer/Delete me too/"; version = 1; permissions = []; parent = Some 13};
       {id = 3; name = "Project deliverables"; path = "server-files/Project deliverables/"; version = 1; permissions = []; parent = Some 1};
       {id = 8; name = "Project emails"; path = "server-files/Project emails/"; version = 1; permissions = []; parent = Some 1};
       {id = 2; name = "Projects"; path = "server-files/Projects/"; version = 1; permissions = []; parent = Some 1};
       {id = 17; name = "Project 1"; path = "server-files/Projects/Project 1/"; version = 1; permissions = [(ReadWrite, Crud); (ReadOnly, Read)]; parent = Some 2};
       {id = 18; name = "Project 2"; path = "server-files/Projects/Project 2/"; version = 1; permissions = [(ReadWrite, Crud); (ReadOnly, Read)]; parent = Some 2};
       {id = 4; name = "Project Templates"; path = "server-files/Project Templates/"; version = 1; permissions = []; parent = Some 1};
       {id = 5; name = "Standard - 1"; path = "server-files/Project Templates/Standard - 1/"; version = 1; permissions = []; parent = Some 1};
       {id = 6; name = "deliverables"; path = "server-files/Project Templates/Standard - 1/deliverables/"; version = 1; permissions = []; parent = Some 5};
       {id = 7; name = "explorer_root"; path = "server-files/Project Templates/Standard - 1/explorer_root/"; version = 1; permissions = []; parent = Some 5};
       {id = 12; name = "Sales Activities"; path = "server-files/Sales Activities/"; version = 1; permissions = []; parent = Some 1};
       {id = 9; name = "Shared files"; path = "server-files/Shared files/"; version = 1; permissions = [(ReadWrite, Crud); (ReadOnly, Read)]; parent = Some 1};
       {id = 20; name = "Delete me"; path = "server-files/Shared files/Delete me/"; version = 1; permissions = []; parent = Some 9};
       {id = 11; name = "snapshots"; path = "server-files/snapshots/"; version = 1; permissions = []; parent = Some 1};
       {id = 14; name = "Users"; path = "server-files/Users/"; version = 1; permissions = []; parent = Some 1};
       {id = 19; name = "none"; path = "server-files/Users/none/"; version = 1; permissions = [(None, Crud)]; parent = Some 14};
       {id = 16; name = "ro"; path = "server-files/Users/ro/"; version = 1; permissions = [(ReadOnly, Crud)]; parent = Some 14};
       {id = 15; name = "rw"; path = "server-files/Users/rw/"; version = 1; permissions = [(ReadWrite, Crud)]; parent = Some 14};];
    files = 
      [{id = 4; name = "INTRO.txt"; size = 184; mimetype = "text/plain"; parentId = 9; version = 1; createdAt = "2021-02-19T15:20:35.704Z"; modifiedAt = "2021-02-19T15:20:35.704Z"; msTimestamp = 637479675580000000; path = "server-files/Shared files/INTRO.txt"; snapshotsEnabled = false; };
       {id = 2; name = "README.txt"; size = 78; mimetype = "text/plain"; parentId = 15; version = 1; createdAt = "2021-02-19T15:20:35.704Z"; modifiedAt = "2021-02-19T15:20:35.704Z"; msTimestamp = 637479675580000000; path = "server-files/Users/rw/README.txt"; snapshotsEnabled = false; };
       {id = 3; name = "README.txt"; size = 78; mimetype = "text/plain"; parentId = 16; version = 1; createdAt = "2021-02-19T15:20:35.704Z"; modifiedAt = "2021-02-19T15:20:35.704Z"; msTimestamp = 637479675580000000; path = "server-files/Users/ro/README.txt"; snapshotsEnabled = false; }];
    directoryIdCounter = 21;
    fileIdCounter = 4;
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

    let rec can_read_directory (user: userEntity) (dirId: int) (permissions: (user_rights * directory_permissions) list) : bool =
      if Util.part_of_list user.checkedOut dirId == false
      then false else (
        match user.rights, permissions with
        | _, []-> false
        | Bypass, (Bypass, _)::_ -> true
        | ReadWrite, (ReadWrite, _)::_ -> true
        | ReadOnly, (ReadOnly, _)::_ -> true
        | None, (None, _)::_ -> true
        | _, f::r -> can_read_directory user dirId r) in

    List.filter (fun (d: directoryEntity) -> can_read_directory user d.id d.permissions) state.directories
;;

let get_list_directory_ignore_checkout (userId: int) (state: system) : directoryEntity list =
  let userOption = get_user userId state in
  match userOption with
  | None -> []
  | Some user ->

    let rec can_read_directory (userRight: user_rights) (permissions: (user_rights * directory_permissions) list) : bool =
        (match userRight, permissions with
        | _, []-> false
        | Bypass, (Bypass, _)::_ -> true
        | ReadWrite, (ReadWrite, _)::_ -> true
        | ReadOnly, (ReadOnly, _)::_ -> true
        | None, (None, _)::_ -> true
        | u, f::r -> can_read_directory u r ) in

    List.filter (fun d -> can_read_directory user.rights d.permissions) state.directories
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

(* Returns the list of directories that the user has write access to *)
let get_write_access_directories (userId: int) (state: system) : directoryEntity list =
  let userOption = get_user userId state in
  match userOption with
  | None -> []
  | Some user ->
    let rec can_write_directory (user: userEntity) (dirId: int) (permissions: (user_rights * directory_permissions) list) : bool =
      if Util.part_of_list user.checkedOut dirId == false
      then false else (
        match user.rights, permissions with
        | _, []-> false
        | Bypass, _::_ -> true
        | ReadWrite, (ReadWrite, _)::_ -> true
        | ReadOnly, (ReadOnly, _)::_ -> false
        | None, (None, _)::_ -> false
        | _, f::r -> can_write_directory user dirId r) in

    List.filter (fun (d: directoryEntity) -> can_write_directory user d.id d.permissions) state.directories
;;

let get_list_files_ignore_checkout (userId: int) (state: system) : fileEntity list =
  let availableDirectories = get_list_directory_ignore_checkout userId state in
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

let get_file_path (fileId: int) (state: system): string option =
  let fileOption: fileEntity option = get_file fileId state in
  match fileOption with 
  | None -> None
  | Some file -> 
    let rec create_path (patentId: int option) (append: string) (path: string): string =
      (match patentId with
      | None -> path
      | Some id -> 
        (match (get_directory id state) with 
        | None -> path
        | Some d -> 
          (if path = "" 
          then create_path d.parent d.name append
          else create_path d.parent d.name (append ^ "/" ^ path)))
      ) in
    Some (create_path (Some file.parentId) file.name "")
;;

let can_read_file (userId: int) (fileId: int) (state: system): bool =
  let userOption = get_user userId state in
  match userOption with
  | None -> false
  | Some user ->
    if user.rights = Bypass then true else
    let filesList = get_list_files_ignore_checkout userId state in
    let file = List.filter (fun f -> f.id = fileId) filesList in
    if List.length file = 1 then true else false
;;

(* Creates a file in a specified directory *)
(* We don't use the parameter timestamp, and it doesn't seem like Orbit does either? *)
let create_file (state: system) (userId: int) (parentId: int) (name: string) (timestamp: string): system = 
  let fileCounter = state.fileIdCounter in
  let fileId = fileCounter + 1 in 
  let createdAt = Util.create_ISO_timestamp () in 
  let dir = get_directory parentId state in 
  let path = 
      match dir with 
      |None -> "" 
      |Some dir -> dir.path
      in
  let newFile = {
      id = fileId;
      name = name;
      size = 0;
      mimetype = "text/plain"; (*Not sure what to do here, there's mimetype for EVERY filetype. All of them. Do we really want to model that?*)
      parentId = parentId;
      version = 1;
      createdAt = createdAt;
      modifiedAt = createdAt;
      msTimestamp = 637479675580000000; (* Seems to be what the Orbit API sets the msTimestamp to always *)
      path = path;
      snapshotsEnabled = false;
    } in
  let newFileList = newFile::state.files in  
  let updateStateFileIdCounter = fileId in
  let updatedState = { 
    users = state.users;
    directories = state.directories;
    files = newFileList;
    directoryIdCounter = state.directoryIdCounter;
    fileIdCounter=  updateStateFileIdCounter;
  } in
  updatedState 
;;