open Util

type user_rights = Bypass | ReadWrite | ReadOnly | NoRights [@@deriving show]

type userEntity = {
  id: int;
  rights: user_rights;
  checkedOut: int list;
  defaults: int list;
} [@@deriving show]

type directory_permissions = Crud | Read | NoPermission [@@deriving show]

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
  content: string;
} [@@deriving show]

type system = {
  users: userEntity list;
  directories: directoryEntity list;
  files: fileEntity list;
} [@@deriving show]

(* let get_file_list userId = *)
let initState = 
  {
    users = 
      [{id = 0; rights = Bypass; checkedOut = []; defaults = []}; 
       {id = 100; rights = ReadWrite; checkedOut = [17; 18; 15]; defaults = [15]}; 
       {id = 101; rights = ReadOnly; checkedOut = [17; 18; 16]; defaults = [16]}; 
       {id = 102; rights = NoRights; checkedOut = [19]; defaults = [19]}];
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
       {id = 5; name = "Standard - 1"; path = "server-files/Project Templates/Standard - 1/"; version = 1; permissions = []; parent = Some 4};
       {id = 6; name = "deliverables"; path = "server-files/Project Templates/Standard - 1/deliverables/"; version = 1; permissions = []; parent = Some 5};
       {id = 7; name = "explorer_root"; path = "server-files/Project Templates/Standard - 1/explorer_root/"; version = 1; permissions = []; parent = Some 5};
       {id = 12; name = "Sales Activities"; path = "server-files/Sales Activities/"; version = 1; permissions = []; parent = Some 1};
       {id = 9; name = "Shared files"; path = "server-files/Shared files/"; version = 1; permissions = [(ReadWrite, Crud); (ReadOnly, Read)]; parent = Some 1};
       {id = 20; name = "Delete me"; path = "server-files/Shared files/Delete me/"; version = 1; permissions = []; parent = Some 9};
       {id = 11; name = "snapshots"; path = "server-files/snapshots/"; version = 1; permissions = []; parent = Some 1};
       {id = 14; name = "Users"; path = "server-files/Users/"; version = 1; permissions = []; parent = Some 1};
       {id = 19; name = "none"; path = "server-files/Users/none/"; version = 1; permissions = [(NoRights, Crud)]; parent = Some 14};
       {id = 16; name = "ro"; path = "server-files/Users/ro/"; version = 1; permissions = [(ReadOnly, Crud)]; parent = Some 14};
       {id = 15; name = "rw"; path = "server-files/Users/rw/"; version = 1; permissions = [(ReadWrite, Crud)]; parent = Some 14};];
    files = 
      [{id = 4; name = "INTRO.txt"; size = 184; mimetype = "text/plain"; parentId = 9; version = 1; createdAt = "2021-02-19T15:20:35.704Z"; modifiedAt = "2021-02-19T15:20:35.704Z"; msTimestamp = 637479675580000000; path = "server-files/Shared files/INTRO.txt"; snapshotsEnabled = false; content = "INTRO.txt located at /Users/Shared files/INTRO.txt\n USER_ID=100 (rw) can read and write content to the file, but USER_ID=101 (ro) can only read it. USER_ID=102 (none) has no access it."};
       {id = 2; name = "README.txt"; size = 78; mimetype = "text/plain"; parentId = 15; version = 1; createdAt = "2021-02-19T15:20:35.704Z"; modifiedAt = "2021-02-19T15:20:35.704Z"; msTimestamp = 637479675580000000; path = "server-files/Users/rw/README.txt"; snapshotsEnabled = false; content = "README.txt located at /Users/rw/README.txt\nOnly USER_ID=100 can access it.\n"};
       {id = 3; name = "README.txt"; size = 78; mimetype = "text/plain"; parentId = 16; version = 1; createdAt = "2021-02-19T15:20:35.704Z"; modifiedAt = "2021-02-19T15:20:35.704Z"; msTimestamp = 637479675580000000; path = "server-files/Users/ro/README.txt"; snapshotsEnabled = false; content = "README.txt located at /Users/ro/README.txt\nOnly USER_ID=101 can access it.\n"}];
  }

let orbit_state: system ref = ref initState
let orbit_do_modification: bool ref = ref false

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
        | NoRights, (NoRights, _)::_ -> true
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
        | NoRights, (NoRights, _)::_ -> true
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

let can_read_directory (userId: int) (dirId: int) (state: system): bool =
  let userOption = get_user userId state in
  match userOption with
  | None -> false
  | Some user ->
    if user.rights = Bypass then true 
    
    else if (user.rights = ReadOnly && dirId = 20)  || (user.rights = ReadWrite && dirId = 20) then true
    
    else
    let findChilds parentId = List.filter (fun child -> child.parent = (Some parentId)) state.directories in

    let read_rights directory = List.length (List.filter (fun rights -> (rights = (user.rights, Crud)) || (rights = (user.rights, Read))) directory.permissions) > 0 in

    let rec check_for_read_rights (dir: directoryEntity) (childs: directoryEntity list) : bool =
      if read_rights dir then true else
      List.length (
        List.filter (fun (child: directoryEntity) ->
          if read_rights child then true else
          (match findChilds child.id with
          | [] -> false
          | f::r ->
            if read_rights f
            then true
            else check_for_read_rights f (findChilds f.id))
      ) childs) > 0 in

    let dirOption = get_directory dirId state in
    (match dirOption with
    | None -> false
    | Some dir -> check_for_read_rights dir (findChilds dirId)
    )
;;

let has_crud_rights (userId: int) (parentDirIdOption: int option) (state: system) : bool =
  match parentDirIdOption with
  | None -> false
  | Some parentDirId ->
    let userOption = get_user userId state in
    let dirOption = get_directory parentDirId state in
    (match userOption, dirOption with
    | None, None | None, _ | _, None -> false
    | Some user, Some dir ->

      let rec crud_directory (userRight: user_rights) (permissions: (user_rights * directory_permissions) list) : bool =
        if user.rights = Bypass then true else
        (match userRight, permissions with
        | _, []-> false
        | Bypass, _ -> true
        | ReadWrite, (ReadWrite, Crud)::_ -> true
        | ReadOnly, (ReadOnly, Crud)::_ -> true
        | NoRights, (NoRights, Crud)::_ -> true
        | u, f::r -> crud_directory u r ) in

      if crud_directory user.rights dir.permissions then true else false)
;;

let is_empty_dir (dirId: int) (state: system) : bool =
  let rec checkDirs (dirs: directoryEntity list) : bool =
    match dirs with
    | [] -> true
    | f::r ->
      if f.parent = Some dirId 
      then false
      else checkDirs r in
  if checkDirs state.directories = false then false else

  let rec checkFiles (files: fileEntity list) : bool = 
    match files with
    | [] -> true
    | f::r ->
      if f.parentId = dirId 
      then false
      else checkFiles r in
  if checkFiles state.files = false then false else true
;;

let printStateStatus s =
  let _ = (Printf.printf "\n!!!!!!!!!!!!!!!! Users: %d - Dirs: %d - Files: %d\n" (List.length !orbit_state.users) (List.length !orbit_state.directories) (List.length !orbit_state.files); ()) in
  ()

let matchResults (expectedResult: unit -> Http_common.response) (requestResult: unit -> Http_common.response) (expectedBody: unit -> 'a option) (requestBody: unit -> 'a) : bool =
  let expectedResult = expectedResult () in
  let _ = (Printf.printf "Expected: %d" (Http_common.status_code_to_int expectedResult.status_code); ()) in
  let requestResult = requestResult () in
  let _ = (Printf.printf " - Actual: %d\n" (Http_common.status_code_to_int requestResult.status_code); ()) in
  if (compare expectedResult requestResult) != 0 
  then (printStateStatus (); begin orbit_do_modification := false end; false ) else

  if (requestResult.status_code != Http_common.HttpOk) 
  then (begin orbit_do_modification := false end; true ) else

  let expectedBody = expectedBody () in
  let requestBody = requestBody () in
  match expectedBody with
  | None -> (printStateStatus (); begin orbit_do_modification := false end; false )
  | Some expectedBody -> if (compare expectedBody requestBody) != 0 
    then (printStateStatus (); begin orbit_do_modification := false end; false )
    else (Printf.printf "\n!!!!!!!!!!!!!!!! CAN CHANGE "; printStateStatus ();  begin orbit_do_modification := true end; true )
;;

let next_state_done (newState: system) : system ref =
    begin orbit_do_modification := false end; 
    begin orbit_state := newState end; 
    orbit_state