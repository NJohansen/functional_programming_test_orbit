open Ezcurl
open Yojson
open Yojson.Basic.Util
open Http_common
open Util
open Orbit

type newVersionElement = {
  id: int;
  version: int;
} [@@deriving show]

type resultData = {
  success: bool;
  newVersions: newVersionElement list;
} [@@deriving show]

let from_body body =
  let json = Yojson.Basic.from_string body in

  let success = json |> member "success" |> to_bool in
  let newVersionsJson = json |> member "newVersions" |> to_list in
  let newVersions = 
    List.map (fun s -> 
      {id = (member "id" s |> to_int); 
       version = (member "version" s |> to_int)}) newVersionsJson in
  {
    success = success;
    newVersions = newVersions;
  }

let getExpectedResultBody (dirId: int) (state: Orbit.system) : resultData option=
  let dirOption: Orbit.directoryEntity option = Orbit.get_directory dirId state in
  match dirOption with 
  | None -> None
  | Some dir -> 
    (match dir.parent with
    | None -> None
    | Some parentId ->
      let pDirOption: Orbit.directoryEntity option = Orbit.get_directory parentId state in
      (match pDirOption with 
      | None -> None
      | Some parentDir -> Some { success = true; newVersions = [{id = parentId; version = parentDir.version + 1}]})
    )
;;

let getExpectedResultHeaders (userId: int) (dirId: int) (version: int) (state: Orbit.system) : Http_common.response =
  let dirOption: Orbit.directoryEntity option = Orbit.get_directory dirId state in
  match dirOption with 
  | None -> Http_common.create_response ~x_entity:(Some "Directory") Http_common.NotFound
  | Some dir -> 

    if dir.version != version 
    then Http_common.create_response ~x_conflict:(Some "Directory-Version") Http_common.Conflict else

    if (Orbit.has_crud_rights userId dir.parent state ) = false
    then Http_common.create_response ~x_entity:(Some "Directory") ~x_access_denied:(Some("Delete")) Http_common.Unauthorized else

    if (Orbit.is_empty_dir dirId state) = false 
    then Http_common.create_response ~x_conflict:(Some "Directory-Not-Empty") Http_common.Conflict else
  
    (* let filePath: string option = Orbit.get_file_path file.id state in  *)
    Http_common.create_response ~content_type:(Some "application/json") Http_common.HttpOk
;;

let matchResults (userId: int) (dirId: int) (version: int) (state: Orbit.system) (body: string) (requestResult: Http_common.response): bool =
  let expectedResultHeaders = getExpectedResultHeaders userId dirId version state in
  if (compare expectedResultHeaders requestResult) != 0 then false else

  if (requestResult.status_code != Http_common.HttpOk) then true else
  let body = from_body body in
  let expectedBodyOption = getExpectedResultBody dirId state in
  match expectedBodyOption with
  | None -> false
  | Some expectedBody -> if (compare expectedBody body) != 0 then false else true

let checkDeleteDirectory (userId: int) (dirId: int) (version: int) (state: Orbit.system) : bool =
  let url = Printf.sprintf "http://localhost:8085/dir?userId=%d&id=%d&version=%d" userId dirId version in
  match Ezcurl.http ~url: url ~meth: DELETE () with
  | Ok resp -> (
    let requestResult = Http_common.map_response resp in
    (matchResults userId dirId version state resp.body requestResult)
    )
  | Error (_) -> false
;;

let deleteDirectoryUpdateState (userId: int) (dirId: int) (version: int) (state: Orbit.system) : Orbit.system =
  let rec deleteDirectory (del: int) (directories: Orbit.directoryEntity list) (newDirs: Orbit.directoryEntity list) : Orbit.directoryEntity list =
    match directories with
    | [] -> newDirs
    | f::rest when f.id = del -> newDirs @ rest
    | f::rest -> deleteDirectory del rest (f::newDirs) in
  let newDirList = deleteDirectory dirId state.directories [] in

  let newUserList = List.map (
    fun user -> 
      let newCheckoutList = Util.delete_from_list dirId user.checkedOut [] in
      let newDefaultList = Util.delete_from_list dirId user.defaults [] in
      { user with checkedOut = newCheckoutList; defaults = newDefaultList}) state.users in

  let newState = {
    state with
    users = newUserList;
    directories = newDirList;
  } in

  begin Orbit.orbit_state := newState end; 
  newState
;;
(* DEref with ! *)