open Ezcurl
open Yojson
open Yojson.Basic.Util
open Http_common
open Orbit
open Util

type newVersionElement = {
  id: int;
  version: int;
} [@@deriving show]

type resultData = {
  name: string;
  id: int;
  version: int;
  parentId: int;
  newVersions: newVersionElement list;
} [@@deriving show]

let from_body body =
  let json = Yojson.Basic.from_string body in

  let name = json |> member "name" |> to_string in
  let id = int_of_string (json |> member "id" |> to_string) in
  let version = json |> member "version" |> to_int in
  let parentId = json |> member "parentId" |> to_int in
  let newVersionsJson = json |> member "newVersions" |> to_list in
  let newVersions = 
    List.map (fun s -> 
      {id = (member "id" s |> to_int); 
       version = (member "version" s |> to_int)}) newVersionsJson in
  {
    name = name;
    id = id;
    version = version;
    parentId = parentId;
    newVersions = newVersions;
  }

let getExpectedResultBody (parentId: int) (dirName: string) (dirVersion: int) (state: Orbit.system): resultData option = 
    let id = (state.directoryIdCounter + 1) in 
    Some {
      name = dirName;
      id = id;
      version = 1;
      parentId = parentId;
      newVersions = [];
    }

let getExpectedResultHeaders (userId: int) (parentId: int) (dirName: string) (version: int) (state: Orbit.system): Http_common.response = 

  if((Util.isNameValid dirName) = false) then Http_common.create_response Http_common.BadRequest else
  
  let dirOption: Orbit.directoryEntity option = Orbit.get_directory parentId state in
  match dirOption with 
  | None -> Http_common.create_response ~x_entity:(Some "Parent") Http_common.NotFound
  | Some dir -> 
    
    let _ = (Printf.printf " VERSION ---- %d - %d" version dir.version ; ()) in
    if version != dir.version 
    then  (Http_common.create_response ~x_conflict:(Some("Parent-Version")) Http_common.Conflict) else  

    if (Orbit.has_crud_rights userId (Some parentId) state ) = false
    then (Http_common.create_response ~x_entity:(Some "Directory") ~x_access_denied:(Some("Create")) Http_common.Unauthorized) else
    
    if(Orbit.dir_exists parentId dirName state) = true 
    then (Http_common.create_response ~x_conflict:(Some("Entity-Exists")) Http_common.Conflict) else 

    Http_common.create_response ~content_type:(Some "application/json") Http_common.HttpOk
;;

let checkCreateDir (userId: int) (parentId: int) (dirName: string) (dirVersion: int) (state: Orbit.system): bool =
  let url = Printf.sprintf ("http://localhost:8085/dir?userId=%d&parentId=%d&name=%s&version=%d") userId parentId dirName dirVersion in
  match Ezcurl.post ~url: url ~params: [] () with
  | Ok resp -> (
    Orbit.matchResults
      (fun _ -> getExpectedResultHeaders userId parentId dirName dirVersion state) 
      (fun _ -> Http_common.map_response resp) 
      (fun _ -> getExpectedResultBody parentId dirName dirVersion state) 
      (fun _ -> from_body resp.body)    
    )
  | Error (_) -> false
;;

(* Creates a dir in a specified directory *)
let createDirUpdateState (state: Orbit.system ref) (userId: int) (parentId: int) (name: string) (dirVersion: int): system ref = 
  if !Orbit.orbit_do_modification = false then state else

  let dirId = !state.directoryIdCounter + 1 in  
  let dir = get_directory parentId !state in 
  let path = 
      match dir with 
      | None -> "" 
      | Some dir -> dir.path
      in
  let newDir = 
  {
      id = dirId; 
      name = name; 
      path = Printf.sprintf ("%s%s/") path name; 
      version = 1; 
      permissions = [] ; 
      parent = Some(parentId); 
      }
  in
  let newDirList = newDir::!state.directories in  
  let updateStateDirIdCounter = dirId in
  let updatedState = { 
    !state with
    directories = newDirList;
    directoryIdCounter=  updateStateDirIdCounter;
  } in
  Orbit.next_state_done updatedState 