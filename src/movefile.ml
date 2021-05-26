open Ezcurl
open Yojson
open Yojson.Basic.Util
open Http_common
open Util
open Orbit

type resultData = {
  id: int;
  version: int;
  name: string;
} [@@deriving show]

let from_body body =
  let json = Yojson.Basic.from_string body in

  let id = json |> member "id" |> to_int in
  let version = json |> member "version" |> to_int in
  let name = json |> member "name" |> to_string in
  {
    id = id;
    version = version;
    name = name;
  }

let getExpectedResultBody (fileId: int) (name: string) (state: Orbit.system): resultData option =
  let fileOption: Orbit.fileEntity option = Orbit.get_file fileId state in
  match fileOption with 
  | None -> None
  | Some file -> 
    Some {
      id = file.id;
      version = file.version + 1;
      name = name;
    }

let getExpectedResultHeaders (userId: int) (fileId: int) (version: int) (parentId: int) (name: string) (state: Orbit.system) : Http_common.response =
  if (Util.isNameValid name ) = false
  then Http_common.create_response Http_common.BadRequest else
  
  let fileOption: Orbit.fileEntity option = Orbit.get_file fileId state in
  match fileOption with 
  | None -> Http_common.create_response ~x_entity:(Some "File") Http_common.NotFound
  | Some file -> 

    if file.version != version 
    then Http_common.create_response ~x_conflict:(Some "File-Version") Http_common.Conflict else

    if (Orbit.has_crud_rights userId (Some file.parentId) state ) = false
    then Http_common.create_response ~x_entity:(Some "Parent") ~x_access_denied:(Some("Delete")) Http_common.Unauthorized else

    let parentOption: Orbit.directoryEntity option = Orbit.get_directory parentId state in
    match parentOption with 
    | None -> Http_common.create_response Http_common.BadRequest
    | Some parentDir -> 

      if (Orbit.has_crud_rights userId (Some parentId) state ) = false
      then Http_common.create_response ~x_entity:(Some "Parent") ~x_access_denied:(Some("Create")) Http_common.Unauthorized else

      Http_common.create_response ~content_type:(Some "application/json") Http_common.HttpOk

let checkMoveFile (userId: int) (fileId: int) (version: int) (parentId: int) (name: string) (timestamp: int) (state: Orbit.system) : bool =
  let url = Printf.sprintf "http://localhost:8085/file/move?userId=%d&id=%d&version=%d&parentId=%d&name=%s&timestamp=%d" userId fileId version parentId name timestamp in
  match Ezcurl.post ~url: url ~params: [] () with
  | Ok resp -> (
    Orbit.matchResults 
      (fun _ -> getExpectedResultHeaders userId fileId version parentId name state) 
      (fun _ -> Http_common.map_response resp) 
      (fun _ -> getExpectedResultBody fileId name state) 
      (fun _ -> from_body resp.body)
    )
  | Error (_) -> false

let moveFileUpdateState (userId: int) (fileId: int) (version: int) (parentId: int) (name: string) (timestamp: int) (state: Orbit.system ref) : Orbit.system ref =
  if !Orbit.orbit_do_modification = false then state else
  let _ = (Printf.printf "\n!!!!!!!!! Move file: %d  version: %d" fileId version; ()) in

  let fileOption: Orbit.fileEntity option = Orbit.get_file fileId !state in
  match fileOption with 
  | None -> state
  | Some file -> 
    let pathOption = Orbit.get_dir_path_with_root parentId !state in
      (match pathOption with 
      | None -> state
      | Some path ->
        let movedFile = {
          file with
          name = name;
          parentId = parentId;
          version = file.version + 1;
          path = path ^ name ^ "/";
        } in

        let rec deleteFile (del: int) (files: Orbit.fileEntity list) (newFiles: Orbit.fileEntity list) : Orbit.fileEntity list =
          (match files with
          | [] -> newFiles
          | f::rest when f.id = del -> newFiles @ rest
          | f::rest -> deleteFile del rest (newFiles @ [f])) in
        let newFileList = deleteFile fileId !state.files [] in        

        let newState = {
          !state with
          files = newFileList @ [movedFile];
        } in

        Orbit.next_state_done newState        
      )
;;