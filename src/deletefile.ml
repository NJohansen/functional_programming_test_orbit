open Ezcurl
open Yojson
open Yojson.Basic.Util
open Http_common
open Orbit

type resultData = {
  success: bool;
} [@@deriving show]

let from_body body =
  let json = Yojson.Basic.from_string body in

  let success = json |> member "success" |> to_bool in

  {
    success = success;
  }

let getExpectedResultBody (fileId: int) (state: Orbit.system) : resultData option=
  let fileOption: Orbit.fileEntity option = Orbit.get_file fileId state in
  match fileOption with 
  | None -> None
  | Some file -> 
    let pDirOption: Orbit.directoryEntity option = Orbit.get_directory file.parentId state in
    (match pDirOption with 
    | None -> None
    | Some parentDir -> Some { success = true })
;;

let getExpectedResultHeaders (userId: int) (fileId: int) (version: int) (state: Orbit.system) : Http_common.response =
  let fileOption: Orbit.fileEntity option = Orbit.get_file fileId state in
  match fileOption with 
  | None -> Http_common.create_response ~x_entity:(Some "File") Http_common.NotFound
  | Some file -> 

    if file.version != version 
    then Http_common.create_response ~x_conflict:(Some "File-Version") Http_common.Conflict else

    if (Orbit.has_crud_rights userId (Some file.parentId) state ) = false
    then Http_common.create_response ~x_entity:(Some "Directory") ~x_access_denied:(Some("Delete")) Http_common.Unauthorized else
  
    Http_common.create_response ~content_type:(Some "application/json") Http_common.HttpOk
;;

let checkDeleteFile (userId: int) (fileId: int) (version: int) (state: Orbit.system) : bool =
  let url = Printf.sprintf "http://localhost:8085/file?userId=%d&id=%d&version=%d" userId fileId version in
  match Ezcurl.http ~url: url ~meth: DELETE () with
  | Ok resp -> (
    Orbit.matchResults 
      (fun _ -> getExpectedResultHeaders userId fileId version state) 
      (fun _ -> Http_common.map_response resp) 
      (fun _ -> getExpectedResultBody fileId state) 
      (fun _ -> from_body resp.body)    
    )
  | Error (_) -> false

let deleteFileUpdateState (userId: int) (fileId: int) (version: int) (state: Orbit.system ref) : Orbit.system ref =
  if !Orbit.orbit_do_modification = false then state else
  let _ = (Printf.printf "\n!!!!!!!!! DELETE FILE: %d  version: %d" fileId version; ()) in

  let fileToDeleteOption = Orbit.get_file fileId !state in
  match fileToDeleteOption with
  | None -> state
  | Some fileToDelete -> 
    let rec deleteFile (del: int) (files: Orbit.fileEntity list) (newFiles: Orbit.fileEntity list) : Orbit.fileEntity list =
      (match files with
      | [] -> newFiles
      | f::rest when f.id = del -> newFiles @ rest
      | f::rest -> deleteFile del rest (newFiles @ [f])) in
    let newFileList = deleteFile fileId !state.files [] in

    let newState = {
      !state with
      files = newFileList;
    } in

    Orbit.next_state_done newState
;;