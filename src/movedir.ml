open Ezcurl
open Yojson
open Yojson.Basic.Util
open Http_common
open Orbit
open Util

type newVersionElement = {
  id: int;
  version: int;
  dirId: int;
  dirVersion: int;
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

let getExpectedResultBody (dirId: int) (dirVersion: int) (state: Orbit.system) : resultData option =
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
      | Some parentDir -> Some { success = true; newVersions = [{id = parentId; version = parentDir.version + 1;}]})
    )
;;

let getExpectedResultHeader (userId: int) (dirId: int) (version: int) (state: Orbit.system) : Http_common.response =
    let dirOption: Orbit.directoryEntity option = Orbit.get_directory dirId state in
    match dirOption with 
    | None -> Http_common.create_response ~x_entity:(Some "Directory") Http_common.NotFound
    | Some dir -> 
        if dir.version != version 
        then Http_common.create_response ~x_conflict:(Some "Parent-Version") Http_common.Conflict else

        if (Orbit.has_crud_rights userId dir.parent state ) = false
        then Http_common.create_response ~x_entity:(Some "Directory") ~x_access_denied:(Some("Delete")) Http_common.Unauthorized else

        (*if (Orbit.is_empty_dir dirId state) = false (* Might not be needed *)
        then Http_common.create_response ~x_conflict:(Some "Directory-Not-Empty") Http_common.Conflict else *)
    
        Http_common.create_response ~content_type:(Some "application/json") Http_common.HttpOk
;;

let checkMoveDir (userId: int) (dirId: int) (dirVersion: int) (dirName: string) (parentDirId: int) (parentDirVersion: int) (state: Orbit.system) =
    let url = Printf.sprintf "http://localhost:8085/dir/move?userId=%d&id=%d&version=%d&name=%s&parentId=%d&parentVersion=%d" userId dirId dirVersion dirName parentDirId parentDirVersion in
    match Ezcurl.post ~url: url ~params: [] () with
    | Ok resp -> (
        Orbit.matchResults 
            (fun _ -> getExpectedResultHeader userId dirId dirVersion state) 
            (fun _ -> Http_common.map_response resp) 
            (fun _ -> getExpectedResultBody dirId dirVersion state) 
            (fun _ -> from_body resp.body)
    )
    | Error (_) -> false