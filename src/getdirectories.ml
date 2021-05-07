open Ezcurl
open Yojson
open Yojson.Basic.Util
open Http_common
open Orbit
open Util

type directory_permissions = {
    create: bool;
    read: bool;
    update: bool;
    delete: bool;
    } [@@deriving show]

type directoryElement = {
  id: int;
  name: string;
  path: string;
  version: int;
  __permissions: directory_permissions option;
  parent: int option; (**Maybe a problem? *)
  is_checked_out: bool;
  is_default: bool;
} [@@deriving show]

let from_body body =
  let json = Yojson.Basic.from_string body in

  let parentObject =  json |> member "parent" |> to_assoc in 
  match List.hd parentObject with 
  | (_,b) -> let parentId = (to_int b) in
  
  (**WORK IN PROGRESS *)
  let permissionObject = json |> member "__permissions" |> to_assoc in 
  match List.hd permissionObject with 
  | (a,b) -> Printf.printf "%s %s" (a) (to_string b);  

  (* {id = (json |> member "id" |> to_int); 
  name = (json |> member "name" |> to_string);
  path = (json |> member "path" |> to_string); 
  version = (json |> member "version" |> to_int);
  __permissions = (json |> member "__permissions" |> to_assoc);
  parent = parentId;
  is_checked_out = (json |> member "is_checked_out" |> to_bool);
  is_default = (json |> member "is_default" |> to_int);
  parent = (json |> member "timestamp" |> to_string)} *)
;;

(* let getExpectedResultData (userId: int) (state: Orbit.system) : directoryElement =
  let expectedDirectories: Orbit.directoryEntity list = Orbit.get_list_directory userId state in
  let expectedDirectoryVersionsElements: directoryVersionsElement list = 
    List.map (fun (dir: Orbit.directoryEntity) -> {id = dir.id; version = dir.version}) expectedDirectories in
  
  let expectedFiles: Orbit.fileEntity list = Orbit.get_list_files userId state in
  let expectedFileListElements: fileListElement list =
    List.map (fun (file: Orbit.fileEntity) -> 
      {id = file.id; name = file.name; parentId = file.parentId; version = file.version; versionChanged = 1; timestamp = (string_of_int file.msTimestamp)}
      ) expectedFiles in

  {
    directoryVersions = expectedDirectoryVersionsElements;
    fileList = expectedFileListElements;
  }
;; *)
(* let matchBodyWithExpectedResult (bodyResult: resultData) (userId: int) (state: Orbit.system) : bool =
  let expectedData = getExpectedResultData userId state in

  let checkDirectories = Util.all_present expectedData.directoryVersions bodyResult.directoryVersions in
  let checkFiles = Util.all_present expectedData.fileList bodyResult.fileList in

  if checkDirectories && checkFiles then true else false *)

let checkGetDirectory (userId: int) (dirId: int) (state: Orbit.system): 'a option =
  let url = "http://localhost:8085/api/directories?userId=" ^ (string_of_int userId) ^ "&id=" ^ (string_of_int dirId) in
  match Ezcurl.get ~url: url () with
  | Ok resp -> (
    match map_response resp with
    | { status_code = HttpOk; _} -> 
      let bodyRes = from_body resp.body in
      (* matchBodyWithExpectedResult bodyRes userId state *)
      Some(bodyRes)
    | _ -> None
    )
  | Error (_) -> None
;;