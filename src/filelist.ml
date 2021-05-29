open Ezcurl
open Yojson
open Yojson.Basic.Util
open Http_common
open Orbit
open Util

type directoryVersionsElement = {
  id: int;
  version: int;
} [@@deriving show]

type fileListElement = {
  id: int;
  name: string;
  parentId: int;
  version: int;
  versionChanged: int;
  timestamp: string;
} [@@deriving show]

type resultData = {
  directoryVersions: directoryVersionsElement list;
  fileList: fileListElement list;
} [@@deriving show]

let map_directoryElement data = 
  List.map (fun s -> 
  {id = (member "id" s |> to_int); 
  version = (member "version" s |> to_int)}) data

let map_fileElements data =
  List.map (fun s -> 
  {id = (member "id" s |> to_int); 
  name = (member "name" s |> to_string);
  parentId = (member "parentId" s |> to_int); 
  version = (member "version" s |> to_int);
  versionChanged = (member "versionChanged" s |> to_int);
  timestamp = (member "timestamp" s |> to_string)}) data


let from_body body =
  let json = Yojson.Basic.from_string body in

  let directories = json |> member "directoryVersions" |> to_list in
  let directoryVersions = map_directoryElement directories in

  let files = json |> member "fileList" |> to_list in
  let fileList = map_fileElements files in
  {
    directoryVersions = directoryVersions;
    fileList = fileList;
  }

let getExpectedResultData (userId: int) (state: Orbit.system) : resultData =
  let expectedDirectories: Orbit.directoryEntity list = Orbit.get_list_directory userId state in
  let expectedDirectoryVersionsElements: directoryVersionsElement list = 
    List.map (fun (dir: Orbit.directoryEntity) -> {id = dir.id; version = dir.version}) expectedDirectories in
  
  let expectedFiles: Orbit.fileEntity list = Orbit.get_list_files userId state in
  let expectedFileListElements: fileListElement list =
    List.map (fun (file: Orbit.fileEntity) ->
      {id = file.id; name = file.name; parentId = file.parentId; version = file.version; versionChanged = file.versionChanged; timestamp = (string_of_int file.msTimestamp)}
      ) expectedFiles in

  {
    directoryVersions = expectedDirectoryVersionsElements;
    fileList = expectedFileListElements;
  }

let matchBodyWithExpectedResult (bodyResult: resultData) (userId: int) (state: Orbit.system) : bool =
  let expectedData = getExpectedResultData userId state in

  let checkDirectories = Util.all_present expectedData.directoryVersions bodyResult.directoryVersions in
  let checkFiles = Util.all_present expectedData.fileList bodyResult.fileList in

  if checkDirectories && checkFiles then true else false

let checkGetListOfFiles (userId: int) (state: Orbit.system): bool =
  let url = "http://localhost:8085/file/list?userId=" ^ (string_of_int userId) in
  match Ezcurl.get ~url: url () with
  | Ok resp -> (
    match map_response resp with
    | { status_code = HttpOk; _} -> 
      let bodyRes = from_body resp.body in
      matchBodyWithExpectedResult bodyRes userId state
    | _ -> false
    )
  | Error (_) -> false
;;