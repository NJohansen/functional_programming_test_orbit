open Ezcurl
open Yojson
open Yojson.Basic.Util
open Http_common
open Orbit

type resultData = {
  id: int;
  version: int;
  name: string;
  timestamp: string
} [@@deriving show]

let from_body body =
  let json = Yojson.Basic.from_string body in

  let id = json |> member "id" |> to_int in
  let version = json |> member "version" |> to_int in
  let name = json |> member "name" |> to_string in
  let timestamp = json |> member "timestamp" |> to_string in
  {
    id = id;
    version = version;
    name = name;
    timestamp = timestamp;
  }

let getExpectedResultData (userId: int) (parentId: int) (fileTitle: string) (timestamp: string) (state: Orbit.system): resultData =
  let updatedState = Orbit.create_file state userId parentId fileTitle timestamp in
  let newFile = Orbit.get_file updatedState.fileIdCounter updatedState in 
  let file: Orbit.fileEntity = match newFile with 
  | None -> {} (*I have no idea what to do in this case *)
  | Some file -> file 
  in
  let expectedFileId = file.id in 
  let expectedTimestamp = file.msTimestamp in
  let expectedFileVersion = file.version in 
  let expectedFileName = file.name in 
  { 
    id = expectedFileId;
    version = expectedFileVersion;
    name = expectedFileName;
    timestamp = expectedTimestamp;
  }


let matchBodyWithExpectedResult (bodyResult: resultData) (userId: int) (parentId: int) (fileTitle: string) (timestamp: string) (state: Orbit.system): bool =
  let expectedData = getExpectedResultData userId parentId fileTitle timestamp state in
  if expectedData = bodyResult then true else false

let checkCreateFile (userId: int) (parentId: int) (fileTitle: string) (timestamp: string) (state: Orbit.system): bool =
  let url = Printf.sprintf ("http://localhost:8085/file?userId=%d&parentId=%d&name=%s&timestamp=%s") userId parentId fileTitle timestamp in
  match Ezcurl.post ~url: url ~params: [] () with
  | Ok resp -> (
    match Http_common.map_response resp with
    | { status_code = HttpOk; _} -> 
      let bodyRes = from_body resp.body in
      matchBodyWithExpectedResult bodyRes userId parentId fileTitle timestamp state
    | _ -> false
    )
  | Error (_) -> false
;;
