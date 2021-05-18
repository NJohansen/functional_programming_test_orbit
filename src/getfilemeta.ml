open Ezcurl
open Yojson
open Yojson.Basic.Util
open Http_common
open Orbit
open Util


type resultData = 
{
  id: int;
  name: string;
  parentId: int;
  version: int;
  versionChanged: int;
  timestamp: string;
} [@@deriving show]

let from_body body =
  let json = Yojson.Basic.from_string body in

  let id = json |> member "id" |> to_int in
  let name = json |> member "name" |> to_string in
  let parentId = json |> member "parentId" |> to_int in
  let version = json |> member "version" |> to_int in
  let versionChanged = json |> member "versionChanged" |> to_int in
  let timestamp = json |> member "timestamp" |> to_string in
  {
      id = id;
      name = name;
      parentId = parentId;
      version = version;
      versionChanged = versionChanged;
      timestamp = timestamp;
  }

let getExpectedResultData (userId: int) (fileId: int) (state: Orbit.system) : resultData =
    let fileOption: Orbit.fileEntity option = Orbit.get_file fileId state in
    match fileOption with 
    | None -> {
        id= 0;
        name= "0";
        parentId = 0;
        version = 0;
        versionChanged = 0;
        timestamp = "0";
    }
    | Some file -> 
        {
            id= file.id; 
            name = file.name; 
            parentId = file.parentId; 
            version = file.version; 
            versionChanged = 1; 
            timestamp = (string_of_int file.msTimestamp)
        }

(*let getExpectedResultData (userId: int) (fileId: int) (state: Orbit.system) : Http_common.response =
    let fileOption: Orbit.fileEntity option = Orbit.get_file fileId state in
    match fileOption with 
    | None -> Http_common.create_response ~x_entity:(Some "File") Http_common.NotFound
    | Some file -> 
        if (Orbit.can_read_file userId fileId state ) = false
        then Http_common.create_response ~x_entity:(Some "Parent") ~x_access_denied:(Some("Read")) Http_common.Unauthorized else

        {
            id= file.id; 
            name = file.name; 
            parentId = file.parentId; 
            version = file.version; 
            versionChanged = 1; 
            timestamp = (string_of_int file.msTimestamp)
        } *)


let matchBodyWithExpectedResult (bodyResult: resultData) (userId: int) (fileId: int) (state: Orbit.system) : bool =
  let expectedData = getExpectedResultData userId fileId state in

  if(compare expectedData bodyResult) != 0 then false else true

let checkFileMeta (userId: int) (fileId: int) (state: Orbit.system) =
    let url = Printf.sprintf "http://localhost:8085/file/meta?userId=%d&id=%d" userId fileId in
    match Ezcurl.get ~url: url () with
    | Ok resp -> (
        match map_response resp with
        | { status_code = HttpOk; _} -> 
        let bodyRes = from_body resp.body in
        matchBodyWithExpectedResult bodyRes userId fileId state
        | _ -> false
    )
    | Error (_) -> false
;;