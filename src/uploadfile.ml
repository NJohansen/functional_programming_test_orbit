open Ezcurl
open Yojson
open Yojson.Basic.Util
open Http_common
open Util
open Orbit

type resultData = 
{
  id: int;
  version: int;
  timestamp: int;
} [@@deriving show]

let from_body body =
  let json = Yojson.Basic.from_string body in

  let id = json |> member "id" |> to_int in
  let version = json |> member "version" |> to_int in
  let timestamp = json |> member "timestamp" |> to_int in
  {
      id = id;
      version = version;
      timestamp = timestamp;
  }

let getExpectedResultHeader (userId: int) (fileId:int) (fileVersion:int) (state: Orbit.system) = 
    let fileOption: Orbit.fileEntity option = Orbit.get_file fileId state in
    match fileOption with 
    | None -> Http_common.create_response ~x_entity:(Some "File") Http_common.NotFound
    | Some file -> 

        if (Orbit.has_crud_rights userId (Some file.parentId) state ) = false
        then Http_common.create_response ~x_entity:(Some "Parent") ~x_access_denied:(Some("Update")) Http_common.Unauthorized else

        if file.version != fileVersion 
        then Http_common.create_response ~x_conflict:(Some "File-Version") Http_common.Conflict else
     
        Http_common.create_response ~content_type:(Some "application/json") Http_common.HttpOk

let getExpectedResultBody (userId: int) (fileId:int) (fileVersion:int) (timestamp:int) (state: Orbit.system) : resultData option = 
    let fileOption: Orbit.fileEntity option = Orbit.get_file fileId state in
    match fileOption with 
    | None -> None
    | Some file -> Some
        {
            id = file.id; 
            version = file.version+1; 
            timestamp = (timestamp * 10000000 + 621355968000000000)    
        }

let checkFileUpload (userId: int) (fileId: int) (fileVersion:int) (timestamp:int)(state: Orbit.system) : bool = 
    let url = Printf.sprintf "http://localhost:8085/file/upload?userId=%d&id=%d&version=%d&timestamp=%d" userId fileId fileVersion timestamp in
    match Ezcurl.post ~url: url ~params: [] () with
    | Ok resp -> (
        Orbit.matchResults 
            (fun _ -> getExpectedResultHeader userId fileId fileVersion state) 
            (fun _ -> Http_common.map_response resp) 
            (fun _ -> getExpectedResultBody userId fileId fileVersion timestamp state) 
            (fun _ -> from_body resp.body)
            (*let expect = getExpectedResultBody userId fileId fileVersion timestamp state in
            let real = from_body resp.body in
            if (compare expect (Some real)) = 0 then true
            else false

            let expHead = getExpectedResultHeader userId fileId fileVersion state in
            let realHead = Http_common.map_response resp in
            if (compare expHead realHead) != 0 then false
            else true*)
    )
    | Error (_) -> false