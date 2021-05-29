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

        if file.version != fileVersion 
        then Http_common.create_response ~x_conflict:(Some "File-Version") Http_common.Conflict else
        
        if (Orbit.has_crud_rights userId (Some file.parentId) state ) = false
        then Http_common.create_response ~x_entity:(Some "Parent") ~x_access_denied:(Some("Update")) Http_common.Unauthorized else

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

let checkFileUpload (userId: int) (fileId: int) (fileVersion:int) (timestamp:int) (state: Orbit.system) : bool = 
    let url = Printf.sprintf "http://localhost:8085/file/upload?userId=%d&id=%d&version=%d&timestamp=%d" userId fileId fileVersion timestamp in
    (* let curlContent: Curl.curlHTTPPost = Curl.CURLFORM_CONTENT ("TEST1", "TEST2", Curl.CONTENTTYPE "text/plain") in *)
    match Ezcurl.post ~url: url ~params: [] () with
    | Ok resp -> (
        Orbit.matchResults 
            (fun _ -> getExpectedResultHeader userId fileId fileVersion state) 
            (fun _ -> Http_common.map_response resp) 
            (fun _ -> getExpectedResultBody userId fileId fileVersion timestamp state) 
            (fun _ -> from_body resp.body)
    )
    | Error (_) -> false

let uploadFileUpdateState (state: Orbit.system ref) (userId: int) (fileId: int) (fileVersion:int) (timestamp: int): system ref = 
    if !Orbit.orbit_do_modification = false then state else 
    let _ = (Printf.printf "\n!!!!!! UPLOAD FILE: %d" fileId; ()) in

    let fileOption = Orbit.get_file fileId !state in
    let msTimestamp = (timestamp * 10000000 + 621355968000000000) in
    let modify = Util.create_ISO_timestamp () in
    match fileOption with 
        | None -> state
        | Some file ->
            let newFile = {
                id = fileId;
                name = file.name;
                size = 0;
                mimetype = "text/plain";
                parentId = file.parentId;
                version = file.version+1;
                createdAt = file.createdAt;
                modifiedAt = modify;
                msTimestamp = msTimestamp;
                path = file.path;
                snapshotsEnabled = false;
                content = "";
                versionChanged = file.version+1;
            } in
            let rec fileToUpload (fileId: int) (files: Orbit.fileEntity list) (newFiles: Orbit.fileEntity list): Orbit.fileEntity list = 
                (match files with
                    | [] -> newFiles
                    | f::rest when f.id = fileId -> newFiles @ (newFile::rest)
                    | f::rest -> fileToUpload fileId rest (newFiles @ [f])) in
            let newFileList = fileToUpload fileId !state.files [] in
    
            let newState = {
                !state with
                files = newFileList;
            } in 

            Orbit.next_state_done newState
;;



    
    
