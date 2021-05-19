open Ezcurl
open Yojson
open Yojson.Basic.Util
open Http_common
open Orbit

type resultData = {
  id: int;
  version: int;
  name: string;
  timestamp: int
} [@@deriving show]

let forbidden_name_characters = ['\\'; '/'; ':'; '*'; '?'; '\"'; '<'; '>';];;

let from_body body =
  let json = Yojson.Basic.from_string body in

  let id = json |> member "id" |> to_int in
  let version = json |> member "version" |> to_int in
  let name = json |> member "name" |> to_string in
  let timestamp = json |> member "timestamp" |> to_int in
  {
    id = id;
    version = version;
    name = name;
    timestamp = timestamp;
  }

(* Checks if the filename is valid according to the API specification *)
let isNameValid (name: string): bool = 
  let escaped_name = String.escaped name in 
  let length = String.length escaped_name in
  if (length = 0) then false else 
  if (String.contains_from escaped_name (length-1) '.') then false else (*Does name end with a dot *)
  if (String.contains_from escaped_name (length-1) ' ') then false else (*Does name end with a whitespace *)
  if (String.rcontains_from escaped_name 0 ' ') then false else (* Does name begin with a whitespace *)

  let rec contains_illegal_chars (illegal_chars: char list): bool = 
    match illegal_chars with 
    | [] -> false
    | f::r -> if (String.contains escaped_name f) then true else contains_illegal_chars r
    in
  if (contains_illegal_chars forbidden_name_characters) then false else true
;;

let getExpectedResultBody (userId: int) (parentId: int) (fileTitle: string) (timestamp: int) (state: Orbit.system): resultData option = 
    let msTimestamp = (timestamp * 10000000 + 621355968000000000) in
    let id = (state.fileIdCounter + 1) in 
    let version = 1 in 
    let name = fileTitle in
    Some {
    id = id;
    version = version;
    name = name;
    timestamp = msTimestamp;
  }

let getExpectedResultHeaders  (userId: int) (parentId: int) (fileTitle: string) (timestamp: int) (state: Orbit.system): Http_common.response = 
  
  if((isNameValid fileTitle) = false) then Http_common.create_response Http_common.BadRequest else
  
  let dirOption: Orbit.directoryEntity option = Orbit.get_directory parentId state in
  match dirOption with 
  | None -> Http_common.create_response ~x_entity:(Some "Directory") Http_common.NotFound
  | Some dir -> 

    if (Orbit.has_crud_rights userId (Some parentId) state ) = false
    then Http_common.create_response ~x_entity:(Some "Directory") ~x_access_denied:(Some("Create")) Http_common.Unauthorized else

    if(Orbit.file_exists parentId fileTitle state) = true 
    then  Http_common.create_response ~x_conflict:(Some("Entity-Exists")) Http_common.Conflict else 

    Http_common.create_response ~content_type:(Some "application/json") Http_common.HttpOk
;;

let checkCreateFile (userId: int) (parentId: int) (fileTitle: string) (timestamp: int) (state: Orbit.system): bool =
  let url = Printf.sprintf ("http://localhost:8085/file?userId=%d&parentId=%d&name=%s&timestamp=%d") userId parentId fileTitle timestamp in
  match Ezcurl.post ~url: url ~params: [] () with
  | Ok resp -> (
    Orbit.matchResults 
      (fun _ -> getExpectedResultHeaders userId parentId fileTitle timestamp state) 
      (fun _ -> Http_common.map_response resp) 
      (fun _ -> getExpectedResultBody userId parentId fileTitle timestamp state) 
      (fun _ -> from_body resp.body)    
    )
  | Error (_) -> false
;;

(* Creates a file in a specified directory *)
let createFileUpdateState (state: Orbit.system ref) (userId: int) (parentId: int) (name: string) (timestamp: int): system ref = 
  if !Orbit.orbit_do_modification = false then state else

  let fileCounter = !state.fileIdCounter in
  let fileId = fileCounter + 1 in 
  let msTimestamp = (timestamp * 10000000 + 621355968000000000) in 
  let createdAt = Util.create_ISO_timestamp () in 
  let dir = get_directory parentId !state in 
  let path = 
      match dir with 
      |None -> "" 
      |Some dir -> dir.path
      in
  let newFile = {
      id = fileId;
      name = name;
      size = 0;
      mimetype = "application/octet-stream"; (*Not sure what to do here, there's mimetype for EVERY filetype. All of them. Do we really want to model that?*)
      parentId = parentId;
      version = 1;
      createdAt = createdAt;
      modifiedAt = createdAt;
      msTimestamp = msTimestamp; 
      path = path;
      snapshotsEnabled = false;
      content = "";
    } in
  let newFileList = newFile::!state.files in  
  let updateStateFileIdCounter = fileId in
  let updatedState = { 
    users = !state.users;
    directories = !state.directories;
    files = newFileList;
    directoryIdCounter = !state.directoryIdCounter;
    fileIdCounter=  updateStateFileIdCounter;
  } in
  Orbit.next_state_done updatedState