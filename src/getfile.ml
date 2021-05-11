open Ezcurl
open Yojson
open Yojson.Basic.Util
open Http_common
open Orbit
open Util

let getExpectedResult (userId: int) (fileId: int) (state: Orbit.system) : Http_common.response =
  let fileOption: Orbit.fileEntity option = Orbit.get_file fileId state in
  match fileOption with 
  | None -> Http_common.create_response ~x_entity:(Some "File") Http_common.NotFound
  | Some file -> 

    if (Orbit.can_read_file userId fileId state ) = false
    then Http_common.create_response ~x_entity:(Some "Parent") ~x_access_denied:(Some("Read")) Http_common.Unauthorized else
  
    let filePath: string option = Orbit.get_file_path file.id state in 
    Http_common.create_response ~content_type:(Some "application/octet-stream") ~x_file_path:filePath Http_common.HttpOk
;;

let checkGetFile (userId: int) (fileId: int) (state: Orbit.system) =
  let url = Printf.sprintf "http://localhost:8085/file?userId=%d&id=%d" userId fileId in
  match Ezcurl.get ~url: url () with
  | Ok resp -> (
    let expectedResult = getExpectedResult userId fileId state in
    let requestResult = Http_common.map_response resp in
    if (compare requestResult expectedResult) = 0 then true else false
    )
  | Error (_) -> false
;;