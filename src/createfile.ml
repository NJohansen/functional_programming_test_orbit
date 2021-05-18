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

let getExpectedResultBody (userId: int) (parentId: int) (fileTitle: string) (timestamp: int) (state: Orbit.system): resultData =

let getExpectedResultHeaders  (userId: int) (parentId: int) (fileTitle: string) (timestamp: int) (state: Orbit.system): Http_common.response = 
  

let matchResults (bodyResult: resultData) (requestResult: Http_common.response) (userId: int) (parentId: int) (fileTitle: string) (timestamp: int) (state: Orbit.system): bool =
  let expectedResultHeaders = getExpectedResultHeaders userId dirId version state in
  if (compare expectedResultHeaders requestResult) != 0 then false else

  if (requestResult.status_code != Http_common.HttpOk) then true else
  let body = from_body body in
  let expectedBodyOption = getExpectedResultBody  in
  match expectedBodyOption with
  | None -> false
  | Some expectedBody -> if (compare expectedBody body) != 0 then false else true

let checkCreateFile (userId: int) (parentId: int) (fileTitle: string) (timestamp: int) (state: Orbit.system): bool =
  let url = Printf.sprintf ("http://localhost:8085/file?userId=%d&parentId=%d&name=%s&timestamp=%d") userId parentId fileTitle timestamp in
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