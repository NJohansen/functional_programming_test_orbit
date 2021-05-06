open Ezcurl
open Yojson
open Yojson.Basic.Util
open Http_common
open Orbit

type jsonData = {
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

let checkCreateFile (userId: int) (parentId: int) (fileTitle: string) (timestamp: string) (state: Orbit.system): bool =
  let url = Printf.sprintf ("http://localhost:8085/file?userId=%d&parentId=%d&name=%s&timestamp=%s") userId parentId fileTitle timestamp in
  match Ezcurl.post ~url: url ~params: [] () with
  | Ok resp -> (
      match Http_common.map_response resp with
      | { status_code = HttpOk; _} -> true
      | { status_code = Conflict; x_conflict = Some("Entity-Exist")} -> true
      | _ -> false
    )
  | Error (_) -> false 
;;
