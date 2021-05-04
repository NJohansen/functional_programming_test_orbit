open Ezcurl
open Yojson
open Yojson.Basic.Util
open Http_common

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

type jsonData = {
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

let checkGetListOfFiles (userId: int): bool =
  let url = "http://localhost:8085/file/list?userId=" ^ (string_of_int userId) in
  match Ezcurl.get ~url: url () with
  | Ok resp -> (
    match map_response resp with
    | { status_code = HttpOk; _} -> 
      let b = from_body resp.body in
    | _ -> false
    )
  | Error (_) -> false
;;

(* let () =
  (* Read the JSON file *)
  let json = Yojson.Basic.from_file "book.json" in

  (* Locally open the JSON manipulation functions *)
  let open Yojson.Basic.Util in
  let title = json |> member "title" |> to_string in
  let tags = json |> member "tags" |> to_list |> filter_string in
  let pages = json |> member "pages" |> to_int in
  let is_online = json |> member "is_online" |> to_bool_option in
  let is_translated = json |> member "is_translated" |> to_bool_option in
  let authors = json |> member "authors" |> to_list in
  let names = List.map authors ~f:(fun json -> member "name" json |> to_string) in

  (* Print the results of the parsing *)
  printf "Title: %s (%d)\n" title pages;
  printf "Authors: %s\n" (String.concat ~sep:", " names);
  printf "Tags: %s\n" (String.concat ~sep:", " tags);
  let string_of_bool_option =
    function
    | None -> "<unknown>"
    | Some true -> "yes"
    | Some false -> "no" in
  printf "Online: %s\n" (string_of_bool_option is_online);
  printf "Translated: %s\n" (string_of_bool_option is_translated) *)