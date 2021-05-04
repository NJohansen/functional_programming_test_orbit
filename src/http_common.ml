open Ezcurl
open QCheck

type status_code = HttpOk | NotFound | BadRequest | Unauthorized | NotAcceptable | Conflict| InternalServerError | Unknown of int

let status_code_of_int n = match n with
  | 200 -> HttpOk
  | 400 -> BadRequest
  | 401 -> Unauthorized
  | 404 -> NotFound
  | 406 -> NotAcceptable
  | 409 -> Conflict
  | 500 -> InternalServerError
  | _ -> Unknown(n)

let status_code_to_int n = match n with
  | HttpOk -> 200
  | BadRequest -> 400
  | Unauthorized -> 401
  | NotFound -> 404
  | NotAcceptable -> 406
  | Conflict -> 409
  | InternalServerError -> 500
  | Unknown(n) -> n

type response = {
  status_code: status_code;
  content_type: string option;
  x_conflict: string option;
  x_entity: string option;
}
  (* x_file_version: string option;
  x_file_path: string option;
  x_file_path_encoded: string option;
  x_file_modified: string option;
  x_conflict: string option;
  (* x_access_denied: string option; *)
  x_bytes: string option; *)
  (* conection: string option;
  transfer_encoding: string option; *)

let rec map_header (headers: (string * string) list) (lookAfter: string): string option =
  match headers with
  | [] -> None
  | first::rest -> 
    match first with
    | (lookAfter, b) -> Some(b)
    | _ -> map_header rest lookAfter


let map_response (http_res: Ezcurl_core.response): response = {
  status_code = status_code_of_int http_res.code;
  content_type = map_header http_res.headers "Content-Type";
  x_conflict = map_header http_res.headers "X-Conflict";
  x_entity = map_header http_res.headers "X-Entity";
}


let url_file_list = "http://localhost:8085/file/list?userId=1";;

let print_ltuples l = List.iter (fun (a,b) -> Printf.printf "%s, %s \n" a b) l;;

(* let _ = match Ezcurl.get ~url: url () with
 | Ok resp ->
    let resss = map_header resp.headers "Content-Type" in
    let s = (match resss with
      | Some(a) -> Printf.printf "HEADER::::: %s \n" a
      | None -> Printf.printf "NO HEADER::::: %s \n" "None") in
    1
 | Error (_) -> 1
;; *)

let checkGetListOfFiles (userId: int): bool =
  let url = "http://localhost:8085/file/list?userId=" ^ (string_of_int userId) in
  match Ezcurl.get ~url: url () with
  | Ok resp -> (
      match map_response resp with
      | { status_code = HttpOk; _} -> true
      | _ -> false
    )
  | Error (_) -> false
;;

let get_file_list = 
  Test.make ~name:"get_file_list" ~count:1000 ~max_gen:500  
  small_signed_int
  (fun i -> checkGetListOfFiles i == true)
;;

let checkCreateFile (userId: int) (parentId: int) (fileTitle: int) (timestamp: int): bool =
  let url = Printf.sprintf ("http://localhost:8085/file?userId=%d&parentId=%d&name=%s&timestamp=%d") userId parentId (string_of_int fileTitle) timestamp in
  match Ezcurl.post ~url: url ~params: [] () with
  | Ok resp -> (
      match map_response resp with
      | { status_code = HttpOk; _} -> true
      | { status_code = Conflict; x_conflict = Some("Entity-Exist")} -> true
      | r -> begin Printf.printf "Response code: %n\n" (status_code_to_int r.status_code); false end
    )
  | Error (_) -> begin Printf.printf "Response code: %s\n" "test"; false end
;;

let intGen =
  Gen.oneof[
    Gen.oneofl[0; 100; 101; 102;];
    Gen.int; 
    Gen.small_nat;
  ]

let value v = v
let create_file = 
  (* let mygen =
    make ~stats:[("int value", value)] intGen in *)
  Test.make ~name:"create_file" ~count:1000 ~max_gen:500
  (quad (small_signed_int) (small_signed_int) (small_signed_int) (small_signed_int)) 
  (fun (l1,l2,l3,l4) -> checkCreateFile l1 l2 l3 l4 == true)
;;

(* QCheck_runner.run_tests ~verbose:true [create_file];;  *)

(* let res = Ezcurl.get ~url: url ();; get_file_list
let moe = map_response  *)

