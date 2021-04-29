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

type response = {
  status_code: status_code;
  content_type: string option;
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
}


let url = "http://localhost:8085/file/list?userId=1";;

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

let checkGetListOfFiles (userId: int): int = 
  match Ezcurl.get ~url: url () with
  | Ok resp -> (
      match map_response resp with
      | { status_code = HttpOk; _} -> 1
      | _ -> 0
    )
  | Error (_) -> 0
;;

let get_file_list = 
  Test.make ~name:"get_file_list" ~count:1000 ~max_gen:500  
  int
  (fun i -> checkGetListOfFiles i == 1)
;;
QCheck_runner.run_tests ~verbose:true [get_file_list];;

(* let res = Ezcurl.get ~url: url ();;
let moe = map_response  *)

