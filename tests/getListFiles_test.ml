open Ezcurl
open QCheck
open Http_common;

let print_ltuples l = List.iter (fun (a,b) -> Printf.printf "%s, %s \n" a b) l;;

let checkGetListOfFiles (userId: int): bool =
  let url = "http://localhost:8085/file/list?userId=" ^ (string_of_int userId) in
  match Ezcurl.get ~url: url () with
  | Ok resp -> (
      match Http_common.map_response resp with
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

QCheck_runner.run_tests ~verbose:true [get_file_list];;