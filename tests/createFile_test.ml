open Ezcurl
open QCheck
open Http_common;

let checkCreateFile (userId: int) (parentId: int) (fileTitle: int) (timestamp: int): bool =
  let url = Printf.sprintf ("http://localhost:8085/file?userId=%d&parentId=%d&name=%s&timestamp=%d") userId parentId (string_of_int fileTitle) timestamp in
  match Ezcurl.post ~url: url ~params: [] () with
  | Ok resp -> (
      match Http_common.map_response resp with
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

QCheck_runner.run_tests ~verbose:true [create_file];;