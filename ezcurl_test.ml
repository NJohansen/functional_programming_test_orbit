open Ezcurl

let url = "https://curl.haxx.se/";;

let print_ltuples l = List.iter (fun (a,b) -> Printf.printf "%s, %s \n" a b) l

let _ = match Ezcurl.get ~url: url () with
 | Ok resp -> 
    let _ = print_ltuples resp.headers in
    resp.code
 | Error (_) -> 0
;;

(* let content = match res with 
  | Ok c -> c 
  | Error (_,s) -> failwith s;;

print_endline(string_of_int content.code);; *)