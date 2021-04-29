open Ezcurl

let url = "https://curl.haxx.se/sddsds";;

let res = Ezcurl.get ~url ();;

let content = match res with 
  | Ok c -> c 
  | Error (_,s) -> failwith s;;



let code =
  print_endline(string_of_int content.code);;



