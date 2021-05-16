open Unix

let all_present (listOne: 'a list) (listTwo: 'a list) : bool =
  if List.length listOne != List.length listTwo 
  then false else 

  let rec check (listOne: 'a list) (listTwo: 'a list) : bool = 
    match listOne with
    | []   -> true
    | f::r -> 
      let filRes = List.filter (fun s -> ((compare f s) = 0)) listTwo in
      if List.length filRes = 1 
      then check r listTwo 
      else false
  in check listOne listTwo
;;

let rec part_of_list (list: 'a list) (lookFor: 'a) : bool =
  match list with
  | [] -> false
  | f::r ->
    if f = lookFor then true else part_of_list r lookFor
;;

(* Thank you NightBlues https://gist.github.com/NightBlues/2e817c7c923ec883e73016d9fec61e2b *)
let iso_of_tm tm =
  Unix.(Printf.sprintf "%u-%02u-%02uT%02u:%02u:%02uZ" (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
          tm.tm_hour tm.tm_min tm.tm_sec)

(* Returns ISO datestring in GMT: YYYY-MM-DD T 00:00:00Z *)
let create_ISO_timestamp () = 
  let epoch_timestamp = Unix.time () in 
  let tm_date = Unix.gmtime epoch_timestamp in 
  let iso_date = iso_of_tm tm_date in 
  iso_date


let rec delete_from_list (delete: 'a) (list: 'a list) (newList: 'a list) : 'a list=
  match list with
  | [] -> newList
  | f::rest when f = delete -> newList @ rest
  | f::rest -> delete_from_list delete rest (f::newList)
;;
