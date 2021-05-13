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


