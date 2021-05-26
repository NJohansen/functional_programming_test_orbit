open Ezcurl
open Yojson
open Yojson.Basic.Util
open Http_common
open Util
open Orbit

let olderThan inputString =
  let min = [1; 0; 60; 0] in
  let inputSplit = inputString |> String.split_on_char '.' in
  let rec check (minList: int list) (splitList: string list) =
    match minList, splitList with
    | [], []  | [], _ -> true
    | f1::r1, f2::r2 ->
      let asInt: int = int_of_string f2 in
      (match f1 with
      | f1 when f1 < asInt -> true
      | f1 when f1 = asInt -> check r1 r2
      | _ -> false
      ) 
    | f::r, [] ->
      (match f with
        | f when f < 0 -> true
        | f when f = 0 -> check r []
        | _ -> false
      ) in
      
  check min inputSplit
;;

let getExpectedResultBody (s: string) (state: Orbit.system) : string option=
  if olderThan s then (Some "{}") else (Some "Client out of date!")
;;

let getExpectedResultHeaders (s: string) (state: Orbit.system) : Http_common.response =
  if olderThan s
  then Http_common.create_response ~content_type:(Some "application/json") Http_common.HttpOk
  else Http_common.create_response Http_common.Forbidden     
;;

let checkVersion (userId: int) (s: string) (state: Orbit.system) : bool =
  let url = Printf.sprintf "http://localhost:8085/version?userId=%d&version=%s" userId s in
  match Ezcurl.get ~url: url () with
  | Ok resp -> (
    Orbit.matchResults 
      (fun _ -> getExpectedResultHeaders s state) 
      (fun _ -> Http_common.map_response resp) 
      (fun _ -> getExpectedResultBody s state) 
      (fun _ -> resp.body)
    )
  | Error (_) -> false