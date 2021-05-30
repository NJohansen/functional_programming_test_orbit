open QCheck
open Orbit

type cmd   =
  | Get_directory of int * int
  | Get_File_List of int 
  | Get_File of int * int
  | Get_Version of int * string
  | Move_File of ((int * int * int) * (int * string * int32))
  | Create_File of int * int * string * int32 
  | Create_Directory of int * int * string * int 
  | Delete_File of int * int * int
  | Delete_Dir of int * int * int 
  | Upload_File of int * int * int * int32
  | Get_File_Meta of int * int [@@deriving show { with_path = false }]

let st = Orbit.initState

let user_id_gen_old =
  let ids: int list = (List.map (fun (u: userEntity) -> u.id) st.users) in
  let idsList = if List.length ids = 0 then [1] else ids in
  Gen.oneof [
    Gen.oneofl idsList;
    Gen.small_signed_int;
  ]

let user_id_gen =
  let ids: int list = (List.map (fun (u: userEntity) -> u.id) st.users) in
  let idsList = if List.length ids = 0 then [1] else ids in
  Gen.frequency[(90, Gen.oneofl idsList); (10, Gen.small_signed_int);]

let file_id_gen =
  let ids: int list = (List.map (fun (u: fileEntity) -> u.id) st.files) in
  let idsList = if List.length ids = 0 then [1] else ids in
  Gen.frequency[(90, Gen.oneofl idsList); (10, Gen.small_signed_int);]

let dir_id_gen =
  let ids: int list = (List.map (fun (u: directoryEntity) -> u.id) st.directories) in
  let idsList = if List.length ids = 0 then [1] else ids in
  Gen.frequency[(90, Gen.oneofl idsList); (10, Gen.small_signed_int);]

let version_gen =
  Gen.frequency[(90, Gen.oneofl [1;2;3;4;5]); (10, Gen.small_signed_int);]

let char_gen = 
  let char_list: char list = ['a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';'u';'v';'x';'y';'z';] in 
  Gen.oneofl char_list
  
let forbidden_char_gen = 
  let forbidden_name_characters: char list = ['/'; ':'; '*'; '?'; '\"'; '<'; '>';] in (*Excluding backslash to avoid any string interpretation issues *)
  Gen.oneofl forbidden_name_characters
  
let name_string_gen = 
  Gen.frequency[(95, char_gen); (5, forbidden_char_gen);]
  
let name_gen =
    Gen.string_size ~gen:name_string_gen Gen.small_int

let timestamp_gen = Gen.ui32

let create_user_parameter_gen = 
  Gen.quad user_id_gen dir_id_gen name_gen timestamp_gen 

let create_dir_parameter_gen = 
  Gen.quad user_id_gen dir_id_gen name_gen version_gen

let version_string_gen =
  Gen.map3 (fun f1 f2 f3 -> Printf.sprintf "%d.%d.%d.%d.%d.%d" f1 f2 f3 f3 f2 f1) Gen.small_signed_int Gen.small_signed_int Gen.small_signed_int

let upload_file_gen = 
  Gen.quad user_id_gen file_id_gen version_gen timestamp_gen
  
let move_file_parameter_gen = 
  Gen.pair (Gen.triple user_id_gen file_id_gen version_gen) (Gen.triple dir_id_gen name_gen timestamp_gen)

let cmd_gen = Gen.oneof
  [ Gen.map (fun userId -> Get_File_List userId) user_id_gen;
    Gen.map2 (fun userId fileId -> Get_File (userId, fileId)) user_id_gen file_id_gen;
    Gen.map2 (fun userId dirId -> Get_directory (userId, dirId)) user_id_gen dir_id_gen;
    Gen.map2 (fun userId versionString -> Get_Version (userId, versionString)) user_id_gen version_string_gen;
    Gen.map3 (fun userId fileId version -> Delete_File (userId, fileId, version)) user_id_gen file_id_gen version_gen;
    Gen.map3 (fun userId dirId version -> Delete_Dir (userId, dirId, version)) user_id_gen dir_id_gen version_gen;
    Gen.map (fun (userId, fileId, fileVersion, timestamp) -> Upload_File (userId, fileId, fileVersion, timestamp)) upload_file_gen;
    Gen.map (fun (userId, parentId, dirName, dirVersion) ->  Create_Directory (userId, parentId, dirName, dirVersion)) create_dir_parameter_gen;
    Gen.map2 (fun userId fileId -> Get_File_Meta (userId, fileId)) user_id_gen file_id_gen;
    Gen.map (fun (userId, dirId, name, timestamp) ->  Create_File (userId, dirId, name,  timestamp)) create_user_parameter_gen;
    Gen.map (fun ((userId, fileId, version),(parentId, name, timestamp)) ->  Move_File ((userId, fileId, version),(parentId, name, timestamp))) move_file_parameter_gen]

let cmd_to_int c = match c with
  | Get_directory _ -> 1
  | Get_File_List _ -> 2 
  | Get_File _ -> 3
  | Get_Version _ -> 4 
  | Move_File _ -> 5
  | Create_File _ -> 6 
  | Create_Directory _ -> 7    
  | Delete_File _ -> 8  
  | Delete_Dir _ -> 9  
  | Upload_File _ -> 10   
  | Get_File_Meta _ -> 11

let char_to_num c = match c with
| 'a' -> 1
| 'b' -> 2
| 'c' -> 3
| 'd' -> 4
| 'e' -> 5
| 'f' -> 6
| 'g' -> 7
| 'h' -> 8
| 'i' -> 9
| 'j' -> 10
| 'k' -> 11
| 'l' -> 12
| 'm' -> 13
| 'n' -> 14
| 'o' -> 15
| 'p' -> 16
| 'q' -> 17
| 'r' -> 18
| 's' -> 19
| 't' -> 20
| 'u' -> 21
| 'v' -> 22
| 'x' -> 23
| 'y' -> 24
| 'z' -> 25
| '/' -> 26
|  ':' -> 27
|  '*' -> 28
|  '?' -> 29
|  '\"' -> 30
|  '<' -> 31
|  '>' -> 32

let name_length n = String.length n

(* exe 03 a*)
let value v = v
;;

(* let arb_tree = make ~print:cmd_to_string cmd_gen *)

let stat_cmd =
  let mygen =
    make ~stats:[("cmd", cmd_to_int)] cmd_gen in
  Test.make  ~count:1000 mygen (fun _ -> true)

let stat_user_id_gen = 
  let mygen =
    make ~stats:[("user id value", value)] user_id_gen in 
  Test.make ~count:1000 mygen (fun _ -> true)
;;

let stat_dir_id_gen = 
  let mygen =
    make ~stats:[("dir id value", value)] dir_id_gen in 
  Test.make ~count:1000 mygen (fun _ -> true)
;;

let stat_chars =
  let mygen =
    make ~stats:[("chars", char_to_num)] name_string_gen in
  Test.make  ~count:1000 mygen (fun _ -> true)
;;

let stat_name_length =
  let mygen =
    make ~stats:[("Length", name_length)] name_gen in
  Test.make  ~count:1000 mygen (fun _ -> true)
;;

QCheck_runner.run_tests ~verbose:true
  [ stat_user_id_gen; stat_cmd; stat_dir_id_gen; stat_chars; stat_name_length]
;;