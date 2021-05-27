open QCheck 
open Orbit
open Filelist
open Getfile
open Createfile
open Createdir
open Deletefile
open Deletedir
open Getfilemeta
open Getdirectory
open Getversion
open Movefile

module CConf =
struct
  type state = Orbit.system ref
  type sut   = Orbit.system ref
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
    | Get_File_Meta of int * int [@@deriving show { with_path = false }]

  let gen_cmd (st: state) =
    let user_id_gen =
      let ids: int list = (List.map (fun (u: userEntity) -> u.id) !st.users) in
      let idsList = if List.length ids = 0 then [1] else ids in
      Gen.oneof [
        Gen.oneofl idsList;
        Gen.small_signed_int;
      ] in
    
    let file_id_gen =
      let ids: int list = (List.map (fun (u: fileEntity) -> u.id) !st.files) in
      let idsList = if List.length ids = 0 then [1] else ids in
      Gen.oneof [
        Gen.oneofl idsList;
        Gen.small_signed_int;
      ] in

    let dir_id_gen =
      let ids: int list = (List.map (fun (u: directoryEntity) -> u.id) !st.directories) in
      let idsList = if List.length ids = 0 then [1] else ids in
      Gen.oneof [
        Gen.oneofl idsList;
        Gen.small_signed_int;
      ] in

    let version_gen =
      Gen.oneof [
        Gen.oneofl [1;2;3;4;5];
        Gen.small_signed_int;
      ] in

    let char_gen = 
      let char_list: char list = ['a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';'u';'v';'x';'y';'z';] in 
      Gen.oneofl char_list
      in
    let forbidden_char_gen = 
      let forbidden_name_characters: char list = ['/'; ':'; '*'; '?'; '\"'; '<'; '>';] in (*Excluding backslash to avoid any string interpretation issues *)
      Gen.oneofl forbidden_name_characters
      in
    let name_string_gen = 
      Gen.frequency[(95, char_gen); (5, forbidden_char_gen);]
      in
    let name_gen =
        Gen.string_size ~gen:name_string_gen Gen.small_int;
      in

    let timestamp_gen =
        Gen.ui32;
      in

    let create_user_parameter_gen = 
      Gen.quad user_id_gen dir_id_gen name_gen timestamp_gen 
      in
    let create_dir_parameter_gen = 
      Gen.quad user_id_gen dir_id_gen name_gen version_gen
      in
    let version_string_gen =
      Gen.map3 (fun f1 f2 f3 -> Printf.sprintf "%d.%d.%d.%d.%d.%d" f1 f2 f3 f3 f2 f1) Gen.small_signed_int Gen.small_signed_int Gen.small_signed_int in

    let move_file_parameter_gen = 
      Gen.pair (Gen.triple user_id_gen file_id_gen version_gen) (Gen.triple dir_id_gen name_gen timestamp_gen) in

    Gen.oneof
      [ Gen.map (fun userId -> Get_File_List userId) user_id_gen;
        Gen.map2 (fun userId fileId -> Get_File (userId, fileId)) user_id_gen file_id_gen;
        Gen.map2 (fun userId dirId -> Get_directory (userId, dirId)) user_id_gen dir_id_gen;
        Gen.map2 (fun userId versionString -> Get_Version (userId, versionString)) user_id_gen version_string_gen;
        Gen.map3 (fun userId fileId version -> Delete_File (userId, fileId, version)) user_id_gen file_id_gen version_gen;
        Gen.map3 (fun userId dirId version -> Delete_Dir (userId, dirId, version)) user_id_gen dir_id_gen version_gen;
        Gen.map (fun (userId, parentId, dirName, dirVersion) ->  Create_Directory (userId, parentId, dirName, dirVersion)) create_dir_parameter_gen]
        Gen.map2 (fun userId fileId -> Get_File_Meta (userId, fileId)) user_id_gen file_id_gen;
        Gen.map (fun (userId, dirId, name, timestamp) ->  Create_File (userId, dirId, name,  timestamp)) create_user_parameter_gen;
        Gen.map (fun ((userId, fileId, version),(parentId, name, timestamp)) ->  Move_File ((userId, fileId, version),(parentId, name, timestamp))) move_file_parameter_gen]

  let arb_cmd (st: state) = QCheck.make ~print:show_cmd (gen_cmd st)

  let init_state = 
    (begin Orbit.orbit_do_modification := false end;
     begin Orbit.orbit_state := Orbit.initState end; Orbit.orbit_state )
  let next_state c st = match c with
    | Get_File_List _ -> Orbit.next_state_done !st
    | Get_directory _ -> Orbit.next_state_done !st
    | Get_File _ -> Orbit.next_state_done !st
    | Get_Version _ -> Orbit.next_state_done !st
    | Move_File ((userId, fileId, version),(parentId, name, timestamp)) -> Movefile.moveFileUpdateState userId fileId version parentId name (Int32.to_int timestamp) st
    | Delete_File (userId, fileId, version) -> Deletefile.deleteFileUpdateState userId fileId version st
    | Delete_Dir (userId, dirId, version) -> Deletedir.deleteDirectoryUpdateState userId dirId version st
    | Get_File_Meta _ -> st
    | Create_File (userId, dirId, name, timestamp) -> Createfile.createFileUpdateState st userId dirId name (Int32.to_int timestamp) 
    | Create_Directory (userId, parentId, dirName, dirVersion) -> Createdir.createDirUpdateState st userId parentId dirName dirVersion

  let init_sut () = (Printf.printf "----------------\n"; Orbit.orbit_state)
  let cleanup _   = ()
  let run_cmd c st su = match c with
    | Get_File_List userId -> 
      (Printf.printf "Get file list, user: %d \n" userId; Filelist.checkGetListOfFiles userId !st)
    | Get_directory (userId,dirId) -> 
      (Printf.printf "Get directory, user: %d - directory: %d \n" userId dirId; Getdirectory.checkGetDirectory userId dirId !st)
    | Get_File (userId, fileId) -> 
      (Printf.printf "Get file, user: %d - file: %d \n" userId fileId; Getfile.checkGetFile userId fileId !st)
    | Get_Version (userId, versionString) -> 
      (Printf.printf "Get version, user: %d - versionString: %s \n" userId versionString; Getversion.checkVersion userId versionString !st)
    | Move_File ((userId, fileId, version),(parentId, name, timestamp)) -> 
      (Printf.printf "Move file, user: %d - file: %d - version: %d \n" userId fileId version; Movefile.checkMoveFile userId fileId version parentId name (Int32.to_int timestamp) !st)           
    | Delete_File (userId, fileId, version) -> 
      (Printf.printf "Delete file, user: %d - file: %d - version: %d \n" userId fileId version; Deletefile.checkDeleteFile userId fileId version !st)      
    | Delete_Dir (userId, dirId, version) -> 
      (Printf.printf "Delete dir, user: %d - dir: %d - version: %d \n" userId dirId version; Deletedir.checkDeleteDirectory userId dirId version !st)
    | Get_File_Meta (userId, fileId) ->
      (Printf.printf "Get file metadata, user: %d - file: %d \n" userId fileId; Getfilemeta.checkFileMeta userId fileId !su)
    | Create_File (userId, dirId, name, timestamp) -> 
      (Printf.printf "Create file, user: %d - dir: %d - name: %s - timestamp: %ld \n" userId dirId name timestamp; Createfile.checkCreateFile userId dirId name (Int32.to_int timestamp) !st)
    | Create_Directory (userId, parentId, dirName, dirVersion) -> 
      (Printf.printf "Create directory, user: %d - parentdir: %d - dirname: %s - dirversion: %d \n" userId parentId dirName dirVersion; Createdir.checkCreateDir userId parentId dirName dirVersion !st)

  let precond _ _ = true
end
module CT = QCSTM.Make(CConf)
;;
(* QCheck_runner.set_seed 202100576;; *)
QCheck_runner.run_tests ~verbose:true
  [CT.agree_test ~count:20 ~name:"orbit-model agreement"]