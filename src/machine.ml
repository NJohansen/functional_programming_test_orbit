open QCheck 
open Orbit
open Filelist
open Getfile
open Deletefile
open Deletedir
open Getversion

module CConf =
struct
  type state = Orbit.system ref
  type sut   = Orbit.system ref
  type cmd   =
    | Get_File_List of int 
    | Get_File of int * int
    | Get_Version of int * string 
    | Delete_File of int * int * int
    | Delete_Dir of int * int * int [@@deriving show { with_path = false }]
    (* | Create_File of (int) (int) (int) (int) *)

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

    let version_string_gen =
      Gen.map3 (fun f1 f2 f3 -> Printf.sprintf "%d.%d.%d.%d.%d.%d" f1 f2 f3 f3 f2 f1) Gen.small_signed_int Gen.small_signed_int Gen.small_signed_int in

    Gen.oneof
      [ Gen.map (fun userId -> Get_File_List userId) user_id_gen;
        Gen.map2 (fun userId fileId -> Get_File (userId, fileId)) user_id_gen file_id_gen;
        Gen.map2 (fun userId versionString -> Get_Version (userId, versionString)) user_id_gen version_string_gen;
        Gen.map3 (fun userId fileId version -> Delete_File (userId, fileId, version)) user_id_gen file_id_gen version_gen;
        Gen.map3 (fun userId dirId version -> Delete_Dir (userId, dirId, version)) user_id_gen dir_id_gen version_gen]

  let arb_cmd (st: state) = QCheck.make ~print:show_cmd (gen_cmd st)

  let init_state = 
    (begin Orbit.orbit_do_modification := false end;
     begin Orbit.orbit_state := Orbit.initState end; Orbit.orbit_state )
  let next_state c st = match c with
    | Get_File_List _ -> Orbit.next_state_done !st
    | Get_File _ -> Orbit.next_state_done !st
    | Get_Version _ -> Orbit.next_state_done !st
    | Delete_File (userId, fileId, version) -> Deletefile.deleteFileUpdateState userId fileId version st
    | Delete_Dir (userId, dirId, version) -> Deletedir.deleteDirectoryUpdateState userId dirId version st

  let init_sut () = (Printf.printf "----------------\n"; Orbit.orbit_state)
  let cleanup _   = ()
  let run_cmd c st su = match c with
    | Get_File_List userId -> 
      (Printf.printf "Get file list, user: %d \n" userId; Filelist.checkGetListOfFiles userId !st)
    | Get_File (userId, fileId) -> 
      (Printf.printf "Get file, user: %d - file: %d \n" userId fileId; Getfile.checkGetFile userId fileId !st)
    | Get_Version (userId, versionString) -> 
      (Printf.printf "Get version, user: %d - versionString: %s \n" userId versionString; Getversion.checkVersion userId versionString !st)    
    | Delete_File (userId, fileId, version) -> 
      (Printf.printf "Delete file, user: %d - file: %d - version: %d \n" userId fileId version; Deletefile.checkDeleteFile userId fileId version !st)      
    | Delete_Dir (userId, dirId, version) -> 
      (Printf.printf "Delete dir, user: %d - dir: %d - version: %d \n" userId dirId version; Deletedir.checkDeleteDirectory userId dirId version !st)

  let precond _ _ = true
end
module CT = QCSTM.Make(CConf)
;;
(* QCheck_runner.set_seed 238825645;; *)
QCheck_runner.run_tests ~verbose:true
  [CT.agree_test ~count:20 ~name:"orbit-model agreement"]