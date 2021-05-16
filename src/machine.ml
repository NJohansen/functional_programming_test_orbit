open QCheck 
open Filelist
open Getfile
open Deletedir

module CConf =
struct
  type state = Orbit.system
  type sut   = Orbit.system
  type cmd   =
    | Get_File_List of int 
    | Get_File of int * int 
    | Delete_Dir of int * int * int [@@deriving show { with_path = false }]
    (* | Create_File of (int) (int) (int) (int) *)

  let gen_cmd s =
    let user_id_gen =
      let ids: int list = (List.map (fun (u: userEntity) -> u.id) s.users) in
      let idsList = if List.length ids = 0 then [1] else ids in
      Gen.oneof [
        Gen.oneofl idsList;
        Gen.small_signed_int;
      ] in
    
    let file_id_gen =
      let ids: int list = (List.map (fun (u: fileEntity) -> u.id) s.files) in
      let idsList = if List.length ids = 0 then [1] else ids in
      Gen.oneof [
        Gen.oneofl idsList;
        Gen.small_signed_int;
      ] in

    let dir_id_gen =
      let ids: int list = (List.map (fun (u: directoryEntity) -> u.id) s.directories) in
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

    Gen.oneof
      [ Gen.map (fun userId -> Get_File_List userId) user_id_gen;
        Gen.map2 (fun userId fileId -> Get_File (userId, fileId)) user_id_gen file_id_gen;
        Gen.map3 (fun userId dirId version -> Delete_Dir (userId, dirId, version)) user_id_gen dir_id_gen version_gen]

  let arb_cmd s = QCheck.make ~print:show_cmd (gen_cmd s)

  (* let init_state  = {users = []; directories = []; files = []} *)
  let init_state = Printf.printf "STATE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!";  Orbit.initState
  let next_state c s = match c with
    | Get_File_List _ -> s
    | Get_File _ -> s
    | Delete_Dir (userId, dirId, version) -> Deletedir.deleteDirectoryUpdateState userId dirId version s

  let init_sut () = Printf.printf "SUT**************************************";  Orbit.initState
  let cleanup _   = ()
  let run_cmd c s h = match c with
    | Get_File_List userId -> (Filelist.checkGetListOfFiles userId h)
    | Get_File (userId, fileId) -> Getfile.checkGetFile userId fileId h
    | Delete_Dir (userId, dirId, version) -> Deletedir.checkDeleteDirectory userId dirId version h

  let precond _ _ = true
end
module CT = QCSTM.Make(CConf)
;;
QCheck_runner.run_tests ~verbose:true
  [CT.agree_test ~count:10 ~name:"orbit-model agreement"]