open QCheck 
open Filelist
open Getfile

module CConf =
struct
  type state = { users: Orbit.userEntity list; directories: Orbit.directoryEntity list; files: Orbit.fileEntity list }
  type sut   = Orbit.system
  type cmd   =
    | Get_File_List of int 
    | Get_File of int * int [@@deriving show { with_path = false }]
    (* | Create_File of (int) (int) (int) (int) *)

  let gen_cmd =
    Gen.oneof
      [ Gen.map (fun userId -> Get_File_List userId) Gen.small_signed_int;
        Gen.map2 (fun userId fileId -> Get_File (userId, fileId)) Gen.small_signed_int Gen.small_signed_int]

  let arb_cmd _ = QCheck.make ~print:show_cmd (gen_cmd)

  let init_state  = {users = []; directories = []; files = []}
  let next_state c s = match c with
    | Get_File_List _ -> s
    | Get_File _ -> s

  let init_sut () = Orbit.initState
  let cleanup _   = ()
  let run_cmd c s h = match c with
    | Get_File_List userId -> (Filelist.checkGetListOfFiles userId h)
    | Get_File (userId, fileId) -> Getfile.checkGetFile userId fileId h

  let precond _ _ = true
end
module CT = QCSTM.Make(CConf)
;;
QCheck_runner.run_tests ~verbose:true
  [CT.agree_test ~count:10 ~name:"orbit-model agreement"]