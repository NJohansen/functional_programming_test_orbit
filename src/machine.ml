open QCheck

module CConf =
struct
  type state = { users: Orbit.userEntity list; directories: Orbit.directoryEntity list; files: Orbit.fileEntity list }
  type sut   = Orbit.system
  type cmd   =
    | Get_File_List of int  [@@deriving show { with_path = false }]
    (* | Create_File of (int) (int) (int) (int) *)

  let gen_cmd s =
    Gen.oneof
      [ Gen.map (fun userId -> Get_File_List userId) Gen.small_signed_int ]

  (* let arb_cmd s = QCheck.make ~print:show_cmd (gen_cmd s) *)
  let arb_cmd s = QCheck.make (gen_cmd s)

  let init_state  = {users = []; directories = []; files = []}
  let next_state c s = match c with
    | Get_File_List _ -> s

  let init_sut () = Orbit.initState
  let cleanup _   = ()
  let run_cmd c s h = match c with
    | Get_File_List _ -> true
  let precond _ _ = true
end
module CT = QCSTM.Make(CConf)
;;
QCheck_runner.run_tests ~verbose:true
  [CT.agree_test ~count:500 ~name:"Hashtbl-model agreement"]