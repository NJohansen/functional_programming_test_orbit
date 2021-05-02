open QCheck

module CConf =
struct
  type state = ((int * string) list * (SOMEFILESYSTEM))
  type sut   = 
  type cmd   =
    | Get_File_List of int
    (* | Create_File of (int) (int) (int) (int) *)

  let gen_cmd s =
    Gen.oneof
      [ Gen.map (fun userId -> Get_File_List userId) small_signed_int;]

  let arb_cmd s = QCheck.make ~print:show_cmd (gen_cmd s)

  let init_state  = []
  let next_state c s = match c with
    | Get_File_List _ -> s

  let init_sut () = ()
  let cleanup _   = ()
  let run_cmd c s h = match c with
    | Get_File_List -> 

end
module CT = QCSTM.Make(CConf)
;;