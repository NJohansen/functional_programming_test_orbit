open Ezcurl
open Yojson
open Yojson.Basic.Util
open Http_common
open Orbit
open Util

type directory_permissions = {
    create: bool;
    read: bool;
    update: bool;
    delete: bool;
    } [@@deriving show]

type directoryElement = {
  id: int;
  name: string;
  path: string;
  version: int;
  __permissions: directory_permissions option;
  parent: int option; 
  is_checked_out: bool;
  is_default: bool;
} [@@deriving show]

let rec map_perm_values list search = 
  match list with 
  | [] -> false
  | (a,bool)::rest when a = search -> (bool |> to_bool)
  | (a,bool)::rest -> map_perm_values rest search

let map_permissionValues permissions : directory_permissions option =
  if List.length permissions = 0 then None else Some(
  {create = map_perm_values permissions "create"; 
  read = map_perm_values permissions "read";
  update = map_perm_values permissions "update"; 
  delete = map_perm_values permissions "delete"})
  
(* Parse data from body to type with records *)
let from_body body =
  let json = Yojson.Basic.from_string body in
  
  let parent = json |> member "parent" in 
  
  (* Handle if it is there is no parent which means it is the root folder *)
  if parent == `Null then 
    {id = (json |> member "id" |> to_int); 
  name = (json |> member "name" |> to_string);
  path = (json |> member "path" |> to_string); 
  version = (json |> member "version" |> to_int);
  __permissions = None;
  parent = None;
  is_checked_out = (json |> member "is_checked_out" |> to_bool);
  is_default = (json |> member "is_default" |> to_bool)} else

  let parentObject =  json |> member "parent" |> to_assoc in 
  match List.hd parentObject with 
  | (_,b) -> let parentId = (to_int_option b) in
  
  let any_permissions = json |> member "__permissions" in 
  
  (* Handle if it is the Bypass user *)
  if any_permissions == `Null then  
  {id = (json |> member "id" |> to_int); 
  name = (json |> member "name" |> to_string);
  path = (json |> member "path" |> to_string); 
  version = (json |> member "version" |> to_int);
  __permissions = None;
  parent = parentId;
  is_checked_out = (json |> member "is_checked_out" |> to_bool);
  is_default = (json |> member "is_default" |> to_bool)}

  (* If not bypass user *)
  else let permissionObject = json |> member "__permissions" |> to_assoc in 
  let permissionList = map_permissionValues permissionObject in 
  {id = (json |> member "id" |> to_int); 
  name = (json |> member "name" |> to_string);
  path = (json |> member "path" |> to_string); 
  version = (json |> member "version" |> to_int);
  __permissions = permissionList;
  parent = parentId;
  is_checked_out = (json |> member "is_checked_out" |> to_bool);
  is_default = (json |> member "is_default" |> to_bool)}
;;

let getExpectedHeader (userId: int) (dirId: int) (state: Orbit.system) : Http_common.response =
  let directoryOption: Orbit.directoryEntity option = Orbit.get_directory dirId state in
  match directoryOption with 
  | None -> Http_common.create_response ~x_entity:(Some("Directory")) Http_common.NotFound
  | Some directory ->  

  if (Orbit.can_read_directory userId dirId state) = false then 
  Http_common.create_response ~x_access_denied:(Some("Read")) Http_common.Unauthorized else 
  Http_common.create_response ~content_type:(Some "application/json") Http_common.HttpOk 
;;

(* Helper method to getExpectedBody *)
let get_checked_out_list (user: Orbit.userEntity option) : int list = 
  match user with 
  | Some {checkedOut = ch} -> ch
  | None -> [] 
;;

(* Helper method to getExpectedBody *)
let get_default_list (user: Orbit.userEntity option) : int list = 
  match user with 
  | Some {defaults = df} -> df
  | None -> [] 
;;

let getExpectedBody (userId: int) (dirId: int) (state: Orbit.system) : directoryElement option =
  let expectedUser: Orbit.userEntity option = Orbit.get_user userId state in 

  let userRights: Orbit.user_rights = 
    match expectedUser with 
    | Some {Orbit.id = int; rights = permissions } -> permissions
    | None -> NoRights
    in

  (* Get all ID from checked out dirs *)
  let checked_out_list = get_checked_out_list expectedUser in
  
  (* Get all ID from default dirs *)
  let default_list = get_default_list expectedUser in

  let expectedDirectory: Orbit.directoryEntity option = Orbit.get_directory dirId state in
  match expectedDirectory with 
  | None -> None
  | Some directory -> 
    if userRights = Bypass then 
      Some (
        {
          id = directory.id; 
          name = directory.name; 
          path = directory.path; 
          version = directory.version; 
          __permissions = None ; 
          parent = directory.parent; 
          is_checked_out = List.mem dirId checked_out_list ; 
          is_default = List.mem dirId default_list; 
        })
    else 
      let permission_object =
        match (Orbit.has_read_rights userId (Some directory.id) state), (Orbit.has_crud_rights userId (Some directory.id) state) with
        | _, true -> Some {create = true; read = true; update = true; delete = true; }
        | true, _ -> Some {create = false; read = true; update = false; delete = false;}
        | _, _ -> None
      in 
  
      Some (
        {
          id = directory.id; 
          name = directory.name; 
          path = directory.path; 
          version = directory.version; 
          __permissions = permission_object ; 
          parent = directory.parent; 
          is_checked_out = List.mem dirId checked_out_list;
          is_default = List.mem dirId default_list; 
        })
;;

let checkGetDirectory (userId: int) (dirId: int) (state: Orbit.system): bool =
  let url = "http://localhost:8085/api/directories?userId=" ^ (string_of_int userId) ^ "&id=" ^ (string_of_int dirId) in
  match Ezcurl.get ~url: url () with
    | Ok resp -> (
    Orbit.matchResults
      (fun _ -> getExpectedHeader userId dirId state) 
      (fun _ -> Http_common.map_response resp) 
      (fun _ -> getExpectedBody userId dirId state) 
      (fun _ -> from_body resp.body)         
    )
  | Error (_) -> false
;;