open Async
open Cohttp
open Cohttp_async
(* open Lwt *)
(* open Lwt
open Cohttp
open Cohttp_async
open Async *)

type info = {
  created_at: string;
  id: int;
  mimetype: string;
  modified_at: string;
  ms_timestamp: int64;
  path: string option;
  snapshots_enabled: bool option;
  name: string;
  parent_id: int;
  size: int64;
  version: int;
} [@@deriving show]

type status_code = HttpOk | NotFound | BadRequest | Unauthorized | NotAcceptable | Conflict| InternalServerError | Unknown of int [@@deriving show]

let status_code_of_int n = match n with
  | 200 -> HttpOk
  | 400 -> BadRequest
  | 401 -> Unauthorized
  | 404 -> NotFound
  | 406 -> NotAcceptable
  | 409 -> Conflict
  | 500 -> InternalServerError
  | _ -> Unknown(n)

type response = {
  status_code: status_code;
  x_entity: string option;
  x_too_long_by: string option;
  x_conflict: string option;
  x_access_denied: string option;
} [@@deriving show]

let response_of_http_response (http_res: Cohttp_async.Response.t): response = {
  status_code = status_code_of_int (Code.code_of_status http_res.status);
  x_entity = Header.get http_res.headers "X-Entity";
  x_too_long_by = Header.get http_res.headers "X-Too-Long-By";
  x_conflict = Header.get http_res.headers "X-Conflict";
  x_access_denied = Header.get http_res.headers "X-Access-Denied";
}

type infoRes =
  | Virker of (Cohttp_async.Response.t * Cohttp_async.Body.t)

type error =
  | FileNotFound
  | UnauthorizedRead
  | UnknownError of (Cohttp_async.Response.t * Cohttp_async.Body.t)

let show_error err =
  match err with
  | FileNotFound -> "FileNotFound"
  | UnauthorizedRead -> "UnauthorizedRead"
  | UnknownError (res, _) -> Printf.sprintf "HTTP Error: %d" (Code.code_of_status res.status)  

let result_of_body (res: Cohttp_async.Response.t) (body: Cohttp_async.Body.t): (infoRes, error) result Async_kernel.Deferred.t =
  (* let status = response_of_http_response res in *)
  let status = res.status |> Code.code_of_status in
  match status with
  (* | 200 -> (Ok (Virker (res, body))) *)
    (* let%bind res = from_body body in *)
    (* return (Ok body) *)
  (* | _ -> (Error FileNotFound) *)
  | _ -> return (Error FileNotFound)

let body =
  let%bind res, body = Client.get (Uri.of_string "http://localhost:8085/file/list?userId=0") in
  result_of_body res body


  (* Client.get (Uri.of_string "http://localhost:8085/file/list?userId=0") >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  Printf.printf "Body of length: %d\n" (String.length body);
  body *)

(* let body =
  let%bind x = 1 in x *)

let () =
  (**print_endline("Test")*)
  (* let body = Lwt_main.run body in *)
  (* let body = body in
  print_endline ("Received body\n" ^ body) *)
  print_endline (body)