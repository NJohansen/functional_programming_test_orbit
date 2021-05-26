type status_code = HttpOk | NotFound | Forbidden | BadRequest | Unauthorized | NotAcceptable | Conflict| InternalServerError | Unknown of int

let status_code_of_int n = match n with
  | 200 -> HttpOk
  | 400 -> BadRequest
  | 401 -> Unauthorized
  | 404 -> NotFound
  | 403 -> Forbidden
  | 406 -> NotAcceptable
  | 409 -> Conflict
  | 500 -> InternalServerError
  | _ -> Unknown(n)

let status_code_to_int n = match n with
  | HttpOk -> 200
  | BadRequest -> 400
  | Unauthorized -> 401
  | Forbidden -> 403
  | NotFound -> 404
  | NotAcceptable -> 406
  | Conflict -> 409
  | InternalServerError -> 500
  | Unknown(n) -> n

type response = {
  status_code: status_code;
  content_type: string option;
  x_conflict: string option;
  x_entity: string option;
  x_file_path: string option;
  x_access_denied: string option;
}
  (* x_file_version: string option;
  x_file_path: string option;
  x_file_path_encoded: string option;
  x_file_modified: string option;
  x_conflict: string option;
  (* x_access_denied: string option; *)
  x_bytes: string option; *)
  (* conection: string option;
  transfer_encoding: string option; *)

let rec map_header (headers: (string * string) list) (lookAfter: string): string option =
  match headers with
  | [] -> None
  | first::rest -> 
    match first with
    | (n, b) when n = lookAfter -> Some(b)
    | _ -> map_header rest lookAfter


let map_response (http_res: Ezcurl_core.response): response = {
  status_code = status_code_of_int http_res.code;
  content_type = map_header http_res.headers "Content-Type";
  x_conflict = map_header http_res.headers "X-Conflict";
  x_entity = map_header http_res.headers "X-Entity";
  x_file_path = map_header http_res.headers "X-File-Path";
  x_access_denied = map_header http_res.headers "X-Access-Denied";
}

let create_response 
  ?(content_type: string option = None) 
  ?(x_conflict: string option = None) 
  ?(x_entity: string option = None)
  ?(x_file_path: string option = None)
  ?(x_access_denied: string option = None)
  (status: status_code) : response =
  {
    status_code = status;
    content_type = content_type;
    x_conflict = x_conflict;
    x_entity = x_entity;
    x_file_path = x_file_path;
    x_access_denied = x_access_denied;
  }