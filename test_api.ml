(* open Base
open Lwt
open Cohttp
open Cohttp_async
open Async *)

(* let body =
  let%bind res, body = Client.get (Uri.of_string "http://localhost:8085/file/list?userId=0") in
  result_of_body res body


let result_of_body (res: Cohttp_async.Response.t) (body: Cohttp_async.Body.t): (info) result Async_kernel.Deferred.t =
  match res.status with
  | 200 -> Ok
  | _ -> Error *)

  (* Client.get (Uri.of_string "http://localhost:8085/file/list?userId=0") >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  Printf.printf "Body of length: %d\n" (String.length body);
  body *)

let body =
  let%bind x = Some(1) in x

let () =
  (**print_endline("Test")*)
  let body = Lwt_main.run body in
  print_endline ("Received body\n" ^ body)