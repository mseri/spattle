open Lwt.Infix

let public_host = "52.19.203.87"

let server_uri host = Uri.make ~scheme:"http" ~host ~port:14561 ()

let get_call host path of_yojson : ('a, string) Result.result Lwt.t= 
  let map_endpoint = Uri.with_uri ~path host in
  Cohttp_lwt_unix.Client.get map_endpoint >>= fun (resp, body) ->
  let status = resp |> Cohttp_lwt.Response.status in
  if Cohttp.Code.(is_error (code_of_status status))
  then Lwt.fail_with (
      "Unable to contact server: " ^ (Cohttp.Code.string_of_status status))
  else 
    body |> Cohttp_lwt.Body.to_string
    >|= Yojson.Safe.from_string >|= of_yojson 
    >>= Types.yojson_error_to_lwt_error

module Client(H: sig val host: string end) = struct
  let host = server_uri H.host

  let get_map () =
    let path = Some "game/v1/map" in
    let of_yojson = Types.map_of_yojson in
    get_call host path of_yojson

  let get_params () =
    let path = Some "game/v1/parameters" in
    let of_yojson = Types.game_params_of_yojson in
    get_call host path of_yojson
end