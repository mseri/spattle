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

let response_to_content ~objectKey ~of_yojson resp =
  let open Yojson.Safe.Util in
  let status = resp |> member "status" |> to_int in
  if status != 200 then
    let error = resp |> member "error" |> to_string in
    let message = resp |> member "message" |> to_string in
    Error (error^": "^message)
  else
    resp |> member objectKey |> of_yojson

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

  (** userKey is in the config, mapId is in [Types.map] returned by [get_map]. *)
  let init_game ~userKey ~mapId () =
    let path = Some ("game/v1/"^userKey^"/"^mapId^"/capital-ship/init") in
    let of_json = response_to_content ~objectKey:"userFleet" ~of_yojson:Types.fleet_report_of_yojson in
    get_call host path of_json

  let query_fleet ~userKey ~mapId () =
    let path = Some ("game/v1/"^userKey^"/"^mapId^"/fleet") in
    let of_json = response_to_content ~objectKey:"userFleet" ~of_yojson:Types.fleet_report_of_yojson in
    get_call host path of_json

end