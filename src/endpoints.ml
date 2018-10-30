open Lwt.Infix
open Types

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
    >>= yojson_error_to_lwt_error

module Client(H: sig val host: string val userKey: string end) = struct
  let host = server_uri H.host
  let userKey = H.userKey

  let get_map () =
    let path = Some "game/v1/map" in
    let of_yojson = map_of_yojson in
    get_call host path of_yojson

  let get_params () =
    let path = Some "game/v1/parameters" in
    let of_yojson = game_params_of_yojson in
    get_call host path of_yojson

  (** userKey is in the config, mapId is in the [map] returned by [get_map]. *)
  let init_game ~mapId () =
    let path = Some ("game/v1/"^userKey^"/"^mapId^"/capital-ship/init") in
    let of_json = response_to_content ~objectKey:"userFleet" ~of_yojson:fleet_report_of_yojson in
    get_call host path of_json

  let fleet_report ~mapId () =
    let path = Some ("game/v1/"^userKey^"/"^mapId^"/fleet") in
    let of_json = response_to_content ~objectKey:"userFleet" ~of_yojson:fleet_report_of_yojson in
    get_call host path of_json

  let capital_ship_top_up ~mapId code =
    let path = Some ("/game/v1/"^userKey^"/"^mapId^"/capital-ship/top-up/"^code) in
    let of_json = response_to_content ~objectKey:"userFleet" ~of_yojson:fleet_report_of_yojson in
    get_call host path of_json

  (* TODO: Returns also the user fleet, extend the of_yojson function *)
  let create_battle_cruiser ~mapId () =
    let path = Some ("/game/v1/"^userKey^"/"^mapId^"/capital-ship/create-battle-cruiser") in
    let of_json = response_to_content ~objectKey:"answer" ~of_yojson:typed_ship_of_yojson in
    get_call host path of_json

  (* TODO: Returns also the user fleet, extend the of_yojson function *)
  let create_energy_carrier ~mapId energyAmount =
    let path = Some ("/game/v1/"^userKey^"/"^mapId^"/capital-ship/create-energy-carrier/"^(string_of_int energyAmount)) in
    let of_json = response_to_content ~objectKey:"answer" ~of_yojson:typed_ship_of_yojson in
    get_call host path of_json

  let battle_cruiser_probe ~mapId battleCruiserUUID =
    let path = Some ("/game/v1/"^userKey^"/"^mapId^"/space-probe/"^battleCruiserUUID) in
    let of_json = response_to_content ~objectKey:"answer" ~of_yojson:space_probe_result_of_yojson in
    get_call host path of_json

  let move ~mapId shipUUID targetPointLabel =
    let path = Some ("/game/v1/"^userKey^"/"^mapId^"/jump/"^shipUUID^"/"^targetPointLabel) in
    let of_json = response_to_content ~objectKey:"userFleet" ~of_yojson:fleet_report_of_yojson in
    get_call host path of_json

  let transfer_energy ~mapId fromUUID toUUID energyLevel =
    let path = Some ("/game/v1/"^userKey^"/"^mapId^"/energy-transfer/"^fromUUID^"/"^toUUID^"/"^energyLevel) in
    let of_json x = Ok x in
    get_call host path of_json

  (* TODO: Returns also the user fleet, extend the of_yojson function *)
  let attack ~mapId shipUUID targetPointLabel =
    let path = Some ("/game/v1/"^userKey^"/"^mapId^"/bomb/"^shipUUID^"/"^targetPointLabel) in
    let of_json = response_to_content ~objectKey:"answer" ~of_yojson:attacker_bomb_damage_report_of_yojson in
    get_call host path of_json

end