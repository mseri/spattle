open Lwt.Infix
open Types

let public_host = "52.19.203.87"

let server_uri host = Uri.make ~scheme:"http" ~host ~port:14561 ()

let get_call host path of_yojson : ('a, string) Result.result Lwt.t= 
  let map_endpoint = Uri.with_uri ~path host in
  Cohttp_lwt_unix.Client.get map_endpoint >>= fun (resp, body) ->
  let status = resp |> Cohttp_lwt.Response.status in
  let of_yojson x = 
    of_yojson x
    |> function
    | Ok v -> Ok v
    | Error e -> Error (Printf.sprintf "%s\n%s" e @@ Yojson.Safe.pretty_to_string x)
  in
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

  let mapId () =
    let open Lwt_result.Infix in
    get_map () >>= fun map ->
    Lwt_result.return map.mapId

  module Game(M: sig val mapId:string end) = struct
    let mapId = M.mapId

    let get_params () =
      let path = Some "game/v1/parameters" in
      let of_yojson = game_params_of_yojson in
      get_call host path of_yojson

    (** userKey is in the config, mapId is in the [map] returned by [get_map]. *)
    let init_game () =
      let path = Some ("game/v1/"^userKey^"/"^mapId^"/capital-ship/init") in
      let of_json = response_to_content ~objectKey:"userFleet" ~of_yojson:fleet_report_of_yojson in
      get_call host path of_json

    let fleet_report () =
      let path = Some ("game/v1/"^userKey^"/"^mapId^"/fleet") in
      let of_json = response_to_content ~objectKey:"userFleet" ~of_yojson:fleet_report_of_yojson in
      get_call host path of_json

    let capital_ship_top_up code =
      let path = Some ("/game/v1/"^userKey^"/"^mapId^"/capital-ship/top-up/"^code) in
      let of_json = response_to_content ~objectKey:"userFleet" ~of_yojson:fleet_report_of_yojson in
      get_call host path of_json

    (* TODO: Returns also the user fleet, extend the of_yojson function *)
    let create_battle_cruiser () =
      let path = Some ("/game/v1/"^userKey^"/"^mapId^"/capital-ship/create-battle-cruiser") in
      let of_json = response_to_content ~objectKey:"answer" ~of_yojson:typed_ship_of_yojson in
      get_call host path of_json

    (* TODO: Returns also the user fleet, extend the of_yojson function *)
    let create_energy_carrier energyAmount =
      let path = Some ("/game/v1/"^userKey^"/"^mapId^"/capital-ship/create-energy-carrier/"^(string_of_int energyAmount)) in
      let of_json = response_to_content ~objectKey:"answer" ~of_yojson:typed_ship_of_yojson in
      get_call host path of_json

    let probe (`Cruiser bC) =
      let path = Some ("/game/v1/"^userKey^"/"^mapId^"/space-probe/"^bC.uuid) in
      let of_json = response_to_content ~objectKey:"answer" ~of_yojson:space_probe_result_of_yojson in
      get_call host path of_json

    let move ship targetPoint =
      let label = targetPoint.label in
      let shipUUID = (from_typed_ship ship).uuid in
      let path = Some ("/game/v1/"^userKey^"/"^mapId^"/jump/"^shipUUID^"/"^label) in
      let of_json = response_to_content ~objectKey:"userFleet" ~of_yojson:fleet_report_of_yojson in
      get_call host path of_json

    (* answer contains the list of affected ships, also returns the use fleet *)
    let transfer_energy (`Carrier bC) destination energyLevel =
      let destination = from_typed_ship destination in
      let path = Some ("/game/v1/"^userKey^"/"^mapId^"/energy-transfer/"^bC.uuid^"/"^destination.uuid^"/"^(string_of_int energyLevel)) in
      let of_json = response_to_content ~objectKey:"answer"
          ~of_yojson:(fun ans ->
              try
                Yojson.Safe.Util.to_list ans
                |> List.fold_left (fun ships s ->
                    match ships with
                    | Error e -> Error e
                    | Ok ships ->
                      match typed_ship_of_yojson s with
                      | Ok s -> Ok (s :: ships)
                      | Error e -> Error e) (Ok [])
              with
                Yojson.Safe.Util.Type_error (e, _) -> Error e
            ) in
      get_call host path of_json

    (* TODO: Returns also the user fleet, extend the of_yojson function *)
    let attack (`Cruiser bC) targetPoint =
      let label = targetPoint.label in
      let path = Some ("/game/v1/"^userKey^"/"^mapId^"/bomb/"^bC.uuid^"/"^label) in
      (* Answer does not seem to contain the affected ships
       * (*~of_yojson: attacker_bomb_damage_report_of_yojson*)
      *)
      let of_json = response_to_content ~objectKey:"userFleet" ~of_yojson:fleet_report_of_yojson in
      get_call host path of_json
  end
end