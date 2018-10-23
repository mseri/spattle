let yojson_error_to_lwt_error = function
  | Error s -> Lwt_result.fail s
  | Ok m -> Lwt_result.ok (Lwt.return m)

(*
{
  "mapSize": 1000,
  "mapJumpPointsCardinality": 500,
  "fleetCapitalShipInitialEnergyLevel": 4000,
  "fleetCapitalShipTopUpChallengeDifficulty": 6,
  "fleetCapitalShipTopUpEnergyValue": 1000,
  "fleetBattleCruiserBuildEnergyCost": 100,
  "fleetBattleCruiserInitialEnergyLevel": 500,
  "fleetBattleCruiserBombBuildingCost": 10,
  "fleetBattleCruiserBombNominalEnergy": 50,
  "fleetEnergyCarrierBuildEnergyCost": 50,
  "fleetShipNomenclature2JumpCostCoefficient": {
    "energyCarrier": 0.01,
    "battleCruiser": 0.1,
    "capitalShip": 0.5
  },
  "fleetShipsMaxEnergy": {
    "capitalShip": 5000,
    "battleCruiser": 1000,
    "energyCarrier": 800
  },
  "bombsEffectMultiplier": 3,
  "scoringPointsForKillPerNomenclature": {
    "energyCarrier": 20,
    "battleCruiser": 100,
    "capitalShip": 1000
  },
  "serverThrottlingPausingPeriodInSeconds": 0.1
}

endpoint: game/v1/parameters
*)

type 'a sheeps_stats = {
  capitalShip: 'a;
  battleCruiser: 'a;
  energyCarrier: 'a;
} [@@deriving yojson]

type game_params = {
  mapSize: int;
  mapJumpPointsCardinality: int;
  fleetCapitalShipInitialEnergyLevel: int;
  fleetCapitalShipTopUpChallengeDifficulty: int;
  fleetCapitalShipTopUpEnergyValue: int;
  fleetBattleCruiserBuildEnergyCost: int;
  fleetBattleCruiserInitialEnergyLevel: int;
  fleetBattleCruiserBombBuildingCost: int;
  fleetBattleCruiserBombNominalEnergy: int;
  fleetEnergyCarrierBuildEnergyCost: int;
  fleetShipNomenclature2JumpCostCoefficient: float sheeps_stats;
  fleetShipsMaxEnergy: int sheeps_stats;
  bombsEffectMultiplier: int;
  scoringPointsForKillPerNomenclature: int sheeps_stats;
  serverThrottlingPausingPeriodInSeconds: float;
} [@@deriving yojson]

(*
Map: {
  "mapId"     : "0fa91da4-788e-458e-b8b0-4e57d31ffbee"
  "timestamp" : "2018-09-15-21"
  "points"    : Array[MapPoint]
}

MapPoint: {
  "label": "4420b065",
  "coordinates": [
    58.21,
    599.61
  ]
}

endpoint: /game/v1/map

*)

type map_point = {
  label: string;
  coordinates: float*float;
} [@@deriving yojson]

type map = {
  mapId: string;
  timestamp: string;
  points: map_point list;
} [@@deriving yojson]


(*
CapitalShip 
{
	"nomenclature" : "capitalShip"
    "uuid"         : UUID
	"location"     : MapPoint
	"energyLevel"  : Float
	"alive"        : Boolean
}

BattleCruiser
{
	"nomenclature" : "battleCruiser"
	"uuid"         : UUID
	"location"     : MapPoint
	"energyLevel"  : Float
	"alive"        : Boolean
}

EnergyCarrier
{
	"nomenclature" : "energyCarrier"
	"uuid"         : UUID
	"location"     : MapPoint
	"energyLevel"  : Float
	"alive"        : Boolean
}
*)

type ship = {
  nomenclature: string;
  (** One of: capitalShip, battleCruiser, energyCarrier *)
  uuid: string;
  location: map_point;
  energyLevel: float;
  alive: bool;
} [@@deriving yojson]

type typed_ship = Capital of ship | Cruiser of ship | Carrier of ship
exception InvalidShip of string

let to_typed_ship s =
  match s.nomenclature with
  | "capitalShip"   -> Capital s
  | "battleCruiser" -> Cruiser s
  | "energyCarrier" -> Carrier s
  | _ -> raise (InvalidShip s.nomenclature)

let of_typed_ship = function
  | Capital s -> s
  | Cruiser s -> s
  | Carrier s -> s

(*
FleetReport
{
    "username"          : YOUR-USERNAME
    "capitalEnergyTopUpChallenge" : CapitalShipTopUpChallenge
    "gameScore"         : Float
    "ships"             : Array[Ship] # Ship is CapitalShip, BattleCruiser or EnergyCarrier
    "mapExploration"    : Array[MapPointLabel]
    "spaceProbeResults" : Map[UUID, SpaceProbeResults]
    "logWarnings"       : Array[WarningLogItem]
}

SpaceProbeResults
{
    "unixtime" : Unixtime
    "datetime" : DateTime # same instant as the unixtime, given for user friendliness
    "location" : MapPoint
    "results"  : Array[SpaceProbeResultItem]
}

SpaceProbeResultItem 
{
    "location"     : MapPoint
    "nomenclature" : ShipNomenclature
    "username"     : USERNAME # who owns this ship
}

WarningLogItemWormholeBomb
 {
 	"unixtime"  : Unixtime
 	"eventUUID" : UUID
 	"eventType" : "WormholeBomb"
 	"eventData" : {
 		"source" : WarningLogItemWormholeBombEventDataSource
 		"target" : Ship
 	}
 }

 WarningLogItemWormholeBombEventDataSource
 {
 	"location"     : MapPoint
 	"nomenclature" : String # ship nomenclature
 	"username"     : String # enemy username
 }

*)

type capital_ship_top_up_challenge = {
  input: string;
  difficulty: string;
} [@@deriving yojson]

type space_probe_result_item = {
  location: map_point;
  nomenclature: string;
  username: string;
} [@@deriving yojson]

type space_probe_result = {
  unixtime: float;
  datetime: string;
  location: map_point;
  results: space_probe_result_item list;
} [@@deriving yojson]


type warning_log_item_wormhole_data_source = {
  location: map_point;
  nomenclature: string;
  username: string;
} [@@deriving yojson]

type warning_log_item_data_source = {
  source: warning_log_item_wormhole_data_source;
  target: ship;
} [@@deriving yojson]

type warning_log_item = {
  unixtime: float;
  eventUUID: string;
  eventType: string; (** only "WormholeBomb" for now*)
  eventData: warning_log_item_data_source;
} [@@deriving yojson]

type fleet_report = {
  username: string;
  capitalEnergyTopUpChallenge: capital_ship_top_up_challenge;
  gameScore: float;
  ships: ship list;
  mapExploration: string list; (** list of map_point.label *)
  spaceProbeResults: (string * space_probe_result) list; (** map of UUID, result *)
  logWarnings: warning_log_item list;
} [@@deriving yojson]


(*
AttackerBombDamageReport = Array[AttackerBombDamageReportItem]

AttackerBombDamageReportItem
{
	"username"     : USERNAME
	"nomenclature" : SHIP-NOMENCLATURE
	"destroyed"    : Boolean
}
*)

type attacker_bomb_damage_report_item = {
  username: string;
  nomenclature: string; (** one of the nomenclatures *)
  destroyed: bool;
} [@@deriving yojson]

type attacker_bomb_damage_report =
  attacker_bomb_damage_report_item list [@@deriving yojson]
