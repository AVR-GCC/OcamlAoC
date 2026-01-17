open Myutils

type resource = Ore | Clay | Obsidian | Geode

type resources = {
  ore: int;
  clay: int;
  obsidian: int;
  geode: int;
}

type resources_per_resource = {
  ore_cost: resources;
  clay_cost: resources;
  obsidian_cost: resources;
  geode_cost: resources;
}

let setr resource v = match resource with
  | Ore -> fun rs -> { ore = v; clay = rs.clay; obsidian = rs.obsidian; geode = rs.geode }
  | Clay -> fun rs -> { ore = rs.ore; clay = v; obsidian = rs.obsidian; geode = rs.geode }
  | Obsidian -> fun rs -> { ore = rs.ore; clay = rs.clay; obsidian = v; geode = rs.geode }
  | Geode -> fun rs -> { ore = rs.ore; clay = rs.clay; obsidian = rs.obsidian; geode = v }

let getr resource = match resource with
  | Ore -> fun rs -> rs.ore
  | Clay -> fun rs -> rs.clay
  | Obsidian -> fun rs -> rs.obsidian
  | Geode -> fun rs -> rs.geode

(* let setrr resource1 resource2 v = match resource1 with *)
(*   | Ore -> fun rpr -> setr resource2 v rpr.ore *)
(*   | Clay -> fun rpr -> setr resource2 v rpr.clay *)
(*   | Obsidian -> fun rpr -> setr resource2 v rpr.obsidian *)
(*   | Geode -> fun rpr -> setr resource2 v rpr.geode *)

let getrc resource1 = match resource1 with
  | Ore -> fun rpr -> rpr.ore_cost
  | Clay -> fun rpr -> rpr.clay_cost
  | Obsidian -> fun rpr -> rpr.obsidian_cost
  | Geode -> fun rpr -> rpr.geode_cost

type blueprint = {
  id: int;
  costs: resources_per_resource
}

type minute_state = {
  count: int;
  resources: resources;
  robots: resources;
  factory: resource option;
}

let base_resources = { ore = 0; clay = 0; obsidian = 0; geode = 0 }

let all_resources = [Ore; Clay; Obsidian; Geode]

let string_of_resource = function
  | Ore -> "Ore"
  | Clay -> "Clay"
  | Obsidian -> "Obsidian"
  | Geode -> "Geode"

let print_resources rc = print_endline ("{ ore: " ^ (string_of_int (getr Ore rc)) ^ " ; clay: " ^ (string_of_int (getr Clay rc)) ^ " ; obsidian: " ^ (string_of_int (getr Obsidian rc)) ^ " ; geode: " ^ (string_of_int (getr Geode rc)) ^ " }")

let print_blueprint bp =
  print_endline "{";
  print_endline (" id: " ^ (string_of_int bp.id));
  print_endline "ore:";
  print_resources bp.costs.ore_cost;
  print_endline "clay:";
  print_resources bp.costs.clay_cost;
  print_endline "obsidian:";
  print_resources bp.costs.obsidian_cost;
  print_endline "geode:";
  print_resources bp.costs.geode_cost;
  print_endline "}"

let print_state state =
  print_endline "-=-=-=-=-=-=-=-=-=-=-=-=-=-";
  print_endline ("count: " ^ string_of_int state.count);
  print_endline "resources:";
  print_resources state.resources;
  print_endline "robots:";
  print_resources state.robots;
  print_endline ("factory: " ^ (string_of_option string_of_resource state.factory))

let print_resource_option_list lst = printlist print_endline (List.map (string_of_option string_of_resource) lst)

let line_to_blueprint line = line
  |> split_on_strings ["Blueprint "; ": Each ore robot costs "; " ore. Each clay robot costs "; " ore. Each obsidian robot costs "; " ore and "; " clay. Each geode robot costs "; " obsidian."]
  |> List.tl
  |> List.rev
  |> List.tl
  |> List.rev
  |> List.map (fun x -> int_of_string x)
  |> function
    | [id; ore_for_ore; ore_for_clay; ore_for_obsidian; clay_for_obsidian; ore_for_geode; obsidian_for_geode] -> 
        let base_resources = { ore = 0; clay = 0; obsidian = 0; geode = 0 } in
        let ore_cost = setr Ore ore_for_ore base_resources in
        let clay_cost = setr Ore ore_for_clay base_resources in
        let obsidian_cost = base_resources |> setr Ore ore_for_obsidian |> setr Clay clay_for_obsidian in
        let geode_cost = base_resources |> setr Ore ore_for_geode |> setr Obsidian obsidian_for_geode in
        let costs = {
          ore_cost = ore_cost;
          clay_cost = clay_cost;
          obsidian_cost = obsidian_cost;
          geode_cost = geode_cost;
        } in
        { id = id; costs = costs }
    | _ -> failwith "Malformed line"

let sum_resources resources1 resources2 mul print = 
  let result = base_resources
  |> setr Ore (getr Ore resources1 + mul * getr Ore resources2)
  |> setr Clay (getr Clay resources1 + mul * getr Clay resources2)
  |> setr Obsidian (getr Obsidian resources1 + mul * getr Obsidian resources2)
  |> setr Geode (getr Geode resources1 + mul * getr Geode resources2) in
  if print then (
    print_endline "sum_resources";
    print_resources resources1;
    print_endline "+";
    print_resources resources2;
    print_endline "*";
    print_int mul;
    print_newline ();
    print_string "=";
    print_resources result;
    print_newline ()
  );
  result

let ciel_divide_int int1 int2 = (int1 + int2 - 1) / int2

let minutes_to_build_robot blueprint robots resources resource print =
  ignore print;
  let cost = getrc resource blueprint.costs in
  let clay_cost = getr Clay cost in
  let obsidian_cost = getr Obsidian cost in
  let clay_bots = getr Clay robots in
  let obsidian_bots = getr Obsidian robots in
  let cant_create =
    (clay_cost > 0 && clay_bots = 0) ||
    (obsidian_cost > 0 && obsidian_bots = 0) in
  if cant_create then None else
  let ore_cost = getr Ore cost in
  let ore_bots = getr Ore robots in
  let ore_reserve = getr Ore resources in
  let clay_reserve = getr Clay resources in
  let obsidian_reserve = getr Obsidian resources in
  (* printlist print_int [ore_bots; clay_bots; obsidian_bots]; *)
  let turns_for_ore = ciel_divide_int (ore_cost - ore_reserve) ore_bots in
  let turns_for_clay = if clay_bots = 0 then 0 else ciel_divide_int (clay_cost - clay_reserve) clay_bots in
  (* if print then (printlist print_int [clay_cost; clay_reserve; clay_bots]; print_newline ()); *)
  let turns_for_obsidian = if obsidian_bots = 0 then 0 else ciel_divide_int (obsidian_cost - obsidian_reserve) obsidian_bots in
  if print then (printlist print_int [ore_reserve; clay_reserve; obsidian_reserve]; print_newline ());
  if print then print_endline "+";
  if print then (printlist print_int [ore_bots; clay_bots; obsidian_bots]; print_newline ());
  if print then print_endline "*";
  if print then (printlist print_int [turns_for_ore; turns_for_clay; turns_for_obsidian]; print_newline ());
  if print then print_endline ">=";
  if print then (printlist print_int [ore_cost; clay_cost; obsidian_cost]; print_newline ());
  let minutes = max_list [turns_for_ore; turns_for_clay; turns_for_obsidian] in
  let abs_minutes = if minutes < 0 then 0 else minutes in
  if print then print_endline (string_of_int abs_minutes);
  Some (abs_minutes, cost)

let add_robot robots = function
  | Some created -> setr created ((getr created robots) + 1) robots
  | None -> robots

let ff_to_built_bot start_resources robots minutes cost print =
    let before_payment = sum_resources start_resources robots (minutes + 1) print in
    sum_resources before_payment cost (-1) print

let rec replay_run state blueprint = function
  | [] -> ()
  | None :: _ -> (
    print_state state;
    print_endline "Done"
  )
  | Some bot :: remaining -> (
    print_state state;
    match minutes_to_build_robot blueprint state.robots state.resources bot false with
    | None -> print_endline ("Cant finish bot " ^ string_of_resource bot)
    | Some (minutes, _) -> (
    let new_count = state.count - minutes - 1 in
    let cost = getrc bot blueprint.costs in
    print_endline "-=-=-=-=-=-=-=-=-=-";
    print_resources cost;
    let new_resources = ff_to_built_bot state.resources state.robots minutes cost false in
    let new_robots = add_robot state.robots (Some bot) in
    let next_state = {
      count = new_count;
      resources = new_resources;
      robots = new_robots;
      factory = Some bot;
    } in
    replay_run next_state blueprint remaining
  )
)

let rec geodes_openable_for_blueprint state blueprint =
  if state.count <= 0 then (getr Geode state.resources + (getr Geode state.robots * state.count), []) else (
  (* let (count, orer, clayr, obsr, geor, oreb, clayb, obsb, geob) = (17, 1, 6, 0, 0, 1, 2, 0, 0) in *)
  (* let is_last_correct = state.count = count *)
  (*   && getr Ore state.resources = orer *)
  (*   && getr Clay state.resources = clayr *)
  (*   && getr Obsidian state.resources = obsr *)
  (*   && getr Geode state.resources = geor *)
  (*   && getr Ore state.robots = oreb *)
  (*   && getr Clay state.robots = clayb *)
  (*   && getr Obsidian state.robots = obsb *)
  (*   && getr Geode state.robots = geob in *)
  (* if is_last_correct then ( *)
  (*   print_endline "FINAL FOUND!~!!"; *)
  (*   print_newline (); *)
  (* ); *)
  let new_robots = add_robot state.robots state.factory in
  let filter_map_creatable_robots = fun r -> 
    let minutes_and_cost = minutes_to_build_robot blueprint new_robots state.resources r false in
    match minutes_and_cost with
    | None -> None
    | Some (minutes, cost) -> Some (Some r, minutes, cost) in
  let created_robots_resources = 
    (None, state.count, base_resources) :: (List.filter_map filter_map_creatable_robots all_resources) in
  let resource_minutes_to_result = (fun (factory, minutes, cost) ->
    let new_resources = ff_to_built_bot state.resources new_robots minutes cost false in
    let new_count = state.count - minutes - 1 in
    let new_state = {
      count = new_count;
      resources = new_resources;
      robots = new_robots;
      factory = factory
    } in
    let (total, rbs) = geodes_openable_for_blueprint new_state blueprint in
    (total, factory :: rbs)
  ) in
  let totals_and_resources = List.map resource_minutes_to_result created_robots_resources in
  let max_result = max_list_by (fun (total, _) -> total) (0, []) totals_and_resources in
  max_result
  )

let part1 blueprints =
  let start_state = {
    count = 24;
    resources = base_resources;
    robots = setr Ore 1 base_resources;
    factory = None;
  } in
  (* let s1 = Sys.time () in *)
  (* let (final, _bots) = geodes_openable_for_blueprint start_state (List.hd (List.tl blueprints)) in *)
  (* let s2 = Sys.time () in *)
  (* print_endline ("total time: " ^ string_of_float (s2 -. s1)); *)
  (* print_newline (); *)
  print_newline ();
  let geodes_openable = List.map (fun bp -> 
    print_newline ();
    print_newline ();
    print_endline "checking blueprint";
    print_blueprint bp;
    let (geodes, bots) = geodes_openable_for_blueprint start_state bp in
    print_newline ();
    print_endline "with bots:";
    print_resource_option_list bots;
    print_newline ();
    print_endline ("cracks " ^ string_of_int geodes ^ " geodes");
    bp.id * geodes
  ) blueprints in
  print_endline "summed quality:";
  print_int (sum geodes_openable)

let part2 blueprints =
  let start_state = {
    count = 32;
    resources = base_resources;
    robots = setr Ore 1 base_resources;
    factory = None;
  } in
  print_newline ();
  let geodes_openable = List.map (fun bp -> 
    print_newline ();
    print_newline ();
    print_endline "checking blueprint";
    print_blueprint bp;
    let s1 = Sys.time () in
    let (geodes, bots) = geodes_openable_for_blueprint start_state bp in
    let s2 = Sys.time () in
    print_endline ("total time: " ^ string_of_float (s2 -. s1));
    print_newline ();
    print_newline ();
    print_endline "with bots:";
    print_resource_option_list bots;
    print_newline ();
    print_endline ("cracks " ^ string_of_int geodes ^ " geodes");
    geodes
  ) blueprints in
  print_endline "product of these blueprints";
  let product = List.fold_left (fun acc elem -> acc * elem) 1 geodes_openable in
  print_int product
      
let run () = print_newline ();
  print_endline "Day 19";
  print_newline ();
  let lines = read_file "./inputs/day19.real.txt" in
  print_newline ();
  let blueprints = List.map line_to_blueprint lines in
  (* part1 blueprints; *)
  let first_three = List.take 3 blueprints in
  printlist print_blueprint first_three;
  part2 first_three;
  print_newline ();;
