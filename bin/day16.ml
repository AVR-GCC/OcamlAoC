open Myutils

let lines = read_file "./inputs/day16real.txt"

type valve = {
  flow_rate: int;
}

type visit = {
  visited: int StringMap.t;
  opened: StringSet.t;
  opened_valve: bool;
  pressure_released: int;
  valve: valve node;
}

type double_visit = {
  visited: int StringMap.t;
  opened: StringSet.t;
  opened_valve: bool * bool;
  pressure_released: int;
  valves: valve node * valve node;
}

let print_valve { flow_rate } = print_string " { "; print_int flow_rate; print_string " } "

let create_value_item flow_rate = { flow_rate = int_of_string flow_rate }

let process_line line = line
|> split_on_strings ["Valve "; " has flow rate="; "; tunnels lead to valves "; "; tunnel leads to valve "]
|> List.tl |> function
  | [node_id; flow_rate; neighbors] -> (node_id, create_value_item flow_rate, List.map (fun neighbor -> (neighbor, 0)) (split_on_string ", " neighbors))
  | _ -> failwith "Malformed line"

let print_valve_tuple = print_mixed_triple print_string print_valve (printlist (print_mixed_tuple print_string print_int))

let print_station (id, flow_rate, _) = print_string id; print_string " "; print_int flow_rate; print_newline ()

let collect_stations graph = let rec collect_stations' acc nodes =
  match nodes with
  | [] -> acc
  | node::rest ->
    let checked = List.exists (fun (id, _, _) -> id = node.id) acc in
    let insert  = not checked && node.value.flow_rate <> 0 in
    let new_node = (node.id, node.value.flow_rate, node.distance_map) in
    let new_acc = if insert then new_node::acc else acc in
    collect_stations' new_acc rest in
  collect_stations' [] graph

let traverse_stations start stations =
  let rec traverse_stations' minutes score remaining (id, flow_rate, distance_map) =
    let new_score = score + minutes * flow_rate in
    let new_remaining = List.filter (fun (oid, _, _) -> oid <> id) remaining in
    let tuple_to_score (id', flow_rate', distance_map') =
      let distance = StringMap.find id' distance_map in
      if distance >= minutes then new_score else
      traverse_stations' (minutes - distance - 1) new_score new_remaining (id', flow_rate', distance_map') in
    let calculated = List.map tuple_to_score new_remaining in
    let max_score = max_list (new_score::calculated) in
    max_score in
  traverse_stations' 30 0 stations start

let traverse_stations_double start stations =
  let rec traverse_stations' score remaining route minutes minutes_to_next_station t1 t2 t3 t4 (id1, flow_rate1, distance_map1) (id2, flow_rate2, distance_map2) =
    let s1 = Sys.time () in
    let new_remaining = List.filter (fun (oid, _, _) -> oid <> id1 && oid <> id2) remaining in
    let s2 = Sys.time () in
    let new_score = score + minutes * flow_rate1 in
    let new_route = route ^ " --- " ^ id1 ^ "-" ^ id2 ^ " " ^ string_of_int minutes_to_next_station ^ " " ^ string_of_int minutes ^ "=" ^ string_of_int new_score in
    (* let spacing = String.make minutes ' ' in *)
    (* print_string spacing; print_string id1; print_string " "; print_int new_score; print_newline (); *)
    let s3 = Sys.time () in
    let t1 = s2 -. s1 +. t1 in
    let t2 = s3 -. s2 +. t2 in
    if List.length new_remaining = 0 then (
      let final_score = new_score + (minutes - minutes_to_next_station) * flow_rate2 in
      let final_route = new_route ^ " --- " ^ id2 ^ " " ^ string_of_int minutes ^ "=" ^ string_of_int final_score ^ " t1 " ^ string_of_float t1 ^ " t2 " ^ string_of_float t2 ^ " t3 " ^ string_of_float t3 ^ " t4 " ^ string_of_float t4 in
      print_endline final_route;
      final_score
    ) else
    let tuple_to_score (id', flow_rate', distance_map') =
      let distance = StringMap.find id' distance_map1 + 1 in
      let s4 = Sys.time () in
      let new_minutes_to_next_station = abs (distance - minutes_to_next_station) in
      let new_minutes = minutes - min distance minutes_to_next_station in
      let s5 = Sys.time () in
      let t3 = s4 -. s3 +. t3 in
      let t4 = s5 -. s4 +. t4 in
      let fn = traverse_stations' new_score new_remaining new_route new_minutes new_minutes_to_next_station t1 t2 t3 t4 in
      if distance >= minutes_to_next_station then
        fn (id2, flow_rate2, distance_map2) (id', flow_rate', distance_map')
      else (
        fn (id', flow_rate', distance_map') (id2, flow_rate2, distance_map2)
      ) in
    let calculated = List.map tuple_to_score new_remaining in
    let max_score = max_list (new_score::calculated) in
    max_score in
  traverse_stations' 0 stations "" 26 0 0. 0. 0. 0. start start

let run () = print_newline ();
  let valve_tuples = List.map process_line lines in
  printlist print_valve_tuple valve_tuples;
  print_newline ();
  let graph_nodes = build_graph valve_tuples in
  update_distance_maps graph_nodes;
  let stations = collect_stations graph_nodes in
  let start_opt = find_node "AA" (List.hd graph_nodes) in
  match start_opt with
  | None -> ()
  | Some start -> 
  let stations_result = traverse_stations (start.id, start.value.flow_rate, start.distance_map) stations in
  print_newline ();
  print_newline ();
  print_graph print_valve start;
  print_newline ();
  print_endline ("stations result " ^ string_of_int stations_result);
  let stations_result_d = traverse_stations_double (start.id, start.value.flow_rate, start.distance_map) stations in
  print_newline ();
  print_newline ();
  print_endline ("stations result double " ^ string_of_int stations_result_d);
  print_newline ();;
