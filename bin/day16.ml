open Myutils

let lines = read_file "./inputs/day16real.txt"

type valve = {
  flow_rate: int;
}

let total_minutes = 30

let print_valve { flow_rate } = print_string " { "; print_int flow_rate; print_string " } "

let create_value_item flow_rate = { flow_rate = int_of_string flow_rate }

let process_line line = line
|> split_on_strings ["Valve "; " has flow rate="; "; tunnels lead to valves "; "; tunnel leads to valve "]
|> List.tl |> function
  | [node_id; flow_rate; neighbors] -> (node_id, create_value_item flow_rate, split_on_string ", " neighbors)
  | _ -> failwith "Malformed line"

let print_valve_tuple = print_mixed_triple print_string print_valve (printlist print_string)

let map_valves base_node = 
  let rec map_valves' map node =
    if StringMap.mem node.id map then map else
    let new_map = StringMap.add node.id node.value.flow_rate map in
    List.fold_left map_valves' new_map node.neighbors in
  map_valves' StringMap.empty base_node

let map_sum minutes distance_map map = StringMap.fold (fun k v acc -> match StringMap.find_opt k distance_map with None -> acc | Some dist -> (acc + ((minutes - dist - 1) * v))) map 0

let traverse_tunnels total_minutes graph =
  let rec traverse_tunnels' route visited remaining_valves opened_valve minutes_left pressure_released valve =
    let last_visit = match StringMap.find_opt valve.id visited with None -> -1 | Some prev_visit -> prev_visit in
    if not opened_valve && last_visit >= pressure_released then (-1, valve.id ^ " revisit") else (
    if minutes_left = 0 || StringMap.cardinal remaining_valves = 0 then (pressure_released, route) else (
    let open_this_valve = if opened_valve || valve.value.flow_rate = 0 || not (StringMap.mem valve.id remaining_valves) then (-1, valve.id ^ " valve open or zero") else (
      let new_pressure_released = pressure_released + valve.value.flow_rate * (minutes_left - 1) in
      let new_visited = StringMap.update valve.id (fun _ -> Some new_pressure_released) visited in
      let new_remaining_valves = StringMap.remove valve.id remaining_valves in
      traverse_tunnels' (route ^ valve.id ^ "^") new_visited new_remaining_valves true (minutes_left - 1) new_pressure_released valve
    ) in
    let new_visited = StringMap.update valve.id (fun _ -> Some pressure_released) visited in
    let all_options = List.map (traverse_tunnels' (route ^ valve.id ^ "-") new_visited (if valve.value.flow_rate = 0 then StringMap.remove valve.id remaining_valves else remaining_valves) false (minutes_left - 1) pressure_released) valve.neighbors in
    let (new_pressure_released, new_pressure_released_route) = max_list_by (fun (pr, _) -> pr) (-1, valve.id ^ " empty list") (open_this_valve::all_options) in
    (new_pressure_released, new_pressure_released_route))) in
  traverse_tunnels' "=" StringMap.empty (map_valves graph) false total_minutes 0 graph

let run () = print_newline ();
  let valve_tuples = List.map process_line lines in
  printlist print_valve_tuple valve_tuples;
  print_newline ();
  let graph_nodes = build_graph valve_tuples in
  let base_node_option = find_node "AA" (List.hd graph_nodes) in
  match base_node_option with
  | None -> ()
  | Some base_node ->
  print_newline ();
  print_graph print_valve base_node;
  print_newline ();
  print_mixed_tuple print_int print_string (traverse_tunnels total_minutes base_node);
  print_newline ();
  print_newline ();;
(*
minute 1 
no valves are open
move to valve II

minute 2
no valves open
move to valve JJ

minute 3
no valves open
open valve JJ

minute 4
valve JJ is open, releasing 21 pressure
move to valve II

minute 5
valve JJ is open, releasing 21 pressure
move to valve AA

minute 6
valve JJ is open, releasing 21 pressure
move to valve DD

minute 7
valve JJ is open, releasing 21 pressure
open valve DD

minute 8
valves JJ and DD are open, releasing 41 pressure
move to valve EE

minute 9
valves JJ and DD are open, releasing 41 pressure
move to valve FF

minute 10
valves JJ and DD are open, releasing 41 pressure
move to valve GG

minute 11
valves JJ and DD are open, releasing 41 pressure
move to valve HH

minute 12
valves JJ and DD are open, releasing 41 pressure
open valve HH

minute 13
valves JJ and DD and HH are open, releasing 63 pressure
move to valve GG

minute 14
valves JJ and DD and HH are open, releasing 63 pressure
move to valve FF

minute 15
valves JJ and DD and HH are open, releasing 63 pressure
move to valve EE

minute 16
valves JJ and DD and HH are open, releasing 63 pressure
open valve EE

minute 17
valves JJ and DD and HH and EE are open, releasing 66 pressure
move to valve DD

minute 18
valves JJ and DD and HH and EE are open, releasing 66 pressure
move to valve AA

minute 19
valves JJ and DD and HH and EE are open, releasing 66 pressure
move to valve BB

minute 20
valves JJ and DD and HH and EE are open, releasing 66 pressure
open valve BB

minute 21
valves JJ and DD and HH and EE and BB are open, releasing 79 pressure
move to valve CC

minute 22
valves JJ and DD and HH and EE and BB are open, releasing 79 pressure
open valve CC

minute 23
valves JJ and DD and HH and EE and BB and CC are open, releasing 81 pressure

minute 24
valves JJ and DD and HH and EE and BB and CC are open, releasing 81 pressure

minute 25
valves JJ and DD and HH and EE and BB and CC are open, releasing 81 pressure

minute 26
valves JJ and DD and HH and EE and BB and CC are open, releasing 81 pressure

minute 27
valves JJ and DD and HH and EE and BB and CC are open, releasing 81 pressure

minute 28
valves JJ and DD and HH and EE and BB and CC are open, releasing 81 pressure

minute 29
valves JJ and DD and HH and EE and BB and CC are open, releasing 81 pressure

minute 30
valves JJ and DD and HH and EE and BB and CC are open, releasing 81 pressure

Mine:
=30 AA - 29 II - 28 JJ ^ 27 JJ 567 - 26 II 567 - 25 AA 567 - 24 DD 567 ^ 23 DD 1027 - 22 EE 1027 - 21 FF 1027 - 20 GG 1027 - 19 HH 1027 ^ 18 HH 1423 - 17 GG 1423 - 16 FF 1423 - 15 EE 1423 ^ 14 EE 1465 - 13 DD 1465 - 12 CC 1465 - 11 BB 1465 ^ 10 BB 1595 - 9 CC 1595 ^ 8 CC 1611 -

Correct:
=30 AA - 29 DD ^ 28 DD - 27 CC 560 - 26 BB 560 ^ 25 BB 885 - 24 AA 885 - 23 II 885  - 22 JJ 885  ^ 21 JJ 1326 - 20 II 1326 - 19 AA 1326 - 18 DD 1326 - 17 EE 1326 - 16 FF 1326 - 15 GG 1326 - 14 HH 1326 ^ 13 HH 1612 - 12 GG 1612 - 11 FF 1612 - 10 EE 1612 ^ 9 EE 1639 - 8 DD 1639 - 7 CC 1639 ^ 6 CC 1651 -
=AA-DD^DD-CC-BB^BB-AA-II-JJ^JJ-II-AA-DD-EE-FF-GG-HH^HH-GG-FF-EE^EE-DD-CC^CC-
   *)