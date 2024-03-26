open Myutils

let lines = read_file "./inputs/day16test.txt"

type valve = {
  flow_rate: int;
  mutable best_visits: (int * string) array;
}

let total_minutes = 30

let print_valve {flow_rate; best_visits} = ignore best_visits; print_string " { "; print_int flow_rate; print_string " } "

let create_value_item flow_rate = {flow_rate = int_of_string flow_rate; best_visits = Array.make total_minutes (-1, "")}

let process_line line = line
|> split_on_strings ["Valve "; " has flow rate="; "; tunnels lead to valves "; "; tunnel leads to valve "]
|> List.tl |> function
  | [node_id; flow_rate; neighbors] -> (node_id, create_value_item flow_rate, split_on_string ", " neighbors)
  | _ -> failwith "Malformed line"

let print_valve_tuple = print_mixed_triple print_string print_valve (printlist print_string)

let traverse_tunnels total_valves total_minutes graph =
  ignore total_valves;
  let rec traverse_tunnels' route open_valves minutes_left pressure_released valve =
    if minutes_left = 0 then (pressure_released, route) else
    let (best_visit_pressure, _) = valve.value.best_visits.(minutes_left - 1) in
    if best_visit_pressure <= pressure_released then
    (valve.value.best_visits.(minutes_left - 1) <- (pressure_released, route);
    let open_this_valve = if valve.value.flow_rate = 0 || StringSet.mem valve.id open_valves then (-1, valve.id ^ " valve open or zero") else traverse_tunnels' (route ^ valve.id ^ "^") (StringSet.add valve.id open_valves) (minutes_left - 1) (pressure_released + valve.value.flow_rate * (minutes_left - 1)) valve in
    let all_options = List.map (traverse_tunnels' (route ^ valve.id ^ "-") (if valve.value.flow_rate = 0 then StringSet.add valve.id open_valves else open_valves) (minutes_left - 1) pressure_released) valve.neighbors in
    let (new_pressure_released, new_pressure_released_route) = max_list_by (fun (pr, _) -> pr) (-1, valve.id ^ " empty list") (open_this_valve::all_options) in
    (new_pressure_released, new_pressure_released_route)) else (-1, valve.id ^ " revisit") in
  traverse_tunnels' "=" StringSet.empty total_minutes 0 graph

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
  print_mixed_tuple print_int print_string (traverse_tunnels (List.length graph_nodes) total_minutes base_node);
  print_newline ();
  print_newline ();
  print_graph print_valve base_node;
  prerr_newline ();;
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
=30AA-29II-28JJ^27JJ567-26II567-25AA567-24DD567^23DD1027-22EE1027-21FF1027-20GG1027-19HH1027^18HH1423-17GG1423-16FF1423-15EE1423^14EE1465-13DD1465-12CC1465-11BB1465^10BB1595-9CC1595^8CC1611-

Correct:
=30AA-29DD^28DD-27CC560-26BB560^25BB885-24AA885-23II885 -22JJ885 ^21JJ1326-20II1326-19AA1326-18DD1326-17EE1326-16FF1326-15GG1326-14HH1326^13HH1612-12GG1612-11FF1612-10EE1612^9EE1639-8DD1639-7CC1639^6CC1651-
   *)