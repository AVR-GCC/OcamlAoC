open Myutils

let lines = read_file "./inputs/day16test.txt"

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

let split_double_visit {visited; opened; opened_valve; pressure_released; valves} = 
  let (opened_valve1, opened_valve2) = opened_valve in
  let (valve1, valve2) = valves in
  let visit1 = {visited = visited; opened = opened; opened_valve = opened_valve1; pressure_released = pressure_released; valve = valve1} in
  let visit2 = {visited = visited; opened = opened; opened_valve = opened_valve2; pressure_released = pressure_released; valve = valve2} in
  (visit1, visit2)

let merge_visits original_pressure_released {visited = visited1; opened = opened1; opened_valve = opened_valve1; pressure_released = pressure_released1; valve = valve1}
    {visited = visited2; opened = opened2; opened_valve = opened_valve2; pressure_released = pressure_released2; valve = valve2} =
  let new_visited = StringMap.merge (fun _ v1 v2 -> match v1, v2 with
    | None, None -> None
    | Some x, None -> Some x
    | None, Some y -> Some y
    | Some x, Some y -> Some (max x y)) visited1 visited2 in
  let new_opened = StringSet.union opened1 opened2 in
  let new_opened_valve = (opened_valve1, opened_valve2) in
  let new_pressure_released = pressure_released1 + pressure_released2 - original_pressure_released in
  let new_valves = (valve1, valve2) in
  {visited = new_visited; opened = new_opened; opened_valve = new_opened_valve; pressure_released = new_pressure_released; valves = new_valves}

let count_flow_valves base_node = 
  let rec count_flow_valves' visited count (node, _) =
    if StringSet.mem node.id visited then count else
    let new_visited = StringSet.add node.id visited in
    let new_count = if node.value.flow_rate = 0 then count else count + 1 in
    sum (List.map (count_flow_valves' new_visited new_count) node.neighbors) in
  count_flow_valves' StringSet.empty 0 (base_node, 0)

let open_valve minutes {visited; opened; pressure_released; valve; _} =
  let new_opened = StringSet.add valve.id opened in
  let new_pressure_released = pressure_released + valve.value.flow_rate * minutes in
  let new_visited = StringMap.update valve.id (fun _ -> Some new_pressure_released) visited in
  {visited = new_visited; opened = new_opened; opened_valve = true; pressure_released = new_pressure_released; valve = valve}

let move_to_valve {visited; opened; pressure_released; valve; _} (new_valve, _) =
  let new_visited = StringMap.update valve.id (fun _ -> Some pressure_released) visited in
  {visited = new_visited; opened = opened; opened_valve = false; pressure_released = pressure_released; valve = new_valve}

let traverse_tunnels total_minutes num_flow_valves start_visit =
  let rec traverse_tunnels' minutes_left visit =
    let {visited; opened; opened_valve; pressure_released; valve} = visit in
    let last_visit = match StringMap.find_opt valve.id visited with None -> -1 | Some prev_visit -> prev_visit in
    let revisit = not opened_valve && last_visit >= pressure_released in
    if revisit then -1 else
    let finished = minutes_left = 0 || StringSet.cardinal opened = num_flow_valves in
    if finished then pressure_released else
    let move_inputs = List.map (move_to_valve visit) valve.neighbors in
    let dont_try_open_this_valve = opened_valve || valve.value.flow_rate = 0 || StringSet.mem valve.id opened in
    let minutes = minutes_left - 1 in
    let all_inputs = if dont_try_open_this_valve then move_inputs else (open_valve minutes visit)::move_inputs in
    let all_options = List.map (traverse_tunnels' minutes) all_inputs in
    max_list all_options in
  traverse_tunnels' total_minutes start_visit

let leftward_double_visit {valves; _;} =
  let (valve1, valve2) = valves in
  valve1.id >= valve2.id
(*
let traverse_tunnels_double total_minutes graph =
  let num_flow_valves = count_flow_valves graph in
  let rec traverse_tunnels' minutes_left visit =
    let {visited; opened; opened_valve; pressure_released; valves} = visit in
    let (opened_valve1, opened_valve2) = opened_valve in
    let (valve1, valve2) = valves in
    let last_visit1 = match StringMap.find_opt valve1.id visited with None -> -1 | Some prev_visit -> prev_visit in
    let last_visit2 = match StringMap.find_opt valve2.id visited with None -> -1 | Some prev_visit -> prev_visit in
    let revisit1 = not opened_valve1 && last_visit1 >= pressure_released in
    let revisit2 = not opened_valve2 && last_visit2 >= pressure_released in
    if revisit1 || revisit2 then -1 else
    let finished = minutes_left = 0 || StringSet.cardinal opened = num_flow_valves in
    if finished then pressure_released else
    let (visit1, visit2) = split_double_visit visit in
    let move_inputs1 = List.map (move_to_valve visit1) valve1.neighbors in
    let move_inputs2 = List.map (move_to_valve visit2) valve2.neighbors in
    let dont_try_open_this_valve1 = opened_valve1 || valve1.value.flow_rate = 0 || StringSet.mem valve1.id opened in
    let dont_try_open_this_valve2 = opened_valve2 || valve2.value.flow_rate = 0 || StringSet.mem valve2.id opened in
    let minutes = minutes_left - 1 in
    let trying_to_open_same_valve = valve1.id = valve2.id && not dont_try_open_this_valve1 && not dont_try_open_this_valve2 in
    let open1 = open_valve minutes visit1 in
    let open2 = open_valve minutes visit2 in
    let all_inputs1 = if dont_try_open_this_valve1 then move_inputs1 else open1::move_inputs1 in
    let all_inputs2 = if dont_try_open_this_valve2 || trying_to_open_same_valve then move_inputs2 else open2::move_inputs2 in
    let all_inputs = cartesian_product (merge_visits pressure_released) all_inputs1 all_inputs2 in
    let inputs_without_replicates = if valve1.id = valve2.id then List.filter (leftward_double_visit) all_inputs else all_inputs in
    let all_options_without_sleep = List.map (traverse_tunnels' minutes) inputs_without_replicates in
    let sleep1 = traverse_tunnels minutes num_flow_valves visit1 in
    let sleep2 = traverse_tunnels minutes num_flow_valves visit2 in
    let all_options = pressure_released::sleep1::sleep2::all_options_without_sleep in
    let final_result = max_list all_options in
    final_result in
  traverse_tunnels' total_minutes {visited = StringMap.empty; opened = StringSet.empty; opened_valve = (false, false); pressure_released = 0; valves = (graph, graph)}
*)

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
  print_int (traverse_tunnels 30 (count_flow_valves base_node) {visited = StringMap.empty; opened = StringSet.empty; opened_valve = false; pressure_released = 0; valve = base_node});
  print_newline ();
  print_newline ();
  print_int (traverse_tunnels_double 26 base_node);
  print_newline ();
  print_newline ();;