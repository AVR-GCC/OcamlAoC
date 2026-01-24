open Myutils

let relocate_single_node (cycle_opt : int cycle_node option) size =
  match cycle_opt with
  | None -> ()
  | Some cycle ->
      let delta = cycle.value / size - (if cycle.value < 0 then 1 else 0) in
      let use_offset = cycle.value + delta in
      if use_offset mod size = 0 then () else
      let place_link = navigate_in_cycle (Some cycle) size use_offset in
      let _final = remove_from_cycle (Some cycle) in
      ignore (add_node_to_cycle place_link cycle)

let rec find_in_cycle (cycle_opt : int cycle_node option) num index print = match cycle_opt with
  | None -> None
  | Some cycle -> if cycle.value = num then Some cycle else find_in_cycle (get_next (Some cycle)) num (index + 1) print

let rec mix_cycle (cycle_list : int cycle_node option list) final_id size index =
  match cycle_list with
  | Some cycle :: rest ->
      relocate_single_node (Some cycle) size;
      if cycle.cid = final_id then () else mix_cycle rest final_id size (index + 1)
  | _ -> ()

(* The first of the list will be the next of the cycle given! *)
let list_cycle = function
  | None -> []
  | Some cycle ->
      let rec list_cycle_rec acc stop_id = function
          | None -> acc
          | Some cyc ->
              if cyc.cid = stop_id then (Some cyc :: acc) else
              list_cycle_rec (Some cyc :: acc) stop_id cyc.next in
      let reveresed = list_cycle_rec [] cycle.cid cycle.next in
      List.rev reveresed

let run () = print_newline ();
  print_endline "Day 20";
  print_newline ();
  let lines = read_file "./inputs/day20.real.txt" in
  let numbers = List.map int_of_string lines in
  let size = List.length lines in
  let last_opt = List.fold_left add_value_to_cycle None numbers in
  Option.iter (fun last ->
    let cycle_list = list_cycle (Some last) in
    let first_in_cycle = get_next last_opt in
    mix_cycle cycle_list last.cid size 0;
    let zero_node = find_in_cycle first_in_cycle 0 0 false in
    let first = navigate_in_cycle zero_node size 1000 in
    let second = navigate_in_cycle first size 1000 in
    let third = navigate_in_cycle second size 1000 in
    print_endline "grove coordinate values";
    match (first, second, third) with
    | (Some f, Some s, Some t) ->
        printlist print_int (f.value::s.value::t.value::[]);
        print_newline ();
        print_endline "final sum";
        print_int (f.value + s.value + t.value);
    | _ -> ()
  ) last_opt;
  print_newline ();;
