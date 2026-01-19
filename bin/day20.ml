open Myutils

let relocate_single_node cycle_opt size =
  match cycle_opt with
  | None -> None
  | Some cycle ->
      let place_link = navigate_in_cycle (Some cycle) size cycle.value in
      (* print_endline "place_link"; *)
      (* print_cycle print_int place_link; *)
      let final = remove_from_cycle (Some cycle) in
      ignore (add_node_to_cycle place_link cycle);
      (* print_newline (); *)
      (* print_endline "added"; *)
      (* print_cycle print_int (Some added); *)
      (* print_newline (); *)
      final

let run () = print_newline ();
  print_endline "Day 20";
  print_newline ();
  let lines = read_file "./inputs/day20.test.txt" in
  let numbers = List.map int_of_string lines in
  let size = List.length lines in
  printlist print_int numbers;
  print_newline ();
  let last = List.fold_left add_value_to_cycle None numbers in
  Option.iter (fun cycle ->
    print_cycle print_int (Some cycle);
    print_newline ();
    print_cycle print_int (relocate_single_node (Some cycle) size);
    (* print_cycle print_int (remove_from_cycle (Some cycle)); *)
  ) (get_next last);
  print_newline ();;
