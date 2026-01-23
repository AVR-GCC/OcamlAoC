open Myutils

type number_boolean = {
  mutable bool: bool;
  num: int;
}

let printnb nb =
  let visited_str = if nb.bool then "mixed" else "fresh" in
  print_string ("(" ^ string_of_int nb.num ^ " - " ^ visited_str ^ ")")

let printnbc (nbc : number_boolean cycle_node) = print_int nbc.cid; print_string ": "; printnb nbc.value; print_newline ()

let print_nb_cycle (cycle_opt : number_boolean cycle_node option) =
  match cycle_opt with
  | None -> print_endline "None"
  | Some cycle ->
      let rec print_cycle_rec stop_id (cyc : number_boolean cycle_node option) = match cyc with
        | None -> print_endline "None"
        | Some node ->
            print_int node.cid;
            print_string ": ";
            printnb node.value;
            print_newline ();
            match node.next with
              | None -> () | Some next when next.cid = stop_id -> ()
              | Some next -> print_cycle_rec stop_id (Some next) in
      print_cycle_rec cycle.cid (Some cycle)

let print_cycle_area cycle_opt =
  let prev_prev = get_prev (get_prev cycle_opt) in
  let next_next = get_next (get_next cycle_opt) in
  print_endline "center:";
  print_opt printnbc (get_prev (get_prev cycle_opt));
  print_string " -> ";
  print_opt printnbc (get_prev cycle_opt);
  print_string " -> ";
  print_opt printnbc cycle_opt;
  print_string " -> ";
  print_opt printnbc (get_next cycle_opt);
  print_string " -> ";
  print_opt printnbc (get_next (get_next cycle_opt));
  print_newline ();
  print_endline "start:";
  print_opt printnbc prev_prev;
  print_string " -> ";
  print_opt printnbc (get_next prev_prev);
  print_string " -> ";
  print_opt printnbc (get_next (get_next prev_prev));
  print_string " -> ";
  print_opt printnbc (get_next (get_next (get_next prev_prev)));
  print_string " -> ";
  print_opt printnbc (get_next (get_next (get_next (get_next prev_prev))));
  print_newline ();
  print_endline "end:";
  print_opt printnbc (get_prev (get_prev (get_prev (get_prev next_next))));
  print_string " -> ";
  print_opt printnbc (get_prev (get_prev (get_prev next_next)));
  print_string " -> ";
  print_opt printnbc (get_prev (get_prev next_next));
  print_string " -> ";
  print_opt printnbc (get_prev next_next);
  print_string " -> ";
  print_opt printnbc next_next;
  print_newline ()

let relocate_single_node (cycle_opt : number_boolean cycle_node option) size =
  match cycle_opt with
  | None -> None
  | Some cycle ->
      if cycle.value.bool then cycle.next else
      let delta = cycle.value.num / size - (if cycle.value.num < 0 then 1 else 0) in
      let use_offset = cycle.value.num + delta in
      if use_offset mod size = 0 then (cycle.value.bool <- true; cycle.next) else (

      let place_link = navigate_in_cycle (Some cycle) size use_offset in
      let final = remove_from_cycle (Some cycle) in
      cycle.value.bool <- true;
      let _added = add_node_to_cycle place_link cycle in
      final
      )

let rec find_in_cycle (cycle_opt : number_boolean cycle_node option) num index print = match cycle_opt with
  | None -> None
  | Some cycle -> if cycle.value.num = num then Some cycle else find_in_cycle (get_next (Some cycle)) num (index + 1) print

let rec mix_cycle (cycle_opt : number_boolean cycle_node option) final_id size index =
  match cycle_opt with
  | None -> None
  | Some cycle ->
      let _zero_node = find_in_cycle cycle_opt 0 0 in
      let next_cycle = relocate_single_node (Some cycle) size in
      if cycle.cid = final_id then next_cycle else mix_cycle next_cycle final_id size (index + 1)

let run () = print_newline ();
  print_endline "Day 20";
  print_newline ();
  let lines = read_file "./inputs/day20.real.txt" in
  let numbers = List.map int_of_string lines in
  let number_bools = List.map (fun (num) -> { bool = false; num = num }) numbers in
  let size = List.length lines in
  let last_opt = List.fold_left add_value_to_cycle None number_bools in
  Option.iter (fun last ->
    let first_in_cycle = get_next last_opt in
    let mixed = get_next (mix_cycle first_in_cycle last.cid size 0) in
    let zero_node = find_in_cycle mixed 0 0 false in
    let first = navigate_in_cycle zero_node size 1000 in
    let second = navigate_in_cycle first size 1000 in
    let third = navigate_in_cycle second size 1000 in
    (* let first = navigate_in_cycle zero_node size 1000 in *)
    (* let second = navigate_in_cycle zero_node size 2000 in *)
    (* let third = navigate_in_cycle zero_node size 3000 in *)
    print_endline "grove coordinate values";
    match (first, second, third) with
    | (Some f, Some s, Some t) ->
        printlist print_int (f.value.num::s.value.num::t.value.num::[]);
        print_newline ();
        print_endline "final sum";
        print_int (f.value.num + s.value.num + t.value.num);
    | _ -> ()
  ) last_opt;
  print_newline ();;
