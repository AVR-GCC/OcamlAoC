open Myutils

let lines = read_file "./inputs/day10.txt"

let do_line line (prev_x, cycle) = let parts = String.split_on_char ' ' line in
  match parts with
  | "addx" :: n :: [] -> (prev_x + int_of_string n, cycle + 2)
  | _ -> (prev_x, cycle + 1)

let machine_states = List.rev @@ List.fold_left (fun acc elem -> (do_line elem (List.hd acc))::acc) [(1, 0)] lines

let rec values_at_cycles cycles_to_sample states acc prev_x = match (cycles_to_sample, states) with
  | ([], _) | (_, []) -> acc
  | (sample_cycle::cycles_tale, (x, cycle)::states_tale) -> if cycle >= sample_cycle then values_at_cycles cycles_tale states ((prev_x, sample_cycle)::acc) x else values_at_cycles cycles_to_sample states_tale acc x

let selected_states = List.rev @@ values_at_cycles [20; 60; 100; 140; 180; 220] machine_states [] 1

let signal_strengths = List.map (fun (x, cycle) -> x * cycle) selected_states

let rec list_numbers_to n = if n <= 0 then [] else n::(list_numbers_to (n - 1))

let all_states_to_240 = List.rev @@ values_at_cycles (List.rev (list_numbers_to 240)) machine_states [] 1


let print_crt_thing states = List.iter (fun (x, cycle) -> let index = (cycle mod 40) in if index + 1 = x || index = x || index - 1 = x then print_string "#" else print_string "."; if index = 0 then print_newline ()) states

let run () =
  print_newline ();
  print_newline ();
  print_endline "Machine states:";
  printlist (print_tuple print_int) machine_states;
  print_newline ();
  print_newline ();
  print_endline "States during selected cycles:";
  printlist (print_tuple print_int) selected_states;
  print_newline ();
  print_newline ();
  print_endline "Signal strengths:";
  printlist print_int signal_strengths;
  print_newline ();
  print_newline ();
  print_endline "Sum of signal strengths:";
  print_int (sum signal_strengths);
  print_newline ();
  print_newline ();
  print_endline "All states to 240:";
  printlist (print_tuple print_int) all_states_to_240;
  print_newline ();
  print_newline ();
  print_endline "CRT:";
  print_crt_thing all_states_to_240;
  print_newline ();;