open Myutils

let run () = print_newline ();
  print_endline "Day 20";
  print_newline ();
  let lines = read_file "./inputs/day20.test.txt" in
  let numbers = List.map int_of_string lines in
  printlist print_int numbers;
  print_newline ();
  let cycle = List.fold_left add_to_original_cycle None numbers in
  Option.iter (fun next ->
    print_cycle print_int (Some next)
  ) (get_next cycle);
  print_newline ();;
