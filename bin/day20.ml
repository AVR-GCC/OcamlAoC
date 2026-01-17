open Myutils

let run () = print_newline ();
  print_endline "Day 20";
  print_newline ();
  let lines = read_file "./inputs/day20.test.txt" in
  printlist print_endline lines;
  print_newline ();;
