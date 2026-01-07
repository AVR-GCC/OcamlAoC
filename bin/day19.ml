open Myutils

let run () = print_newline ();
  print_endline "Day 19";
  print_newline ();
  let lines = read_file "./inputs/day19.test.txt" in
  printlist print_endline lines;
  print_newline ();;
