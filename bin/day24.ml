open Myutils

let run () = print_newline ();
  print_endline "Day 24";
  let lines = read_file "./inputs/day24.example.txt" in
  let exploded = List.map explode lines in
  print_newline ();
  printlist (fun x -> printlist print_string x; print_newline ()) exploded;
  print_newline ();;
