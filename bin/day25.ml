open Myutils

let convert_digit = function
  | "=" -> -2
  | "-" -> -1
  | dig -> int_of_string dig

let rec snafu_to_decimal lst =
  match lst with
  | [] -> 0
  | dig :: t -> convert_digit dig + 5 * snafu_to_decimal t

let run () = print_newline ();
  print_endline "Day 25";
  let lines = read_file "./inputs/day25.test.txt" in
  let exploded = List.map explode lines in
  let reved = List.map List.rev exploded in
  let decimals = List.map snafu_to_decimal reved in
  printlist print_int decimals;
  print_newline ();
  print_newline ();;
