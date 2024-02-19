open Myutils
let lines = read_file "./inputs/day2.txt"
let line_to_game_wrong line = let tuple_to_result = function 
  | (a, 'X') -> 1 + (match a with
    | 'A' -> 3
    | 'B' -> 0
    | _ -> 6)
  | (a, 'Y') -> 2 + (match a with
    | 'A' -> 6
    | 'B' -> 3
    | _ -> 0)
  | (a, _) -> 3 + match a with
    | 'A' -> 0
    | 'B' -> 6
    | _ -> 3 in
  tuple_to_result (line.[0], line.[2])

let line_to_game line = let tuple_to_result = function 
  | (a, 'X') -> 0 + (match a with
    | 'A' -> 3
    | 'B' -> 1
    | _ -> 2)
  | (a, 'Y') -> 3 + (match a with
    | 'A' -> 1
    | 'B' -> 2
    | _ -> 3)
  | (a, _) -> 6 + match a with
    | 'A' -> 2
    | 'B' -> 3
    | _ -> 1 in
  tuple_to_result (line.[0], line.[2])

let game_results_wrong = List.map line_to_game_wrong lines;;
let game_results = List.map line_to_game lines

let run () = print_string "\nThe result given my original interpretation is: ";
print_int (sum game_results_wrong);
print_string "\nThe result given the correct interpretation is: ";
print_int (sum game_results);
print_string "\n";;
