open Myutils

let lines = read_file "./inputs/day13.txt"

type side = Int of int | Empty of int list | Sides of side list

let rec print_side = function
  | Empty _ -> print_string "Empty"
  | Int i -> print_string "Int("; print_int i; print_string ")"
  | Sides l -> print_string "Sides("; printlist print_side l; print_string ")"

let rec organize_pairs = function
  | [] -> []
  | a::b::(Empty _)::t -> (a,b)::(organize_pairs t)
  | a::b::_ -> (a,b)::[]
  | _ -> failwith "odd number of elements"

let find_closing_bracket_distance str i = let rec find_closing_bracket_distance' str dist count =
  if i >= String.length str then failwith "no closing bracket found"
  else match str.[i + dist] with
  | '[' -> find_closing_bracket_distance' str (dist + 1) (count + 1)
  | ']' -> if count = 1 then dist else find_closing_bracket_distance' str (dist + 1) (count - 1)
  | _ -> find_closing_bracket_distance' str (dist + 1) count
  in find_closing_bracket_distance' str 0 1

let replace_commas_according_to_bracket_level str = let rec replace_commas_according_to_bracket_level' str i count =
  if i >= String.length str then str
  else match str.[i] with
  | '[' -> replace_commas_according_to_bracket_level' str (i + 1) (count + 1)
  | ']' -> replace_commas_according_to_bracket_level' str (i + 1) (count - 1)
  | ',' -> replace_commas_according_to_bracket_level' (String.sub str 0 i ^ (String.make 1 (add_to_char 'A' count)) ^ String.sub str (i + 1) (String.length str - i - 1)) (i + 1) count
  | _ -> replace_commas_according_to_bracket_level' str (i + 1) count
  in replace_commas_according_to_bracket_level' str 0 0

let rec parse_side str char = if str = "" then Empty [] else match str.[0] with
  | '[' -> let trimmed = String.sub str 1 (String.length str - 2) in
    let splitted = String.split_on_char char trimmed in
    Sides (List.map (fun s -> parse_side s (add_to_char char 1)) splitted)
  | _ -> Int (int_of_string str)

let rec compare_tuple (left, right) = match left, right with
  | Sides [], Sides [] | Empty _, Sides [] | Sides [], Empty _  | Empty _, Empty _ -> 0
  | Empty _, _ | Sides [], _ -> 1
  | _, Empty _ | _, Sides [] -> -1
  | Int l, Int r -> compare r l
  | Sides l, Int r -> compare_tuple (Sides l, Sides [Int r])
  | Int l, Sides r -> compare_tuple (Sides [Int l], Sides r)
  | Sides (lh::lt), Sides (rl::rt) -> let comp = compare_tuple (lh, rl) in if comp = 0 then compare_tuple (Sides lt, Sides rt) else comp

let run () =  print_newline ();
  let replaced_commas = List.map (fun s -> replace_commas_according_to_bracket_level s) lines in
  let parsed_sides = List.map (fun s -> parse_side s 'B') replaced_commas in
  let pairs = organize_pairs parsed_sides in
  printlist (print_tuple print_side) pairs;
  print_newline ();
  print_newline ();
  let results = List.map (fun (a, b) -> compare_tuple (a, b)) pairs in
  printlist print_int results;
  print_newline ();
  print_newline ();
  let right_order_indecies = fold_lefti (fun acc v i -> if v > 0 then (i + 1)::acc else acc) [] results in
  printlist print_int right_order_indecies;
  print_newline ();
  print_newline ();
  print_int (sum right_order_indecies);
  print_newline ();;