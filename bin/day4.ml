let lines = Myutils.read_file "./inputs/day4.txt"

let print_bool b = if b then print_string "true" else print_string "false"

let line_to_range_array line = line
  |> String.split_on_char ','
  |> List.map (fun range -> String.split_on_char '-' range)

let lines_to_range_arrays = List.map line_to_range_array

let edges_contained a1 a2 b1 b2 = (int_of_string a1 <= int_of_string b1 && int_of_string b2 <= int_of_string a2) || (int_of_string b1 <= int_of_string a1 && int_of_string a2 <= int_of_string b2)

let edges_overlap a1 a2 b1 b2 = int_of_string a1 <= int_of_string b2 && int_of_string b1 <= int_of_string a2

let lists_to_edges_condition condition list = match list with
  | a::b::_ -> (match a with
    | a1::a2::_ -> (match b with
      | b1::b2::_ -> condition a1 a2 b1 b2
      | _::[] -> false
      | [] -> false)
    | _::[] -> false
    | [] -> false)
  | _::[] -> false
  | [] -> false

let lists_contained = lists_to_edges_condition edges_contained

let lists_overlap = lists_to_edges_condition edges_overlap

let count pred = List.fold_left (fun acc elem -> if pred elem then acc + 1 else acc) 0

let count_contains = count lists_contained

let count_overlaps = count lists_overlap

let total_contains = lines |> lines_to_range_arrays |> count_contains

let total_overlaps = lines |> lines_to_range_arrays |> count_overlaps

let run () =  print_string "\nnumber of assigned pairs where one range fully contains the other: ";
  print_int total_contains;
  print_string "\n";
  print_string "number of assigned pairs where one range overlaps with the other: ";
  print_int total_overlaps;
  print_string "\n";;