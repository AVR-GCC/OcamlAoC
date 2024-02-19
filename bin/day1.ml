open Myutils

let number_strings_array = read_file "./inputs/day1.txt"

let numbers_array = split_list int_of_string "" number_strings_array

let min lst =
  let rec minimum cur_index min_and_index remainder = match remainder with
  | [] -> min_and_index
  | h::t -> match min_and_index with
    (min_so_far, _) -> if min_so_far > h
      then minimum (cur_index + 1) (h, cur_index) t
      else minimum (cur_index + 1) min_and_index t in
  minimum 0 (max_int, -1) lst

let sums_array = List.map sum numbers_array;;
let max_numbers = List.fold_left (
  fun acc x -> match acc with
  | (first_number, second_number, third_number) ->
    match (min [first_number; second_number; third_number]) with
    | (min_value, min_index) -> if x > min_value
      then match min_index with
      | 0 -> (x, second_number, third_number)
      | 1 -> (first_number, x, third_number)
      | 2 -> (first_number, second_number, x)
      | _ -> (first_number, second_number, third_number)
      else (first_number, second_number, third_number)
  ) (0, 0, 0)

let run () = match (max_numbers sums_array) with
| (number_one, number_two, number_three) ->
  print_string "\nThe elf carrying the most calories was carrying a total of ";
  print_int @@ max_list [number_one; number_two; number_three];
  print_string " calories\nThe top three were carrying ";
  print_int number_one;
  print_string ", ";
  print_int number_two;
  print_string " and ";
  print_int number_three;
  print_string "\nWith a total of ";
  print_int (number_one + number_two + number_three);
  print_string "\n";;
