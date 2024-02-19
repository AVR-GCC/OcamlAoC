open Myutils

let lines = read_file "./inputs/day3.txt"

module SS = Set.Make(Char)

let string_to_set str =
  String.fold_left (fun acc x -> SS.add x acc) SS.empty str

let is_char_in_set set x = SS.exists (fun y -> y = x) set

let rucksack_to_item rucksack =
  let half_length = (String.length rucksack) / 2 in
  let compartment_one = String.sub rucksack 0 half_length in
  let compartment_two = String.sub rucksack half_length half_length in
  let compartment_set = string_to_set compartment_one in
  String.fold_left (fun acc x -> if is_char_in_set compartment_set x then x else acc) ' ' compartment_two

let item_to_priority it = if ((int_of_char it) < (int_of_char 'a'))
  then (int_of_char it) - (int_of_char 'A') + 27
  else (int_of_char it) - (int_of_char 'a') + 1

let items = List.map rucksack_to_item lines

let priorities = List.map item_to_priority items

let priorities_sum = sum priorities

let triplet_to_item triplet = match triplet with
  | (sack1, sack2, sack3) ->
    let set_two = string_to_set sack2 in
    let set_three = string_to_set sack3 in
    String.fold_left (fun acc x -> if is_char_in_set set_two x && is_char_in_set set_three x then x else acc) ' ' sack1

let rec lines_to_badges badges lns = match lns with
  | a::b::c::t -> lines_to_badges ((triplet_to_item (a, b, c))::badges) t
  | _ -> badges

let badges = List.map item_to_priority (lines_to_badges [] lines)

let badges_sum = sum badges

let run () =
print_string "\nThe sum of the priorities of the items in the rucksacks: ";
print_int priorities_sum;
print_string "\nThe sum of the priorities of the badges: ";
print_int badges_sum;
print_string "\n";;
