open Myutils

let lines = read_file "./inputs/day5.txt"

let all_lines_to_original_stack_and_moves_arrays lines =
  let list = split_list (fun x -> x) "" lines in
  match list with
  | h::n::_ -> (n, h)
  | _::[] -> ([], [])
  | _ -> ([], [])

(* let two_arrays = split_list (fun x -> x) "" (str_to_lines test_string) *)
let (stacks_and_numbers, moves) = all_lines_to_original_stack_and_moves_arrays lines

let num_stacks = ((match stacks_and_numbers with
  | h::_ -> h |> String.trim |> String.split_on_char ' ' |> List.length
  | [] -> 0) + 2) / 3

let stacks_strings = match stacks_and_numbers with
  | _::tl -> tl
  | [] -> []

let str_to_crate_level_list str =
    let exploded = explode str in
    let rec triplets acc list = match list with
      | _::b::_::_::tl -> triplets (b::acc) tl
      | _::b::_::[] -> b::acc
      | _ -> acc in
    triplets [] exploded

let crate_level_list = List.map str_to_crate_level_list stacks_strings

let rec do_level acc level = match (acc, level) with
  | (accumulator_head::accumulator_tail, level_head::level_tail) ->
    if level_head = " "
      then accumulator_head::(do_level accumulator_tail level_tail)
      else (level_head::accumulator_head)::(do_level accumulator_tail level_tail)
  | (_, _) -> acc

let do_levels = List.fold_left do_level (List.init num_stacks (fun _ -> []))

let stacks_list = List.rev (do_levels crate_level_list)

let move_to_triplet move_string = match String.split_on_char ' ' move_string with
  | _::crates::_::stack_from::_::stack_to::[] -> (int_of_string crates, int_of_string stack_from - 1, int_of_string stack_to - 1)
  | _ -> (0, 0, 0)

let move_triplets = List.rev (List.map move_to_triplet moves)

let print_triplet prnt trip = match trip with
  | (one, two, three) -> print_string "("; prnt one; print_string ", "; prnt two; print_string ", "; prnt three; print_string ")"

let print_int_triplet = print_triplet print_int

let cut_n list n =
  let rec cut_n_rec acc lst k = if k = 0 then (List.rev acc, lst) else match lst with
    | h::t -> cut_n_rec (h::acc) t (k - 1)
    | _ -> (List.rev acc, lst) in
  cut_n_rec [] list n

let print_stack = printlist print_string

let print_stacks = printlist print_stack

let tl lst = match lst with
  | [] -> []
  | _ -> List.tl lst

let make_move one_by_one stacks move = match move with
  | (crates, stack_from, stack_to) ->
    let from_stack = List.nth stacks stack_from in
    let to_stack = List.nth stacks stack_to in
    let (to_additional_blocks, new_from) = cut_n from_stack crates in
    let to_addition = if one_by_one then List.rev to_additional_blocks else to_additional_blocks in
    let new_to = List.concat [to_addition; to_stack] in
    if stack_from < stack_to then
      let (to_from, from_from) = cut_n stacks stack_from in
      let (between, from_to) = cut_n from_from (stack_to - stack_from) in
      let new_stacks = to_from @ (new_from :: (tl between)) @ (new_to :: (tl from_to)) in
      new_stacks
    else
      let (to_to, from_to) = cut_n stacks stack_to in
      let (between, from_from) = cut_n from_to (stack_from - stack_to) in
      let new_stacks = to_to @ (new_to :: (tl between)) @ (new_from :: (tl from_from)) in
      new_stacks

let make_move_9000 = make_move true

let final_stacks_9000 = List.fold_left make_move_9000 stacks_list move_triplets

let final_message_9000 = String.concat "" (List.map List.hd final_stacks_9000)

let make_move_9001 = make_move false

let final_stacks_9001 = List.fold_left make_move_9001 stacks_list move_triplets

let final_message_9001 = String.concat "" (List.map List.hd final_stacks_9001)

let run () =
  print_string "\nStart stacks:\n";
  print_stacks stacks_list;
  print_string "\nDoing moves:\n";
  printlist print_int_triplet move_triplets;
  print_string "\nFinal stacks 9000:\n";
  print_stacks final_stacks_9000;
  print_string "\nFinal message 9000:\n";
  print_string final_message_9000;
  print_string "\nFinal stacks 9001:\n";
  print_stacks final_stacks_9001;
  print_string "\nFinal message 9001:\n";
  print_string final_message_9001;
  print_string "\n";;