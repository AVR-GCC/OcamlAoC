open Myutils

let lines = read_file "./inputs/day11.txt"

let monkeys_string_lists = List.rev @@ List.map List.rev @@ List.map (List.map String.trim) @@ split_list (fun x -> x) "" lines

type monkey = {
  operation: int -> int;
  divider: int;
  throw_to: bool -> int;
}

let process_starting_items_string starting_items_string = 
  match String.split_on_char ':' starting_items_string with
    | "Starting items" :: starting_items_str :: [] -> 
      List.map (fun s -> int_of_string (String.trim s)) @@ String.split_on_char ',' (String.trim starting_items_str)
    | _ -> failwith "Invalid starting items string"

let process_operation_string operation_string = 
  match String.split_on_char ':' operation_string with
    | "Operation" :: operation_str :: [] -> 
      let splitted = String.split_on_char ' ' operation_str in
      let operation = match splitted with
        | _ :: "new" :: "=" :: "old" :: "*" :: "old" :: [] -> 
          (fun old -> old * old)
        | _ :: "new" :: "=" :: "old" :: "*" :: multiplier_str :: [] -> 
          let multiplier = int_of_string multiplier_str in
          (fun old -> old * multiplier)
        | _ :: "new" :: "=" :: "old" :: "+" :: adder_str :: [] -> 
          let adder = int_of_string adder_str in
          (fun old -> old + adder)
        | _ -> print_endline ("failed parsing operation string: " ^ operation_str); failwith "Invalid operation string"
      in
      operation
    | _ -> print_endline ("failed parsing operation string: " ^ operation_string); failwith "Invalid operation string"

let process_divider_string test_string = 
  match String.split_on_char ':' test_string with
    | "Test" :: test_str :: [] -> 
      let test = match String.split_on_char ' ' test_str with
        | _ :: "divisible" :: "by" :: divisor_str :: [] -> int_of_string divisor_str
        | _ -> print_endline ("failed parsing test string: " ^ test_string); failwith "Invalid test string"
      in
      test
    | _ -> print_endline ("failed parsing test string: " ^ test_string); failwith "Invalid test string"

let process_throw_to_strings throw_to_strings = match throw_to_strings with
  | true_str :: false_str :: [] -> (match (String.split_on_char ' ' true_str, String.split_on_char ' ' false_str) with
    | "If" :: "true:" :: "throw" :: "to" :: "monkey" :: throw_to_true_str :: [], "If" :: "false:" :: "throw" :: "to" :: "monkey" :: throw_to_false_str :: [] -> 
      let throw_to_true = int_of_string throw_to_true_str in
      let throw_to_false = int_of_string throw_to_false_str in
      (fun b -> if b then throw_to_true else throw_to_false)
    | _ -> failwith "Invalid throw to strings")
  | _ -> failwith "Invalid throw to strings"
  

let process_monkey_string_list string_list = match string_list with
  | _ :: starting_items_str :: operation_str :: test_str :: throw_to_true :: throw_to_false :: [] ->
    let operation = process_operation_string operation_str in
    let divider = process_divider_string test_str in
    let throw_to = process_throw_to_strings [throw_to_true; throw_to_false] in
    let starting_items = process_starting_items_string starting_items_str in
    ({ operation = operation; divider = divider; throw_to = throw_to }, starting_items)
  | _ -> failwith "Invalid list"

type monkey_list_type = { mutable cur: monkey list }
type items_list_type = { mutable cur: int list array }
type items_inspected_type = { mutable cur: int array }

let monkey_list: monkey_list_type = { cur = [] }
let items_list: items_list_type = { cur = [||] }
let items_inspected: items_inspected_type = { cur = [||] }

let init_monkeys string_lists = 
  let monkeys_with_starting_items = List.map process_monkey_string_list string_lists in
  let monkeys = List.map (fun (monkey, _) -> monkey) monkeys_with_starting_items in
  let starting_inspected = Array.make (List.length monkeys) 0 in
  let starting_items_arr = Array.make (List.length monkeys) [] in
  List.iteri (fun i (_, starting_items) -> starting_items_arr.(i) <- List.rev starting_items) monkeys_with_starting_items;
  items_inspected.cur <- starting_inspected; monkey_list.cur <- monkeys; items_list.cur <- starting_items_arr

let print_monkey start_number monkey = 
  print_endline @@ "Monkey " ^ (string_of_int start_number) ^ ": ";
  print_string "Operation: ";
  let after_operation = monkey.operation start_number in
  print_int after_operation;
  print_newline ();
  let test_result = (after_operation mod monkey.divider) == 0 in
  print_string "Test: ";
  print_string @@ if test_result then "true" else "false";
  print_newline ();
  print_string "Throw to: ";
  print_int @@ monkey.throw_to test_result;
  print_newline ()

let monkey_inspect_item op monkey index item =
  let monkey_op_done = monkey.operation item in
  let monkey_bored_done = op monkey_op_done in
  let monkey_test_done = monkey_bored_done mod monkey.divider == 0 in
  let monkey_throw_to = monkey.throw_to monkey_test_done in
  items_list.cur.(monkey_throw_to) <- monkey_bored_done :: items_list.cur.(monkey_throw_to);
  items_inspected.cur.(index) <- items_inspected.cur.(index) + 1

let do_single_monkey_round total_mod i monkey = 
  List.iter (monkey_inspect_item (fun x -> x mod total_mod) monkey i) (List.rev items_list.cur.(i));
  items_list.cur.(i) <- []

let print_full_round i = 
  print_newline ();
  print_endline ("After round " ^ (string_of_int i) ^ " the monkeys are holding items with these worry levels:");
  Array.iteri (fun j monkey_items -> 
    print_string ("Monkey " ^ (string_of_int j) ^ ": ");
    printlist print_int (List.rev monkey_items); print_newline ()) items_list.cur

let do_full_round total_mod j = 
  List.iteri (fun i monkey -> do_single_monkey_round total_mod i monkey) monkey_list.cur; print_full_round j

let do_x_rounds total_mod total = let rec do_x_rounds' x =
  if x > 0 then (do_full_round total_mod (total - x + 1); do_x_rounds' (x - 1)) in
  do_x_rounds' total

let two_max_items_of_array arr = 
  let rec two_max_items_of_array' arr max1 max2 i =
    if i < Array.length arr then
      if arr.(i) > max1 then two_max_items_of_array' arr arr.(i) max1 (i + 1)
      else if arr.(i) > max2 then two_max_items_of_array' arr max1 arr.(i) (i + 1)
      else two_max_items_of_array' arr max1 max2 (i + 1)
    else (max1, max2) in
  two_max_items_of_array' arr 0 0 0

let get_max_int lst = List.fold_left (fun acc x -> if x > acc then x else acc) 0 lst
let run () =
  print_newline ();
  printlist (printlist print_string) monkeys_string_lists;
  print_newline ();
  init_monkeys monkeys_string_lists;
  print_newline ();
  print_endline "Starting items:";
  print_full_round 0;
  print_newline ();
  do_x_rounds (List.fold_left (fun acc x -> x * acc) 1 (List.map (fun x -> x.divider) monkey_list.cur)) 10000;
  print_newline ();
  print_endline "Number of items inspected by each monkey:";
  print_array print_int items_inspected.cur;
  print_newline ();
  print_newline ();
  print_endline "Max two active monkeys:";
  let top_monkeys = two_max_items_of_array items_inspected.cur in
  print_tuple print_int top_monkeys;
  print_newline ();
  print_newline ();
  print_endline "Monkey business:";
  let monkey_business = match top_monkeys with (x, y) -> x * y in
  print_int monkey_business;
  print_newline ();;