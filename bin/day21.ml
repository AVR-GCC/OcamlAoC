open Myutils

type operator = Plus | Minus | Mul | Div
type operand = Num of int | Str of string

type operation = {
  id: string;
  left: operand;
  right: operand;
  operator: operator;
}

type value = {
  id: string;
  number: int;
}

type statement = Op of operation | Val of value

let get_op = function
  | "+" -> Plus
  | "-" -> Minus
  | "*" -> Mul
  | "/" -> Div
  | x -> failwith ("bad operator: " ^ x)

let string_of_op = function
  | Plus -> "+"
  | Minus -> "-"
  | Mul -> "*"
  | Div -> "/"

let do_op left right = function
  | Plus -> left + right
  | Minus -> left - right
  | Mul -> left * right
  | Div -> left / right

let string_of_operand = function
  | Num x -> string_of_int x
  | Str s -> s

let print_statement = function
  | Op { id; left; right; operator } ->
    print_endline (id ^ ": " ^ (string_of_operand left) ^ " " ^ (string_of_op operator) ^ " " ^ (string_of_operand right))
  | Val { id; number } ->
    print_endline (id ^ ": " ^ (string_of_int number))

let line_to_statement line = line
  |> split_on_strings [": "; " "]
  |> function
    | [id; number] ->
      Val { id = id; number = int_of_string number }
    | [id; left; op; right] ->
      Op { id = id; left = Str left; right = Str right; operator = get_op op }
    | _ -> failwith "bad line!"

let rec add_statement map statement =
  match statement with
  | Val { id; number } -> (
      match StringMap.find_opt id map with
      | None -> StringMap.add id (Val { id; number }) map
      | Some Op { id = rec_id; left; right; operator } -> (
          match (left, right) with
          | (Str ls, Str rs) when ls = id -> StringMap.add id (Op { id = rec_id; left = Num number; right = Str rs; operator = operator }) map
          | (Str ls, Str rs) when rs = id -> StringMap.add id (Op { id = rec_id; left = Str ls; right = Num number; operator = operator }) map
          | (Str ls, Num rs) when ls = id -> add_statement map (Val { id = rec_id; number = do_op number rs operator })
          | (Num ls, Str rs) when rs = id -> add_statement map (Val { id = rec_id; number = do_op ls number operator })
          | _ -> failwith "Operation statement with two integers")
      | _ -> failwith "Found double value statement")
  | Op { left; right; _ } -> (
      match (left, right) with
      | (Str ls, Str rs) -> map |> StringMap.add ls statement |> StringMap.add rs statement
      | _ -> failwith "start operation statement with an integer")

let run () = print_newline ();
  print_endline "Day 20";
  print_newline ();
  let lines = read_file "./inputs/day21.test.txt" in
  let statements = List.map line_to_statement lines in
  printlist print_statement statements;
  print_newline ();
  let final_map = List.fold_left add_statement StringMap.empty statements in
  print_string_map print_statement final_map;
  (* let root = StringMap.find "root" final_map in *)
  (* print_statement root; *)
  print_newline ();;
