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

let flip_op = function
  | Plus -> Minus
  | Minus -> Plus
  | Mul -> Div
  | Div -> Mul

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

let print_operation { id; left; right; operator } = print_endline (id ^ ": " ^ (string_of_operand left) ^ " " ^ (string_of_op operator) ^ " " ^ (string_of_operand right))

let print_statement = function
  | Op op -> print_operation op
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

let rec add_value values_map operations_map { id; number } =
  let temp_values_map = StringMap.add id number values_map in
  match StringMap.find_opt id operations_map with
    | None -> (temp_values_map, operations_map)
    | Some { id = oid; left; right; operator } ->
        match (left, right) with
        | (Str ls, Str rs) when ls = id ->
            let new_operation = { id = oid; left = Num number; right = Str rs; operator = operator } in
            let temp_operations_map = operations_map |> StringMap.remove id |> StringMap.add rs new_operation in
            (temp_values_map, temp_operations_map)
        | (Str ls, Str rs) when rs = id ->
            let new_operation = { id = oid ; left = Str ls; right = Num number; operator = operator } in
            let temp_operations_map = operations_map |> StringMap.remove id |> StringMap.add ls new_operation in
            (temp_values_map, temp_operations_map)
        | (Str ls, Num rn) when ls = id ->
            let new_value = { id = oid; number = do_op number rn operator } in
            let temp_operations_map = operations_map |> StringMap.remove id in
            add_value temp_values_map temp_operations_map new_value
        | (Num ln, Str rs) when rs = id ->
            let new_value = { id = oid; number = do_op ln number operator } in
            let temp_operations_map = operations_map |> StringMap.remove id in
            add_value temp_values_map temp_operations_map new_value
        | _ -> failwith "Operation statement with two integers"

let add_operation values_map operations_map { id; left; right; operator } =
  match (left, right) with
     | (Str ls, Str rs) -> (
        match (StringMap.find_opt ls values_map, StringMap.find_opt rs values_map) with
        | (Some ln, Some rn) ->
            let number = do_op ln rn operator in
            add_value values_map operations_map { id = id; number = number }
        | (Some ln, None) ->
            let new_operation = { id = id; left = Num ln; right = right; operator = operator } in
            (values_map, StringMap.add rs new_operation operations_map)
        | (None, Some rn) ->
            let new_operation = { id = id; left = left; right = Num rn; operator = operator } in
            (values_map, StringMap.add ls new_operation operations_map)
        | (None, None) ->
            let new_operation = { id = id; left = left; right = right; operator = operator } in
            (values_map, operations_map |> StringMap.add ls new_operation |> StringMap.add rs new_operation)
    )
    | (Str ls, Num rn) -> (
      match StringMap.find_opt ls values_map with
      | Some ln ->
        let number = do_op ln rn operator in
        add_value values_map operations_map { id = id; number = number }
      | None -> 
        let new_operation = { id = id; left = left; right = Num rn; operator = operator } in
        (values_map, StringMap.add ls new_operation operations_map)
    )
    | (Num ln, Str rs) -> (
      match StringMap.find_opt rs values_map with
      | Some rn ->
        let number = do_op ln rn operator in
        add_value values_map operations_map { id = id; number = number }
      | None -> 
        let new_operation = { id = id; left = Num ln; right = right; operator = operator } in
        (values_map, StringMap.add rs new_operation operations_map)
    )
    | _ -> failwith "Operation statement with two integers"


let add_statement (values_map, operations_map) statement =
  match statement with
  | Val value -> add_value values_map operations_map value
  | Op operation -> add_operation values_map operations_map operation

let flip_op_map map =
  StringMap.fold (fun key { id; left; right; operator } acc ->
    StringMap.add id { id = key; left = left; right = right; operator = operator } acc
  ) map StringMap.empty

let rec calc_back op_id num map =
  let { id; left; right; operator } = StringMap.find op_id map in
  match (left, right) with
  | (Str ls, Num rn) ->
      let new_num = do_op num rn (flip_op operator) in
      if ls = "humn" then new_num else calc_back id new_num map
  | (Num ln, Str rs) -> (
      match operator with
      | Plus | Mul ->
        let new_num = do_op num ln (flip_op operator) in
        if rs = "humn" then new_num else calc_back id new_num map
      | _ -> 
        let new_num = do_op ln num operator in
        if rs = "humn" then new_num else calc_back id new_num map
  )
  | _ -> failwith "bad operation"

let part1 statements =
  let (values_map, _) = List.fold_left add_statement (StringMap.empty, StringMap.empty) statements in
  let root = StringMap.find "root" values_map in
  print_int root

let part2 statements =
  let filtered = List.filter (fun statement ->
    match statement with
    | Val { id; _ } -> id <> "humn"
    | _ -> true
  ) statements in
  let (_, op_map) = List.fold_left add_statement (StringMap.empty, StringMap.empty) filtered in
  let flipped = flip_op_map op_map in
  let { left; right; _ } = StringMap.find "root" flipped in
  let final = match (left, right) with
  | (Str s, Num n) | (Num n, Str s) ->
      calc_back s n flipped
  | _ -> failwith "bad operation" in
  print_int final;
  print_newline ()

let run () = print_newline ();
  print_endline "Day 21";
  print_newline ();
  let lines = read_file "./inputs/day21.real.txt" in
  let statements = List.map line_to_statement lines in
  part2 statements;
  print_newline ();;
