open Myutils

let convert_digit_snafu = function
  | "=" -> -2
  | "-" -> -1
  | dig -> int_of_string dig

let convert_digit_decimal = function
  | 1 -> "1"
  | 2 -> "2"
  | 3 -> "="
  | 4 -> "-"
  | _ -> "0"


let rec snafu_to_decimal lst =
  match lst with
  | [] -> 0
  | dig :: t -> convert_digit_snafu dig + 5 * snafu_to_decimal t

let decimal_to_snafu num =
  let rec decimal_to_snafu_rec carry num =
    let current_remainder = num mod 5 + carry in
    let current_digit = convert_digit_decimal current_remainder in
    let next_carry = if current_remainder > 2 then 1 else 0 in
    let next_num = num / 5 in
    if next_num = 0 then (if next_carry = 1 then [current_digit; "1"] else [current_digit]) else current_digit :: decimal_to_snafu_rec next_carry next_num in
  List.rev @@ decimal_to_snafu_rec 0 num

let run () = print_newline ();
  print_endline "Day 25";
  let lines = read_file "./inputs/day25.real.txt" in
  let exploded = List.map explode lines in
  let reved = List.map List.rev exploded in
  let decimals = List.map snafu_to_decimal reved in
  let dec_sum = sum decimals in
  let snafu_sum = decimal_to_snafu dec_sum in
  List.iter print_string snafu_sum;
  print_newline ();
  print_newline ();;
