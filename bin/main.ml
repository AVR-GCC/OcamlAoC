let test_string = "2214
58854

118771
33221
44333

14433
33323
13

32432
34445";;

let rec map fn arr = match arr with
[] -> []
| h::t -> fn h :: map fn t;;

let rec implode chars =
  match chars with
    [] -> ""
    | h::t -> h ^ (implode t);;

let string_of_char c = String.make 1 c;;

let explode str =
  let rec expl curarr index =
    if index = String.length str
    then curarr
    else expl (curarr @ [string_of_char str.[index]]) (index + 1) in
  expl [] 0;;

(* let printlist printelem lst = let rec middle = function
| [] -> ()
| h::t -> printelem h; if t = [] then () else print_string "; "; middle t in
print_string "["; middle lst; print_string "]";; *)

let split str delimiter =
  let rec splt curstring curarr strarr = match strarr with
  [] -> curarr@[curstring]
  | h::t -> if h = delimiter
    then splt [] (curarr@[curstring]) t
    else splt (curstring@[h]) curarr t in
  splt [] [] str;;

let number_strings_array = split (map implode (split (explode test_string) "\n")) "";;

let numbers_array = map (map int_of_string) number_strings_array;;

let rec sum arr = match arr with
[] -> 0
| h::t -> sum t + h;;

let sums_array = map sum numbers_array;;

let rec max_int arr = match arr with
[h] -> h
| h::t -> if h > max_int t then h else max_int t
| [] -> 0;;

let () = print_int (max_int sums_array);
