let printlist printelem lst = let rec middle = function
| [] -> ()
| h::t -> printelem h; if t = [] then () else print_string "; "; middle t in
print_string "["; middle lst; print_string "]"

let split_list manipulate delimiter lst =
  let rec get_chunk this_chunk remaining_list = match remaining_list with
  | [] -> (this_chunk, [])
  | h::t -> if h = delimiter
    then (this_chunk, t)
    else get_chunk ((manipulate h)::this_chunk) t in
  let rec get_chunks result_so_far remaining_list = match remaining_list with
  | [] -> result_so_far
  | _::_ -> let chunk = get_chunk [] remaining_list in
  match chunk with
  | (this_chunk, new_remaining_list) -> get_chunks (this_chunk::result_so_far) new_remaining_list in
  get_chunks [] lst

let sum = List.fold_left (+) 0

let max_list = List.fold_left (fun acc elem -> max acc elem) min_int

let print_tuple prnt tup = match tup with
  | (one, two) -> print_string "("; prnt one; print_string ", "; prnt two; print_string ")"

let explode str = List.rev (String.fold_left (fun acc elem -> (String.make 1 elem)::acc) [] str)

let print_array prnt arr = 
  print_string "[";
  Array.iteri (fun i x -> prnt x; if (i < Array.length arr - 1) then print_string ", ") arr;
  print_string "]"
