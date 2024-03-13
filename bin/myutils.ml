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

let read_file filename = 
  let in_channel = open_in filename in
  let rec read_lines acc = 
    try
      let line = input_line in_channel in
      read_lines (line::acc)
    with End_of_file -> List.rev acc in
  let result = read_lines [] in
  close_in in_channel;
  result

let split_on_chars str chars = let rec split_on_chars' list_acc str_acc i = if i = String.length str then List.rev (str_acc::list_acc) else
  match str.[i] with
  | c when List.mem c chars -> split_on_chars' (str_acc::list_acc) "" (i+1)
  | c -> split_on_chars' list_acc (str_acc ^ Char.escaped c) (i+1) in
  split_on_chars' [] "" 0

let add_to_char c i = Char.chr (Char.code c + i)

let fold_lefti f acc lst = let rec fold_lefti' acc lst i = match lst with
  | [] -> acc
  | h::t -> fold_lefti' (f acc h i) t (i + 1) in
  fold_lefti' acc lst 0

let my_merge_sort cmp lst = let rec merge_two_lists lst1 lst2 = match lst1, lst2 with
  | [], l2 -> l2
  | l1, [] -> l1
  | h1::t1, h2::t2 -> let cmp_res = cmp (h1, h2) in
    if cmp_res < 0 then h1::(merge_two_lists t1 lst2)
    else h2::(merge_two_lists lst1 t2) in
  let rec do_one_run lst = match lst with
  | [] -> []
  | [x] -> [x]
  | a::b::t -> (merge_two_lists a b)::(do_one_run t) in
  let rec do_all_runs mlst = match mlst with
  | [] -> []
  | [x] -> x
  | l -> do_all_runs (do_one_run l) in
  do_all_runs (List.map (fun x -> [x]) lst)

let find_indecies cond = fold_lefti (fun acc v i -> if cond v then i::acc else acc) []

let rec is_sub_str_from_index str sub_str index = match sub_str with
  | [] -> true
  | h::t -> if index >= String.length str then false
    else if h.[0] = str.[index] then is_sub_str_from_index str t (index + 1)
    else false

let split_on_string delim str = let delimiter = explode delim in
  let rec split_on_string' list_acc str_acc i = if i = String.length str then List.rev (str_acc::list_acc) else
  if is_sub_str_from_index str delimiter i then split_on_string' (str_acc::list_acc) "" (i + (String.length delim))
  else split_on_string' list_acc (str_acc ^ Char.escaped str.[i]) (i + 1) in
  split_on_string' [] "" 0

let map_2d mat fn = ignore (List.mapi (fun i row ->
  ignore (List.mapi (fun j v -> fn i j v ) row);
  print_endline ""
) mat)

let map_2d_arr mat fn = ignore (Array.mapi (fun i row ->
  ignore (Array.mapi (fun j v -> fn i j v ) row);
  print_endline ""
) mat)

let split_on_strings delims str = let delimiters = List.map explode delims in
  let rec split_on_string' list_acc str_acc i = if i = String.length str then List.rev (str_acc::list_acc) else
  match List.find_opt (fun delimiter -> is_sub_str_from_index str delimiter i) delimiters with
  | Some (delim) -> split_on_string' (str_acc::list_acc) "" (i + (List.length delim))
  | None -> split_on_string' list_acc (str_acc ^ Char.escaped str.[i]) (i + 1) in
  split_on_string' [] "" 0

let print_mixed_tuple prnt1 prnt2 (x, y) = 
  print_string "(";
  prnt1 x;
  print_string ", ";
  prnt2 y;
  print_string ")"

let print_mixed_triple prnt1 prnt2 prnt3 (x, y, z) = 
  print_string "(";
  prnt1 x;
  print_string ", ";
  prnt2 y;
  print_string ", ";
  prnt3 z;
  print_string ")"

let clear_duplicates lst = let rec clear_duplicates' acc lst = match lst with
  | [] -> List.rev acc
  | h::t -> if List.mem h acc then clear_duplicates' acc t
    else clear_duplicates' (h::acc) t in
  clear_duplicates' [] lst

let print_opt prnt opt = match opt with
  | Some x -> prnt x
  | None -> print_string "None"