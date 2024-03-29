open Myutils

let lines = read_file "./inputs/day12.txt"
let explode_char str = List.rev (String.fold_left (fun acc elem -> elem::acc) [] str)
let explode_2d s = List.map explode_char (String.split_on_char '\n' s)

let twod_array_of_twod_list twod_list =
  let twod_array = Array.make (List.length twod_list) (Array.make (List.length (List.hd twod_list)) 0) in
  let rec loop twod_list i =
    match twod_list with
    | [] -> ()
    | hd :: tl -> twod_array.(i) <- Array.of_list (List.map (fun x -> let use_char = if x = 'S' then 'a' else if x = 'E' then 'z' else x in int_of_char use_char - 97) hd); loop tl (i + 1) in
  loop twod_list 0; twod_array

let get_direction_char diff = match diff with
  | (0, 1) -> '<'
  | (0, -1) -> '>'
  | (1, 0) -> '^'
  | (-1, 0) -> 'v'
  | _ -> ' '

let find_in_path path (i, j) = List.exists (fun (x, y) -> x = i && y = j) path

let directions = [(0, 1); (0, -1); (1, 0); (-1, 0)]

let draw_path (height, width) path =
  let map_array = Array.make_matrix height width '.' in
  let (endi, endj) = List.hd path in
  map_array.(endi).(endj) <- 'E';
  let rec loop path = match path with
    | [] -> ()
    | (fromi, fromj)::(toi, toj)::tl -> map_array.(fromi).(fromj) <- get_direction_char (toi - fromi, toj - fromj); loop ((toi, toj)::tl)
    | (i, j)::[] -> map_array.(i).(j) <- 'S' in
  loop (List.tl path); Array.iter (fun row -> Array.iter (fun elem -> print_char elem) row; print_newline ()) map_array

let print_option print_elem opt = match opt with
  | None -> print_string "None"
  | Some (elem) -> print_elem elem

let print_int_tuple = print_tuple print_int

let print_int_tuple_list = printlist print_int_tuple

let print_int_tuple_list_option lst = print_option print_int_tuple_list lst

let rec find_path height width prev_height checked_array heights_array path (i, j) block_cond end_cond =
  if i < 0 || j < 0 || i >= height || j >= width then (None) else
  let cur_height = heights_array.(i).(j) in
  if block_cond cur_height prev_height then (None) else
  if end_cond i j then Some ((i, j)::path) else
  if (checked_array.(i).(j) <= List.length path + 1) then (None) else (
  checked_array.(i).(j) <- List.length path + 1;
  let rests = List.map (fun (di, dj) ->
    find_path height width cur_height checked_array heights_array ((i, j)::path) (i + di, j + dj) block_cond end_cond) directions in
  List.fold_left
    (fun min_opt cur_opt ->
    match (min_opt, cur_opt) with
      | (Some (min_path), Some (cur_path)) -> if List.length cur_path < List.length min_path then cur_opt else min_opt
      | (_, Some (_)) -> cur_opt
      | (Some (_), _) -> min_opt
      | _ -> None)
    None rests)

let rec find_c cha i lst = match lst with
  | [] -> -1
  | hd :: tl -> if hd = cha then i else find_c cha (i + 1) tl

let rec find_char cha i heights_list =
  match heights_list with
  | [] -> (-1, -1)
  | hd :: tl -> let j = find_c cha 0 hd in if j >= 0 then (i, j) else find_char cha (i + 1) tl

let run () =
  print_newline ();
  let heights_list = explode_2d (String.concat "\n" lines) in
  print_endline "Heights list:";
  printlist (printlist print_char) heights_list;
  print_newline ();
  print_newline ();
  let heights_array = twod_array_of_twod_list heights_list in
  print_endline "Heights array:";
  print_array (print_array print_int) heights_array;
  let start_pos = find_char 'S' 0 heights_list in
  let (endi, endj) = find_char 'E' 0 heights_list in
  let (height, width) = (Array.length heights_array, Array.length heights_array.(0)) in
  let checked_array = Array.make_matrix height width max_int in
  print_newline ();
  print_newline ();
  print_endline "Start position:";
  print_tuple print_int start_pos;
  print_newline ();
  print_endline "End position:";
  print_tuple print_int (endi, endj);
  let path_opt = find_path height width (int_of_char 'a' - 97) checked_array heights_array [] start_pos (fun cur_height prev_height -> cur_height - prev_height > 1) (fun i j -> i = endi && j = endj) in
  print_newline ();
  print_newline ();
  print_endline "Path:";
  match path_opt with None -> print_string "No path found" | Some (path) -> printlist (print_tuple print_int) path;
  print_newline ();
  print_newline ();
  draw_path (Array.length heights_array, Array.length heights_array.(0)) path;
  print_newline ();
  print_newline ();
  print_endline "Steps:";
  print_int (List.length path - 1);
  print_newline ();
  print_newline ();
  let checked_array_2 = Array.make_matrix height width max_int in
  let path_from_any_a_opt = find_path height width (int_of_char 'z' - 97) checked_array_2 heights_array [] (endi, endj)
    (fun cur_height prev_height -> prev_height - cur_height > 1)
    (fun i j -> let he = heights_array.(i).(j) in he = 0) in
  print_newline ();
  print_newline ();
  print_endline "Path to any a:";
  match path_from_any_a_opt with None -> print_string "No path found" | Some (path_to_a) -> printlist (print_tuple print_int) path_to_a;
  print_newline ();
  print_newline ();
  draw_path (Array.length heights_array, Array.length heights_array.(0)) (List.rev path_to_a);
  print_newline ();
  print_newline ();
  print_endline "Steps:";
  print_int (List.length path_to_a - 1);
  print_newline ();;
