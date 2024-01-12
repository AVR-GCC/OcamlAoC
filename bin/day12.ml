let test_string = "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi"

let explode_char str = List.rev (String.fold_left (fun acc elem -> elem::acc) [] str)
let explode_2d s = List.map explode_char (String.split_on_char '\n' s)

let twod_array_of_twod_list twod_list =
  let twod_array = Array.make (List.length twod_list) (Array.make (List.length (List.hd twod_list)) 0) in
  let rec loop twod_list i =
    match twod_list with
    | [] -> ()
    | hd :: tl -> twod_array.(i) <- Array.of_list (List.map (fun x -> int_of_char x - 82) hd); loop tl (i + 1) in
  loop twod_list 0; twod_array

let get_direction_char diff = match diff with
  | (0, 1) -> '<'
  | (0, -1) -> '>'
  | (1, 0) -> '^'
  | (-1, 0) -> 'V'
  | _ -> ' '

let find_in_path path (i, j) = List.exists (fun (x, y) -> x = i && y = j) path

let get_height heights_array (i, j) = if heights_array.(i).(j) = 1 then 15 else heights_array.(i).(j)

let directions = [(0, 1); (0, -1); (1, 0); (-1, 0)]

let draw_path (height, width) path =
  let map_array = Array.make_matrix height width '.' in
  let rec loop path = match path with
    | [] -> ()
    | (fromi, fromj)::(toi, toj)::tl -> map_array.(fromi).(fromj) <- get_direction_char (toi - fromi, toj - fromj); loop ((toi, toj)::tl)
    | (i, j)::[] -> map_array.(i).(j) <- 'S' in
  loop path; Array.iter (fun row -> Array.iter (fun elem -> print_char elem) row; print_newline ()) map_array

let print_option print_elem opt = match opt with
  | None -> print_string "None"
  | Some (elem) -> print_elem elem

let print_int_tuple tup = Day5.print_tuple print_int tup

let print_int_tuple_list lst = Day1.printlist print_int_tuple lst

let print_int_tuple_list_option lst = print_option print_int_tuple_list lst

let rec find_path height width prev_height checked_array heights_array path (i, j) =
  if i < 0 || j < 0 || i >= height || j >= width then (None) else (
  let cell_height = get_height heights_array (i, j) in
  if (cell_height < 0) then (if prev_height = (int_of_char 'z' - 82) then Some ((i, j)::path) else (None)) else
  if cell_height - prev_height > 1 then (None) else
  if (checked_array.(i).(j) <= List.length path + 1) then (None) else (
  checked_array.(i).(j) <- List.length path + 1;
  let rests = List.map (fun (di, dj) ->
    find_path height width cell_height checked_array heights_array ((i, j)::path) (i + di, j + dj)) directions in
  List.fold_left
    (fun min_opt cur_opt ->
    match (min_opt, cur_opt) with
      | (Some (min_path), Some (cur_path)) -> if List.length cur_path < List.length min_path then cur_opt else min_opt
      | (_, Some (_)) -> cur_opt
      | (Some (_), _) -> min_opt
      | _ -> None)
    None rests))


let rec find_start heights_list = let rec find_s i j lst = match lst with
  | [] -> (-1, -1)
  | hd :: tl -> if hd = 'S' then (i, j) else find_s i (j + 1) tl in
  match heights_list with
  | [] -> (-1, -1)
  | hd :: tl -> let (i, j) = find_s 0 0 hd in if i >= 0 then (i, j) else find_start tl

let run () =
  print_newline ();
  let heights_list = explode_2d test_string in
  print_endline "Heights list:";
  Day1.printlist (Day1.printlist print_char) heights_list;
  print_newline ();
  print_newline ();
  let heights_array = twod_array_of_twod_list heights_list in
  print_endline "Heights array:";
  Day11.print_array (Day11.print_array print_int) heights_array;
  let start_pos = find_start heights_list in
  let (height, width) = (Array.length heights_array, Array.length heights_array.(0)) in
  let checked_array = Array.make_matrix height width max_int in
  print_newline ();
  print_newline ();
  print_endline "Start position:";
  Day5.print_tuple print_int start_pos;
  let path_opt = find_path height width (int_of_char 'a' - 82) checked_array heights_array [] start_pos in
  print_newline ();
  print_newline ();
  print_endline "Path:";
  match path_opt with None -> print_string "No path found" | Some (path) -> Day1.printlist (Day5.print_tuple print_int) path;
  print_newline ();
  print_newline ();
  draw_path (Array.length heights_array, Array.length heights_array.(0)) path;
  print_newline ();
  print_newline ();
  print_endline "Steps:";
  print_int (List.length path - 1);
  print_newline ();;
