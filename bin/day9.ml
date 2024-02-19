open Myutils

let lines = read_file "./inputs/day9.txt"

let do_move (x, y) (dx, dy) =
  let x = x + dx in
  let y = y + dy in
  (x, y)

let do_move_set (direction, distance) positions =
  let dx, dy =
    match direction with
    | 'R' -> (1, 0)
    | 'L' -> (-1, 0)
    | 'U' -> (0, 1)
    | 'D' -> (0, -1)
    | _ -> failwith "Invalid direction"
  in
  let rec loop positions distance =
    if distance = 0 then positions
    else loop ((do_move (List.hd positions) (dx, dy))::positions) (distance - 1)
  in
  loop positions distance

let do_line positions line = match String.split_on_char ' ' line with
  | direction :: distance :: [] -> do_move_set (direction.[0], int_of_string distance) positions
  | _ -> failwith "Invalid line"

let head_positions = List.rev @@ List.fold_left (fun acc elem -> do_line acc elem) [(0, 0)] lines

let update_tale_position (hx, hy) (tx, ty) = match (hx - tx, hy - ty) with
  | (-1, -1) | (-1, 0) | (0, -1) | (0, 0) | (0, 1) | (1, 0) | (1, 1) | (-1, 1) | (1, -1) -> (tx, ty)
  | (0, ydiff) -> (tx, (ty + if ydiff < 0 then -1 else 1))
  | (xdiff, 0) -> ((tx + if xdiff < 0 then -1 else 1), ty)
  | (xdiff, ydiff) -> ((tx + if xdiff < 0 then -1 else 1), (ty + if ydiff < 0 then -1 else 1))

let trace_next_link prev_link_positions = List.tl @@ List.rev @@ List.fold_left (fun acc elem -> (update_tale_position elem (List.hd acc))::acc) [(0, 0)] prev_link_positions

let tale_positions = trace_next_link head_positions

let ninth_link_positions = List.fold_left (fun acc _ -> trace_next_link acc) head_positions [1; 2; 3; 4; 5; 6; 7; 8; 9]

module PairSet = Set.Make(struct 
  type t = int * int
  let compare = compare
end)

let positions_set = PairSet.of_list tale_positions

let ninth_link_positions_set = PairSet.of_list ninth_link_positions

let maxs_and_mins lst = let rec maxs_and_mins' lst maxx maxy minx miny = match lst with
  | [] -> (maxx, maxy, minx, miny)
  | (x, y)::tl -> maxs_and_mins' tl (max x maxx) (max y maxy) (min x minx) (min y miny)
  in maxs_and_mins' lst 0 0 0 0

let print_line y minx maxx positions_set = print_newline (); let rec print_line' x = match x with
  | x when x > maxx -> ()
  | x -> if x = 0 && y = 0 then print_string "s" else if PairSet.mem (x, y) positions_set then print_string "#" else print_string "."; print_line' (x + 1)
  in print_line' minx

let print_grid positions_set = let maxx, maxy, minx, miny = maxs_and_mins tale_positions in
  let rec print_grid' y minx maxx positions_set = match y with
  | y when y < miny -> ()
  | y -> print_line y minx maxx positions_set; print_grid' (y - 1) minx maxx positions_set
  in print_grid' maxy minx maxx positions_set

let run () =
  print_newline ();
  print_endline "Head positions:";
  printlist (print_tuple print_int) (head_positions);
  print_newline ();
  print_newline ();
  print_endline "Tale positions list:";
  printlist (print_tuple print_int) (tale_positions);
  print_newline ();
  print_newline ();
  print_endline "Tale positions map:";
  print_grid positions_set;
  print_newline ();
  print_newline ();
  print_endline "Total tale positions:";
  print_int @@ PairSet.cardinal positions_set;
  print_newline ();
  print_newline ();
  print_endline "Ninth link positions list:";
  printlist (print_tuple print_int) (ninth_link_positions);
  print_newline ();
  print_newline ();
  print_endline "Ninth link positions map:";
  print_grid ninth_link_positions_set;
  print_newline ();
  print_newline ();
  print_endline "Total ninth link positions:";
  print_int @@ PairSet.cardinal ninth_link_positions_set;
  print_newline ();;