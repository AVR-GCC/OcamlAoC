open Myutils

let lines = read_file "./inputs/day17test.txt"

let num_columns = 7

let minus_rock = [[0; 1; 2; 3]]
let plus_rock = [[1]; [0; 1; 2]; [1]]
let l_rock = [[2]; [2]; [0; 1; 2]]
let line_rock = [[0]; [0]; [0]; [0]]
let square_rock = [[0; 1]; [0; 1]]

let rocks = [minus_rock; plus_rock; l_rock; line_rock; square_rock]

let rec print_chamber_floor floor rock_layer index =
  if index = -1 then (print_char '|'; print_chamber_floor floor rock_layer 0) else
  if index = num_columns then print_endline "|" else
  let next_index = index + 1 in
  match (floor, rock_layer) with
    | ([], []) -> print_char '.'; print_chamber_floor [] [] next_index
    | (fh :: ft, []) ->
        if fh = index then (print_char '#'; print_chamber_floor ft [] next_index)
        else (print_char '.'; print_chamber_floor floor [] next_index)
    | ([], rh :: rt) ->
        if rh = index then (print_char '@'; print_chamber_floor [] rt next_index)
        else (print_char '.'; print_chamber_floor [] rock_layer next_index)
    | (fh :: ft, rh :: rt) ->
        if rh = index then (print_char '@'; print_chamber_floor floor rt next_index)
        else if fh = index then (print_char '#'; print_chamber_floor ft rock_layer next_index)
        else (print_char '.'; print_chamber_floor floor rock_layer next_index)

let rec print_static_chamber chamber =
  match chamber with
  | [] -> print_endline "+-------+"
  | floor :: rest -> print_chamber_floor floor [] (-1); print_static_chamber rest

let rec print_chamber chamber rock (x, y) =
  let prepare_rock_layer rh = (List.map (fun ind -> ind + x) rh) in
  match (rock, y, chamber) with
  | (_, _, []) -> print_static_chamber []
  | ([], top, _) when top > 0 -> print_chamber_floor [] [] (-1); print_chamber chamber [] (x, top - 1)
  | ([], _, _) -> print_static_chamber chamber
  | (rh :: rt, top, _) when top > 0 -> print_chamber_floor [] (prepare_rock_layer rh) (-1); print_chamber chamber rt (x, top - 1)
  | (rh :: rt, 0, ch :: ct) -> print_chamber_floor ch (prepare_rock_layer rh) (-1); print_chamber ct rt (x, 0)
  | (_, top, ch :: ct) -> print_chamber_floor ch [] (-1); print_chamber ct rock (x, top + 1)

(* chamber = [[3]; [2; 3; 4]; [3]; [2; 3; 4; 5]] *)
(* print_chamber chamber l_rock (2, 6); *)
(* |....@..| *)
(* |....@..| *)
(* |..@@@..| *)
(* |.......| *)
(* |.......| *)
(* |.......| *)
(* |...#...| *)
(* |..###..| *)
(* |...#...| *)
(* |..####.| *)
(* +-------+ *)

let run () = print_newline ();
  print_endline "Day 17";
  print_newline ();
  print_newline ();
  print_endline (List.hd lines);
  print_newline ();;
