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

let prepare_rock_layer x = List.map (fun ind -> ind + x)
let prepare_rock x = List.map (prepare_rock_layer x)

let rec print_chamber chamber rock (x, y) =
  match (rock, y, chamber) with
  | ([], top, _) when top > 0 -> print_chamber_floor [] [] (-1); print_chamber chamber [] (x, top - 1)
  | ([], _, _) -> print_static_chamber chamber
  | (rh :: rt, top, _) when top > 0 -> print_chamber_floor [] (prepare_rock_layer x rh) (-1); print_chamber chamber rt (x, top - 1)
  | (rh :: rt, 0, ch :: ct) -> print_chamber_floor ch (prepare_rock_layer x rh) (-1); print_chamber ct rt (x, 0)
  | (_, top, ch :: ct) -> print_chamber_floor ch [] (-1); print_chamber ct rock (x, top + 1)
  | (_, _, []) -> print_static_chamber []

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

let rec floor_clear floor rock_layer =
  match (floor, rock_layer) with
  | (_, []) -> true
  | ([], _) -> true
  | (f :: loor, r :: _) when f > r -> floor_clear loor rock_layer
  | (f :: _, r :: _) when f = r -> false
  | (_ :: _, _ :: ock) -> floor_clear floor ock

let rec chamber_clear chamber rock (x, y) =
  let max_rock = max_list_2d (List.map (prepare_rock_layer x) rock) in
  if max_rock >= num_columns then false else
  match (chamber, rock, y) with
  | (_, [], _) -> true (* Finished checking rock *)
  | ([], _, _) -> false (* Passed chamber floor *)
  | (_, _ :: ock, yy) when yy > 0 -> chamber_clear chamber ock (x, y - 1) (* This rock layer is above top floor *)
  | (_ :: hamber, _, yy) when yy < 0 -> chamber_clear hamber rock (x, y + 1) (* This floor is above the rock *)
  | (c :: hamber, r :: ock, _) -> (floor_clear c (prepare_rock_layer x r)) && chamber_clear hamber ock (x, 0)

let step chamber rock (x, y) direction =
  let new_x = match direction with
  | ">" -> if chamber_clear chamber rock (x + 1, y) then x + 1 else x
  | _ -> if chamber_clear chamber rock (x - 1, y) && x > 0 then x - 1 else x in
  let new_y = if chamber_clear chamber rock (new_x, y - 1) then y - 1 else y in
  (new_x, new_y)

let rec drop_rock chamber rock (x, y) directions =
  match directions with
  | dir :: rest -> 
      (match step chamber rock (x, y) dir with
      | (new_x, new_y) when new_y = y -> (new_x, y)
      | (new_x, new_y) -> drop_rock chamber rock (new_x, new_y) rest)
  | [] -> failwith "Out of directions"

let rec merge_rock_into_chamber chamber rock (x, y) =
  match (chamber, rock, y) with
  | (_, [], _) -> chamber
  | ([], _, _) -> prepare_rock x rock
  | (_, r :: ock, yy) when yy > 0 -> (prepare_rock_layer x r) :: merge_rock_into_chamber chamber ock (x, y - 1)
  | (c :: hamber, _, yy) when yy < 0 -> c :: merge_rock_into_chamber hamber rock (x, y + 1)
  | (c :: hamber, r :: ock, _) -> merge_sorted (<) c r :: merge_rock_into_chamber hamber ock (x, 0)

let run () = print_newline ();
  print_endline "Day 17";
  print_newline ();
  print_newline ();
  print_endline (List.hd lines);
  print_newline ();;
