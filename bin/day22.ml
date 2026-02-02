open Myutils

type orientation = North | East | South | West

let flip = function
  | North -> South
  | East -> West
  | South -> North
  | West -> East

let inc num = function
  | North | West -> num - 1
  | _ -> num + 1

let print_orientation = function
  | North -> print_string "North"
  | East -> print_string "East"
  | South -> print_string "South"
  | West -> print_string "West"

type direction = Left | Right

let print_direction = function
  | Left -> print_string "Left"
  | Right -> print_string "Right"

type instruction = Dist of int | Turn of direction

type state = {
  orientation: orientation;
  position: int * int;
}

module StateMap = Map.Make(struct
  type t = state
  let compare = compare
end)

let ms ori x y = {
  orientation = ori;
  position = (x, y)
}

let print_state { orientation; position } =
  print_orientation orientation;
  print_string " ";
  print_tuple print_int position

type point = Clear | Blocked | Space | End | Start

let print_instruction = function
  | Dist x -> print_int x
  | Turn t -> print_direction t

let orientation_deltas = function
  | North -> (0, -1)
  | East -> (1, 0)
  | South -> (0, 1)
  | West -> (-1, 0)

let orientation_numbers = function
  | North -> 3
  | East -> 0
  | South ->  1
  | West -> 2

let right_turn = function
  | North -> East
  | East -> South
  | South -> West
  | West -> North

let left_turn = function
  | North -> West
  | East -> North
  | South -> East
  | West -> South

let turn orientation = function
  | Right -> right_turn orientation
  | Left -> left_turn orientation

let get_point field (x, y) =
  if y >= Array.length field then End else 
  if y < 0 then Start else 
  if x >= Array.length field.(y) then End else
  if x < 0 then Start else
  match field.(y).(x) with
  | " " -> Space
  | "." -> Clear
  | _ -> Blocked

let rec walk field ({ orientation; position = (x, y) } as state) instructions =
  (* print_state state; *)
  (* print_newline (); *)
  match instructions with
  | [] -> state
  | ins :: rest ->
      (* print_instruction ins; *)
      (* print_newline (); *)
      match ins with
      | Turn d ->
          walk field { orientation = (turn orientation d); position = (x, y) } rest
      | Dist d ->
          if d = 0 then walk field state rest else
          let next_state = StateMap.find state field in
          walk field next_state ((Dist (d - 1)) :: rest)

let rec map_row row index row_index start_blocked start_index ori =
  let next_ind = inc index ori in
  let path_to_self = (ms ori index row_index, ms ori index row_index) in
  (* printlist print_string row; *)
  (* print_string @@ " " ^ string_of_int index; *)
  (* print_newline (); *)
  match (row, start_blocked, start_index) with
  | (" " :: " " :: rest, _, _) -> map_row (" " :: rest) next_ind row_index false None ori
  | (" " :: "." :: rest, _, _) -> map_row ("." :: rest) next_ind row_index false (Some next_ind) ori
  | (" " :: "#" :: rest, _, _) -> map_row ("#" :: rest) next_ind row_index true (Some next_ind) ori
  | ("." :: " " :: _, true, _) -> path_to_self :: []
  | ("." :: " " :: _, false, Some num) ->
      let path_to_start = (ms ori index row_index, ms ori num row_index) in
      let path_from_start = (ms (flip ori) num row_index, ms (flip ori) index row_index) in
      path_to_start :: path_from_start :: []
  | ("." :: "." :: rest, _, _) ->
      let path_to_next = (ms ori index row_index, ms ori next_ind row_index) in
      let path_from_next = (ms (flip ori) next_ind row_index, ms (flip ori) index row_index) in
      let rec_call = map_row ("." :: rest) next_ind row_index start_blocked start_index ori in
      path_to_next :: path_from_next :: rec_call
  | ("." :: "#" :: rest, _, _) ->
      let rec_call = map_row ("#" :: rest) next_ind row_index start_blocked start_index ori in
      path_to_self :: rec_call
  | ("#" :: " " :: _, true, _) | ([" "], _, _) | ([], _, _) -> []
  | ("#" :: " " :: _, false, Some num) ->
      let path_from_start_to_self = (ms (flip ori) num row_index, ms (flip ori) num row_index) in
      path_from_start_to_self :: []
  | ("#" :: "." :: rest, _, _) ->
      let path_from_next_to_self = (ms (flip ori) next_ind row_index, ms (flip ori) next_ind row_index) in
      let rec_call = map_row ("." :: rest) next_ind row_index start_blocked start_index ori in
      path_from_next_to_self :: rec_call
  | ("#" :: "#" :: rest, _, _) -> map_row ("#" :: rest) next_ind row_index start_blocked start_index ori
  | _ -> failwith "Unexpected details"

let map_col row index row_index start_blocked start_index ori =
  let mapped_as_row = map_row row index row_index start_blocked start_index ori in
  List.map (function { orientation; position = (sx, sy) }, { orientation = _ori; position = (cx, cy) } -> ({ orientation; position = (sy, sx) }, { orientation; position = (cy, cx) })) mapped_as_row

let rec pad_spaces size lst = match (lst, size) with
  | (_, 0) -> []
  | ([], x) -> " " :: pad_spaces (x - 1) []
  | (e :: rest, x) -> e :: pad_spaces (x - 1) rest

let init_spaces len = List.init len (function _ -> " ")

let state_to_password { orientation; position = (x, y) } = orientation_numbers orientation + (1000 * (y + 1)) + (4 * (x + 1))

let run () = print_newline ();
  print_endline "Day 22";
  print_newline ();
  let lines = read_file "./inputs/day22.real.txt" in
  let reversed = List.rev lines in
  let instructions_strs = List.hd reversed in
  let map_strs = reversed |> List.tl |> List.tl |> List.rev in
  (* print_endline "instructions"; *)
  (* print_newline (); *)
  let splitted_right = split_on_string "R" instructions_strs in
  let splitted = List.map (split_on_string "L") splitted_right in
  let instructions = List.tl @@ List.fold_left (fun acc left_splitted ->
    let distances = List.map (fun x -> Dist (int_of_string x)) left_splitted in
    let left_instructions = join_list (Turn Left) distances in
    let extension = Turn Right :: left_instructions in
    List.append acc extension
  ) [] splitted in
  (* printlist print_instruction instructions; *)
  (* print_newline (); *)
  let exploded = List.map explode map_strs in
  let start_x_opt = List.find_index (fun x -> x = ".") @@ List.hd exploded in
  let start_x = match start_x_opt with
  | None -> 0
  | Some x -> x in
  (* print_endline "map"; *)
  let longest_row = max_list @@ List.map List.length exploded in
  let equalized = List.map (pad_spaces longest_row) exploded in
  (* printlist (fun x -> printlist print_string x; print_newline ()) equalized; *)
  (* print_newline (); *)
  let pre = init_spaces (longest_row + 2) in
  let middle = List.map (function x -> " " :: (x @ [" "])) equalized in
  let post = init_spaces (longest_row + 2) in
  let padded = pre :: (middle @ [post]) in
  (* let lst = List.map (fun r -> List.nth r 4) padded in *)
  (* let mappings = map_col lst (-1) 3 false None South in *)
  (* printlist print_string lst; *)
  (* print_newline (); *)
  (* printlist (print_mixed_tuple print_state (print_tuple print_int)) mappings; *)
  (* print_newline (); *)
  (* print_newline (); *)
  let horizontal_mappings = List.flatten @@ List.mapi (fun index x -> map_row x (-1) (index - 1) false None East) padded in
  let misc = init_spaces (longest_row + 2) in
  let vertical_mappings = List.flatten @@ List.mapi (fun index _ -> map_col (List.map (fun r -> List.nth r index) padded) (-1) (index - 1) false None South) misc in
  let all_mappings = horizontal_mappings @ vertical_mappings in
  (* printlist (print_mixed_tuple print_state (print_tuple print_int)) all_mappings; *)
  (* print_newline (); *)
  let field = StateMap.of_list all_mappings in
  let final_state = walk field { orientation = East; position = (start_x, 0) } instructions in
  print_string "final state: ";
  print_state final_state;
  print_newline ();
  print_string "final number: ";
  print_int @@ state_to_password final_state;
  print_newline ();;
