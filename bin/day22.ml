open Myutils

(* Types *)
type direction = Left | Right

let print_direction = function
  | Left -> print_string "Left"
  | Right -> print_string "Right"

type instruction = Dist of int | Turn of direction

(* Print *)
let print_instruction = function
  | Dist x -> print_int x
  | Turn t -> print_direction t

(* Utility *)
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

let rec pad_spaces size lst = match (lst, size) with
  | (_, 0) -> []
  | ([], x) -> " " :: pad_spaces (x - 1) []
  | (e :: rest, x) -> e :: pad_spaces (x - 1) rest

let rec get_start_point = function
  | Point p :: _ -> p
  | _ :: rest -> get_start_point rest
  | _ -> failwith "start not found"

(* Connect *)
(* let parse_faces mat = *)
(*   let size = List.length mat / 3 in *)
(*   let section_1 = mat |> List.take size in *)
(*   let section_2 = mat |> List.drop size |> List.take size in *)
(*   let section_3 = mat |> List.drop (size * 2) |> List.map (List.filter (fun x -> x <> Space)) in *)
(*   let face_one = List.map (List.filter (fun x -> x <> Space)) section_1 in *)
(*   let face_two = List.map (fun row -> List.take size row) section_2 in *)
(*   let face_three = List.map (fun row -> row |> List.drop size |> List.take size) section_2 in *)
(*   let face_four = List.map (fun row -> row |> List.drop (size * 2) |> List.take size) section_2 in *)
(*   let face_five = List.map (fun row -> row |> List.take size) section_3 in *)
(*   let face_six = List.map (fun row -> row |> List.drop size |> List.take size) section_3 in *)
(*   (face_one, face_two, face_three, face_four, face_five, face_six) *)
(**)
(* let connect_cube mat = *)
(*   let (f1, f2, f3, f4, f5, f6) = parse_faces mat in *)
(*   let lf1 = List.map (fun r -> List.hd r) f1 in *)
(*   let lf2 = List.map (fun r -> List.hd r) f2 in *)
(*   let lf5 = List.map (fun r -> List.hd r) f5 in *)
(*   let rf1 = List.map (fun r -> List.hd (List.rev r)) f1 in *)
(*   let rf4 = List.map (fun r -> List.hd (List.rev r)) f4 in *)
(*   let rf6 = List.map (fun r -> List.hd (List.rev r)) f6 in *)
(*   let tf1 = List.hd f1 in *)
(*   let tf2 = List.hd f2 in *)
(*   let tf3 = List.hd f3 in *)
(*   let tf6 = List.hd f6 in *)
(*   let bf2 = List.hd (List.rev f2) in *)
(*   let bf3 = List.hd (List.rev f3) in *)
(*   let bf5 = List.hd (List.rev f5) in *)
(*   let bf6 = List.hd (List.rev f6) in *)
(*   connect_edge lf1 tf3 West North; *)
(*   connect_edge lf2 (List.rev bf6) West South; *)
(*   connect_edge lf5 (List.rev bf3) West South; *)
(*   connect_edge rf1 (List.rev rf6) East East; *)
(*   connect_edge rf4 (List.rev tf6) East North; *)
(*   connect_edge tf1 (List.rev tf2) North North; *)
(*   connect_edge bf2 (List.rev bf5) South South *)

let parse_faces mat =
  let size = 50 in
  let section_1 = mat |> List.take size in
  let section_2 = mat |> List.drop size |> List.take size in
  let section_3 = mat |> List.drop (size * 2) |> List.take size in
  let section_4 = mat |> List.drop (size * 3) |> List.take size in
  let face_one = List.map (fun row -> row |> List.drop size |> List.take size) section_1 in
  let face_two = List.map (fun row -> row |> List.drop (size * 2) |> List.take size) section_1 in
  let face_three = List.map (fun row -> row |> List.drop size |> List.take size) section_2 in
  let face_four = List.map (fun row -> row |> List.take size) section_3 in
  let face_five = List.map (fun row -> row |> List.drop size |> List.take size) section_3 in
  let face_six = List.map (fun row -> row |> List.take size) section_4 in
  (face_one, face_two, face_three, face_four, face_five, face_six)

let connect_cube mat =
  let (f1, f2, f3, f4, f5, f6) = parse_faces mat in
  let lf1 = List.map (fun r -> List.hd r) f1 in
  let lf3 = List.map (fun r -> List.hd r) f3 in
  let lf4 = List.map (fun r -> List.hd r) f4 in
  let lf6 = List.map (fun r -> List.hd r) f6 in
  let rf2 = List.map (fun r -> List.hd (List.rev r)) f2 in
  let rf3 = List.map (fun r -> List.hd (List.rev r)) f3 in
  let rf5 = List.map (fun r -> List.hd (List.rev r)) f5 in
  let rf6 = List.map (fun r -> List.hd (List.rev r)) f6 in
  let tf1 = List.hd f1 in
  let tf2 = List.hd f2 in
  let tf4 = List.hd f4 in
  let bf2 = List.hd (List.rev f2) in
  let bf5 = List.hd (List.rev f5) in
  let bf6 = List.hd (List.rev f6) in
  connect_edge lf1 (List.rev lf4) West West;
  connect_edge tf1 lf6 North West;
  connect_edge tf2 bf6 North South;
  connect_edge rf2 (List.rev rf5) East East;
  connect_edge bf2 rf3 South East;
  connect_edge lf3 tf4 West North;
  connect_edge bf5 rf6 South East

let create_and_connect_points exploded len cube =
  let (initialized, transposed) = exploded_to_points exploded len in
  if cube then connect_cube initialized else connect_edges initialized transposed;
  (* printlist (fun l -> printlist print_point_opt l; print_newline ()) initialized; *)
  initialized

(* Walk *)
let rec walk ({ orientation; point } as state) instructions =
  (* print_state state; *)
  (* print_newline (); *)
  match instructions with
  | [] -> state
  | ins :: rest ->
      (* print_instruction ins; *)
      (* print_newline (); *)
      match ins with
      | Turn d -> walk { orientation = (turn orientation d); point } rest
      | Dist d ->
          if d = 0 then (walk state rest) else (
            (* print_point_pos point; print_orientation orientation; print_newline (); *)
          let (next_point, next_orientation) = get_neighbor point orientation in
          let next_state = { point = next_point; orientation = next_orientation } in
          walk next_state ((Dist (d - 1)) :: rest))

let state_to_password { orientation; point = { position = (x, y); _ } } = orientation_numbers orientation + (1000 * (y + 1)) + (4 * (x + 1))

let run () = print_newline ();
  print_endline "Day 22";
  print_newline ();
  let lines = read_file "./inputs/day22.real.txt" in
  let reversed = List.rev lines in
  let map_strs = reversed |> List.tl |> List.tl |> List.rev in
  let exploded = List.map explode map_strs in
  let longest_row = max_list @@ List.map List.length exploded in
  let equalized = List.map (pad_spaces longest_row) exploded in
  let points = create_and_connect_points equalized longest_row true in
  let start_point = get_start_point @@ List.hd points in
  let start_state = { orientation = East; point = start_point } in
  let instructions_strs = List.hd reversed in
  let splitted_right = split_on_string "R" instructions_strs in
  let splitted = List.map (split_on_string "L") splitted_right in
  let instructions = List.tl @@ List.fold_left (fun acc left_splitted ->
    let distances = List.map (fun x -> Dist (int_of_string x)) left_splitted in
    let left_instructions = join_list (Turn Left) distances in
    let extension = Turn Right :: left_instructions in
    List.append acc extension
  ) [] splitted in
  (* print_endline "instructions"; *)
  (* print_newline (); *)
  (* printlist print_instruction instructions; *)
  (* print_newline (); *)
  let finish_state = walk start_state instructions in
  print_endline "final state:";
  print_traveler finish_state;
  print_newline ();
  let password = state_to_password finish_state in
  print_endline "password:";
  print_int password;
  print_newline ();;
