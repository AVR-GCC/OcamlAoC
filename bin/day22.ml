open Myutils

(* Types *)
type direction = Left | Right

let print_direction = function
  | Left -> print_string "Left"
  | Right -> print_string "Right"

type instruction = Dist of int | Turn of direction

(* Print *)
let print_point_opt = function
  | Block -> print_string "Block"
  | Space -> ()
  | Point p -> print_point p

let print_instruction = function
  | Dist x -> print_int x
  | Turn t -> print_direction t

(* Utility *)
let flip = function
  | North -> South
  | East -> West
  | South -> North
  | West -> East

let get_neighbor point orientation =
  match OrientationMap.find_opt orientation point.neighbors with
  | None -> (point, orientation)
  | Some p -> p

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

let rec transpose mat = function
  | 0 -> []
  | i ->
      let (col, tail) = List.split @@ List.map (fun row ->
        match row with
        | h :: t -> (h, t)
        | _ -> failwith "row not equalized"
      ) mat in
      col :: transpose tail (i - 1)

let rec pad_spaces size lst = match (lst, size) with
  | (_, 0) -> []
  | ([], x) -> " " :: pad_spaces (x - 1) []
  | (e :: rest, x) -> e :: pad_spaces (x - 1) rest

let rec get_start_point = function
  | Point p :: _ -> p
  | _ :: rest -> get_start_point rest
  | _ -> failwith "start not found"

(* Initialize *)
let rec initialize_row row x y =
  match row with
  | "." :: rest ->
      let point = Point { neighbors = OrientationMap.empty; position = (x, y) } in
      point :: initialize_row rest (x + 1) y
  | "#" :: rest -> Block :: initialize_row rest (x + 1) y
  | " " :: rest -> Space :: initialize_row rest (x + 1) y
  | _ -> []

let initialize_mat mat = List.mapi (fun y row -> initialize_row row 0 y) mat

let get_edges row =
  let rec get_row_edges_rec start cur =
    match (start, cur) with
    | (Space, Space :: t) -> get_row_edges_rec Space t
    | (Space, h :: t) -> get_row_edges_rec h t
    | (Space, []) -> failwith "empty row"
    | (f, [l]) | (f, l :: Space :: _) -> (f, l)
    | (f, _ :: Block :: t) | (f, _ :: Point _ :: t) -> get_row_edges_rec f t
    | _ -> failwith "malformed line" in
  get_row_edges_rec Space row

(* Connect *)
let connect_points_vertical ({ neighbors = ln; _ } as l) ({ neighbors = rn; _ } as r) =
    l.neighbors <- OrientationMap.add South (r, South) ln;
    r.neighbors <- OrientationMap.add North (l, North) rn

let connect_points_horizontal ({ neighbors = ln; _ } as l) ({ neighbors = rn; _ } as r) =
    l.neighbors <- OrientationMap.add East (r, East) ln;
    r.neighbors <- OrientationMap.add West (l, West) rn

let rec connect_row = function
  | Point l :: Point r :: rest -> (
    connect_points_horizontal l r;
    connect_row (Point r :: rest)
  )
  | [] | [_] -> ()
  | _ :: t -> connect_row t

let rec connect_rows = function
  | [] -> ()
  | h :: t -> connect_row h; connect_rows t

let rec connect_col = function
  | Point l :: Point r :: rest -> (
    connect_points_vertical l r;
    connect_col (Point r :: rest)
  )
  | _ :: t -> connect_col t
  | [] -> ()

let rec connect_cols = function
  | [] -> ()
  | h :: t -> connect_col h; connect_cols t

let connect_ends_row row =
  match get_edges row with
  | (Point ps, Point pf) -> connect_points_horizontal pf ps
  | _ -> ()

let connect_row_ends mat = List.iter connect_ends_row mat

let connect_ends_col col =
  match get_edges col with
  | (Point ps, Point pf) -> connect_points_vertical pf ps
  | _ -> ()

let connect_col_ends mat = List.iter connect_ends_col mat

let connect_edges mat tra is_part_1 =
  match is_part_1 with
  | true -> (
    connect_row_ends mat;
    connect_col_ends tra
  )
  | _ -> ()

let connect_points ({ neighbors = ln; _ } as l) ({ neighbors = rn; _ } as r) ltr rtl =
    l.neighbors <- OrientationMap.add ltr (r, flip rtl) ln;
    r.neighbors <- OrientationMap.add rtl (l, flip ltr) rn

let rec connect_edge e1 e2 rtl ltr =
  match (e1, e2) with
  | (Point h1 :: t1, Point h2 :: t2) -> connect_points h1 h2 rtl ltr; connect_edge t1 t2 rtl ltr
  | (_ :: t1, _ :: t2) -> connect_edge t1 t2 rtl ltr
  | ([], []) -> ()
  | _ -> failwith "different sized squares"

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
  let initialized = initialize_mat equalized in
  let transposed = transpose initialized longest_row in
  connect_rows initialized;
  connect_cols transposed;
  connect_cube initialized;
  (* connect_edges initialized transposed true; *)
  (* printlist (fun l -> printlist print_point_opt l; print_newline ()) initialized; *)
  let start_point = get_start_point @@ List.hd initialized in
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
