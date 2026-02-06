open Myutils

(* Types *)
type orientation = North | East | South | West

let flip = function
  | North -> South
  | East -> West
  | South -> North
  | West -> East

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

module OrientationMap = Map.Make(struct
  type t = orientation
  let compare = compare
end)

type point = {
  mutable neighbors: (point * orientation) OrientationMap.t;
  position: int * int;
}

type point_opt = Point of point | Block | Space

type state = {
  orientation: orientation;
  point: point;
}

(* Print *)
let print_point_pos { position; _ } = print_tuple print_int position

let print_point { neighbors; position } =
  print_tuple print_int position;
  print_string " - [";
  match OrientationMap.find_opt North neighbors with
  | Some ({ position; _ }, _) -> print_string " N: "; print_tuple print_int position;
  | _ -> ();
  match OrientationMap.find_opt South neighbors with
  | Some ({ position; _ }, _) -> print_string " S: "; print_tuple print_int position;
  | _ -> ();
  match OrientationMap.find_opt East neighbors with
  | Some ({ position; _ }, _) -> print_string " E: "; print_tuple print_int position;
  | _ -> ();
  match OrientationMap.find_opt West neighbors with
  | Some ({ position; _ }, _) -> print_string " W: "; print_tuple print_int position;
  | _ -> ();
  print_string "]"

let print_point_opt = function
  | Block -> print_string "Block"
  | Space -> print_string "Space"
  | Point p -> print_point p

let print_state { orientation; point } =
  print_orientation orientation;
  print_string " ";
  print_point point

let print_instruction = function
  | Dist x -> print_int x
  | Turn t -> print_direction t

(* Utility *)
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
  connect_edges initialized transposed true;
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
  print_newline ();
  print_endline "final state:";
  print_state finish_state;
  print_newline ();
  let password = state_to_password finish_state in
  print_endline "password:";
  print_int password;
  print_newline ();;
