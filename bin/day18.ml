open Myutils

type point = {
  x: int;
  y: int;
  z: int;
}

type direction = X | Y | Z

type face = {
  point: point;
  direction: direction;
}

module Face = struct
  type t = face
  let compare = compare
end

module FaceSet = Set.Make(Face)

module Point = struct
  type t = point
  let compare = compare
end

module PointSet = Set.Make(Point)

(* type visit = { *)
(*   visited: int StringMap.t; *)
(*   opened: StringSet.t; *)
(*   opened_valve: bool; *)
(*   pressure_released: int; *)
(*   valve: valve node; *)
(* } *)
(**)
(* type double_visit = { *)
(*   visited: int StringMap.t; *)
(*   opened: StringSet.t; *)
(*   opened_valve: bool * bool; *)
(*   pressure_released: int; *)
(*   valves: valve node * valve node; *)
(* } *)

let print_point point =
  print_endline ("(" ^ string_of_int point.x ^ ", " ^ string_of_int point.y ^ ", " ^ string_of_int point.z ^ ")")

let line_to_drop line =
  match (String.split_on_char ',' line
  |> List.map (fun x -> int_of_string x)) with
  | [x; y; z] -> { x = x; y = y; z = z }
  | _ -> failwith "Malformed line"

let point_to_faces point =
  [
    { direction = X; point = point };
    { direction = X; point = { x = point.x - 1; y = point.y; z = point.z }};
    { direction = Y; point = point };
    { direction = Y; point = { x = point.x; y = point.y - 1; z = point.z }};
    { direction = Z; point = point };
    { direction = Z; point = { x = point.x; y = point.y; z = point.z - 1 }};
  ]

let pass_face point face =
  match face.direction with
  | X -> if face.point.x = point.x then { x = point.x + 1; y = point.y; z = point.z } else { x = point.x - 1; y = point.y; z = point.z }
  | Y -> if face.point.y = point.y then { x = point.x; y = point.y + 1; z = point.z } else { x = point.x; y = point.y - 1; z = point.z }
  | Z -> if face.point.z = point.z then { x = point.x; y = point.y; z = point.z + 1 } else { x = point.x; y = point.y; z = point.z - 1 }

let add_face_to_mapping exposed_faces face =
  if FaceSet.mem face exposed_faces then FaceSet.remove face exposed_faces else FaceSet.add face exposed_faces

let rec add_faces_to_mapping exposed_faces faces =
  match faces with
  | [] -> exposed_faces
  | face :: rest -> add_faces_to_mapping (add_face_to_mapping exposed_faces face) rest

let add_drop_to_mapping exposed_faces drop =
  let point_faces = point_to_faces drop in
  add_faces_to_mapping exposed_faces point_faces

let rec add_drops_to_mapping exposed_faces drops =
  match drops with
  | [] -> exposed_faces
  | drop :: rest -> add_drops_to_mapping (add_drop_to_mapping exposed_faces drop) rest

let rec get_coordinates points =
  match points with
  | [] -> ([], [], [])
  | point :: rest -> 
      match get_coordinates rest with
      | (xs, ys, zs) -> (point.x :: xs, point.y :: ys, point.z :: zs)

let get_bounding_box points =
  match get_coordinates points with
  | (xs, ys, zs) -> (min_max_list xs, min_max_list ys, min_max_list zs)

let rec stream_expand_face constants covered current =
  let (point, face) = current in
  let (all_surface, _) = constants in
  let (outer_surfice, points_covered) = covered in
  if FaceSet.mem face all_surface then (FaceSet.add face outer_surfice, points_covered) else
  steam_expand constants (outer_surfice, points_covered) (pass_face point face)
  
and steam_expand constants covered point =
  let (_, bounds) = constants in
  let (outer_surfice, points_covered) = covered in
  let ((min_x, max_x), (min_y, max_y), (min_z, max_z)) = bounds in
  if point.x > max_x + 1 || point.x < min_x - 1 then covered else
  if point.y > max_y + 1 || point.y < min_y - 1 then covered else
  if point.z > max_z + 1 || point.z < min_z - 1 then covered else
  if PointSet.mem point points_covered then covered else 
  let new_covered = (outer_surfice, PointSet.add point points_covered) in
  let point_faces = point_to_faces point in
  let fold_func = fun cur_covered face -> stream_expand_face constants cur_covered (point, face) in
  List.fold_left fold_func new_covered point_faces

let run () = print_newline ();
  print_endline "Day 18";
  let lines = read_file "./inputs/day18.real.txt" in
  let drops = List.map line_to_drop lines in
  let all_surface = add_drops_to_mapping FaceSet.empty drops in
  let surface_area = FaceSet.cardinal all_surface in
  print_endline ("Total surface area: " ^ string_of_int surface_area);
  let bounds = get_bounding_box drops in
  let ((min_x, _), (min_y, _), (min_z, _)) = bounds in
  let start_point = { x = min_x - 1; y = min_y - 1; z = min_z -1 } in
  let (outer_surface, _) = steam_expand (all_surface, bounds) (FaceSet.empty, PointSet.empty) start_point in
  let surface_area = FaceSet.cardinal outer_surface in
  print_endline ("Outer surface area: " ^ string_of_int surface_area);
  print_newline ();;
