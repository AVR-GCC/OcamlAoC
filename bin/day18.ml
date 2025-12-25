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

let run () = print_newline ();
  print_endline "Day 18";
  let lines = read_file "./inputs/day18.real.txt" in
  let drops = List.map line_to_drop lines in
  (* printlist print_point drops; *)
  let final_set = add_drops_to_mapping FaceSet.empty drops in
  let surface_area = FaceSet.cardinal final_set in
  print_endline ("Total surface area: " ^ string_of_int surface_area);
  print_newline ();;
