open Myutils

let lines = read_file "./inputs/day15real.txt"

let dist_func (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

let process_line line = line
|> split_on_strings ["Sensor at x="; ", y="; ": closest beacon is at x="; ", y="]
|> List.tl
|> List.map int_of_string
|> fun lst -> match lst with
  | [x; y; bx; by] -> (((x, y), dist_func (x, y) (bx, by)), (bx, by))
  | _ -> failwith "Invalid input"

let point_is_in_range (x, y) ((sx, sy), dist) = dist_func (x, y) (sx, sy) <= dist

let point_is_in_list point lst = List.exists (fun x -> x = point) lst

let rec find_edges = function
  | [] -> (0, 0, 0)
  | ((x, _), dist) :: [] -> (x, x, dist)
  | ((x, _), dist) :: tl -> 
    let (min_x, max_x, max_dist) = find_edges tl in
    (min x min_x, max x max_x, max dist max_dist)

let count_clear_coordinates_for_y beacons start_x end_x y sensors_and_distances = 
  let (sensors, _) = List.split sensors_and_distances in
  let rec count_clear_coordinates_for_y' x acc = 
    if x > end_x then acc
    else
      let is_in_range = List.exists (point_is_in_range (x, y)) sensors_and_distances in
      let is_beacon = point_is_in_list (x, y) beacons in
      let is_sensor = point_is_in_list (x, y) sensors in
      count_clear_coordinates_for_y' (x + 1) (if is_in_range && not is_beacon && not is_sensor then acc + 1 else acc) in
  count_clear_coordinates_for_y' start_x 0

let run () =  print_newline ();
  let sensor_and_distance_and_beacon = List.map process_line lines in
  let (sensor_and_distance, beacons) = List.split sensor_and_distance_and_beacon in
  printlist (print_mixed_tuple (print_tuple print_int) print_int) sensor_and_distance;
  print_newline ();
  print_newline ();
  let (min_x, max_x, max_dist) = find_edges sensor_and_distance in
  let left_edge = min_x - max_dist in
  let right_edge = max_x + max_dist in
  print_int (count_clear_coordinates_for_y beacons left_edge right_edge 2000000 sensor_and_distance);
  print_newline ();;