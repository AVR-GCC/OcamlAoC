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

let find_first_x_in_range sensors_and_distances start_x y =
  let rec find_first_x_in_range' prev_x cur_x =
    if abs cur_x <= 1 then prev_x else
      match List.find_opt (point_is_in_range (cur_x, y)) sensors_and_distances with
      | None -> find_first_x_in_range' cur_x (cur_x / 2)
      | Some _ -> prev_x in
  find_first_x_in_range' start_x start_x

let points_in_list_in_range ((sx, sy), (ex, ey)) lst = sum @@ List.map (fun (x, y) -> if x <= ex && x >= sx && y <= ey && y >= sy then 1 else 0) lst

let rec find_edges = function
  | [] -> (0, 0, 0)
  | ((x, _), dist) :: [] -> (x, x, dist)
  | ((x, _), dist) :: tl -> 
    let (min_x, max_x, max_dist) = find_edges tl in
    (min x min_x, max x max_x, max dist max_dist)

let count_clear_coordinates_for_y beacons start_x end_x y sensors_and_distances = 
  let (sensors, _) = List.split sensors_and_distances in
  let rec count_clear_coordinates_for_y' x (pos, count) = 
    if x > end_x then (pos, count)
    else
      let is_in_range_of = List.find_opt (point_is_in_range (x, y)) sensors_and_distances in
      match is_in_range_of with
      | None -> count_clear_coordinates_for_y' (x + 1) (Some (x, y), count)
      | Some ((sx, sy), dist) ->
        let next_x = min (sx + dist - abs (y - sy) + 1) end_x in
        let beacons_in_spot = points_in_list_in_range ((x, y), (x, y)) beacons in
        let sensors_in_spot = points_in_list_in_range ((x, y), (x, y)) sensors in
        let is_beacon = beacons_in_spot = 1 in
        let is_sensor = sensors_in_spot = 1 in
        let next_x = if next_x = x then x + 1 else next_x in
        let is_clear = (not is_beacon) && (not is_sensor) in
        if not is_clear then (count_clear_coordinates_for_y' (x + 1) (pos, count)) else
        let beacons_in_range = points_in_list_in_range ((x, y), (next_x, y)) beacons in
        let sensors_in_range = points_in_list_in_range ((x, y), (next_x, y)) sensors in
        let next_count = count + next_x - x - beacons_in_range - sensors_in_range in
        (count_clear_coordinates_for_y' next_x (pos, next_count)) in
  count_clear_coordinates_for_y' start_x (None, 0)

let run () =  print_newline ();
  let sensor_and_distance_and_beacon = List.map process_line lines in
  let (sensor_and_distance, beacons_with_duplicates) = List.split sensor_and_distance_and_beacon in
  let beacons = clear_duplicates beacons_with_duplicates in
  let (sensors, _) = List.split sensor_and_distance in
  print_endline "Sensors:";
  printlist (print_tuple print_int) sensors;
  print_newline ();
  print_newline ();
  print_endline "Beacons:";
  printlist (print_tuple print_int) beacons;
  print_newline ();
  print_newline ();
  print_endline "Sensors and distances:";
  printlist (print_mixed_tuple (print_tuple print_int) print_int) sensor_and_distance;
  print_newline ();
  print_newline ();
  let y = 2000000 in
  let (min_x, max_x, max_dist) = find_edges sensor_and_distance in
  let left_edge = min_x - max_dist in
  let right_edge = max_x + max_dist in
  let left_edge = find_first_x_in_range sensor_and_distance left_edge y in
  let right_edge = find_first_x_in_range sensor_and_distance right_edge y in
  print_endline "Limits:";
  print_int left_edge; print_char ' '; print_int right_edge; print_newline ();
  let (_, count) = count_clear_coordinates_for_y beacons left_edge right_edge y sensor_and_distance in
  print_newline ();
  print_string "Clear spots in y="; print_int y; print_string ": ";
  print_int count;
  print_newline ();;