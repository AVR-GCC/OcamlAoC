open Myutils

module ThrupleMap = Map.Make(struct
  type t = int * int * int
  let compare = compare
end)

(* Print *)
let rec print_field_row blizzard_map (ex, ey) = function
  | [] -> ()
  | Point { position = (x, y); _ } :: t -> (
      if x = ex && y = ey then (print_string @@ if TupleMap.mem (x, y) blizzard_map then "X" else "E") else (
        match TupleMap.find_opt (x, y) blizzard_map with
        | None -> print_string "."
        | Some ls -> (
            match ls with
            | [] -> ()
            | [{ orientation; _ }] -> (
                match orientation with
                | North -> print_string "^"
                | South -> print_string "v"
                | West -> print_string "<"
                | East -> print_string ">"
            )
            | _ -> print_int @@ List.length ls
        );
      );
      print_field_row blizzard_map (ex, ey) t
  )
  | _ :: t -> print_string "#"; print_field_row blizzard_map (ex, ey) t

let print_field blizzard_map expedition points = ignore @@ List.map (fun row -> print_field_row blizzard_map expedition row; print_newline ()) points

let rec print_path consts blizzards path step =
  let (points, all_blizzards) = consts in
  let (b, bl) = match blizzards with
  | bliz :: rest -> (bliz, rest)
  | [] -> (List.hd all_blizzards, List.tl all_blizzards) in
  match path with
  | p :: pa -> print_int step; print_newline (); print_field b p points; print_newline (); print_path consts bl pa @@ step + 1
  | [] -> ()

(* Connect *)
let connect_opposite_walls mat tra =
  let height = List.length mat in
  let width = List.length tra in
  let clipped_top_and_bottom = List.take (height - 2) @@ List.tl mat in
  List.iter (fun row ->
    let left_opt = List.hd (List.tl row) in
    let right_opt = List.nth row (width - 2) in
    match (left_opt, right_opt) with
    | (Point left, Point right) -> connect_points_horizontal right left
    | _ -> failwith "Bad field"
  ) clipped_top_and_bottom;
  let clipped_left_and_right = List.take (width - 4) @@ List.tl @@ List.tl tra in
  List.iter (fun col ->
    let top_opt = List.hd (List.tl col) in
    let bottom_opt = List.nth col (height - 2) in
    match (top_opt, bottom_opt) with
    | (Point top, Point bottom) -> connect_points_vertical bottom top
    | _ -> failwith "Bad field"
  ) clipped_left_and_right

let create_and_connect_points exploded len =
  let (initialized, transposed) = exploded_to_points exploded len in
  connect_opposite_walls initialized transposed;
  initialized

(* Blizzard *)
let rec map_blizzards = function
  | [] -> TupleMap.empty
  | { point = { position; _ }; _ } as b :: t ->
      let map = map_blizzards t in
      match TupleMap.find_opt position map with
      | None -> TupleMap.add position [b] map
      | Some ls -> TupleMap.add position (b :: ls) map

let rec collect_blizzards_row row =
  match row with
  | [] -> []
  | Point ({ orientation; _ } as p) :: t -> (
      match orientation with
      | None -> collect_blizzards_row t
      | Some ori -> { point = p; orientation = ori } :: collect_blizzards_row t
  )
  | _ :: t -> collect_blizzards_row t

let collect_blizzards ps = ps |> List.map collect_blizzards_row |> List.flatten |> map_blizzards

let move_blizzards map =
  let rec move_blizzards_rec = function
    | [] -> []
    | { point; orientation } :: t ->
        let (np, _) = get_neighbor point orientation in
        { point = np; orientation = orientation } :: move_blizzards_rec t in
  let (_, lst_lst) = List.split @@ TupleMap.to_list map in
  let lst = List.flatten lst_lst in
  let nlst = move_blizzards_rec lst in
  map_blizzards nlst

let rec get_all_blizzard_positions blizzards = function
  | 0 -> []
  | x -> blizzards :: (get_all_blizzard_positions (move_blizzards blizzards) (x - 1))

(* Expidition *)
let move_possible bliz point start_y = function
  | None -> if TupleMap.mem point.position bliz then None else Some point
  | Some ori ->
      let ({ position = (nx, ny); _ } as np, _) = get_neighbor point ori in
      if TupleMap.mem (nx, ny) bliz then None else
      let (cx, cy) = point.position in
      if cx = nx && cy = ny then None else
      if ny = start_y then None else
      match ori with
      | North -> if cy <= ny then None else Some np
      | South -> if cy >= ny then None else Some np
      | West -> if cx <= nx then None else Some np
      | East -> if cx >= nx then None else Some np

let rec march consts blizzards step limit mem point =
  let (end_height, end_width, start_y, all_blizzards, points, moves, print) = consts in
  let { position = (x, y); _ } = point in
  if print then (
    print_tuple print_int (x, y);
    print_newline ();
  );
  if limit < abs (end_width - x) + abs (end_height - y) then (None, mem) else ( (* Longer than path already found *)
  if end_height = y then (Some [(x, y)], mem) else ( (* Reached end *)
  let map_key = (x, y, step mod (List.length all_blizzards)) in
  match ThrupleMap.find_opt map_key mem with
  | Some path-> (path, mem) (* Found memoized position *)
  | None -> (
  let (cur_blizzard, rem_blizzards) = match blizzards with
  | cur_blizzard :: rem_blizzards -> (cur_blizzard, rem_blizzards)
  | [] -> (List.hd all_blizzards, List.tl all_blizzards) in
  let possible_points_opts = List.map (move_possible cur_blizzard point start_y) moves in
  let possible_points = List.filter_map (fun x -> x) possible_points_opts in
  if print then (
    printlist print_point_pos possible_points;
    print_newline ();
    print_field cur_blizzard (x, y) points;
    print_newline ();
  );
  if List.length possible_points = 0 then (None, mem) else ( (* Dead end *)
  let (path_opt, memo) = List.fold_left (fun (path_opt, memoi) next_point ->
    match path_opt with
    | None -> march consts rem_blizzards (step + 1) (limit - 1) memoi next_point
    | Some path ->
        match march consts rem_blizzards (step + 1) (List.length path - 1) memoi next_point with
        | (None, memoiz) -> (Some path, memoiz)
        | (Some opto, memoiz)-> if List.length opto < List.length path then (Some opto, memoiz) else (Some path, memoiz)
  ) (None, mem) possible_points in
  let final_path = match path_opt with
  | None -> None
  | Some path -> Some ((x, y) :: path) in
  let memoi = ThrupleMap.add map_key final_path memo in
  (final_path, memoi)
  ))))

let run () = print_newline ();
  print_endline "Day 24";
  let lines = read_file "./inputs/day24.real.txt" in
  let exploded = List.map explode lines in
  let width = List.length @@ List.hd exploded in
  let height = List.length exploded in
  let points = create_and_connect_points exploded width in
  let blizzard_cycle = lcm (width - 2) (height - 2) in
  let base_blizzards = collect_blizzards points in
  let all_blizzards = get_all_blizzard_positions base_blizzards blizzard_cycle in
  let exp_opt = List.hd @@ List.tl @@ List.hd points in
  let moves = [Some East; Some South; Some West; Some North; None] in
  match exp_opt with
  | Block | Space -> failwith "Bad start position"
  | Point exp -> (
    let consts = (height - 1, width - 2, 0, all_blizzards, points, moves, false) in
    let (path_opt, _) = march consts (List.tl all_blizzards) 0 max_int ThrupleMap.empty exp in
    match path_opt with
    | None -> print_endline "No path found"
    | Some path ->
    let first_trip_time = List.length path - 1 in
    print_endline "first trip:";
    print_int first_trip_time;
    print_newline ();
    let blizzard_index_return = first_trip_time mod List.length all_blizzards + 1 in
    let return_trip_blizzards = List.drop blizzard_index_return all_blizzards in
    let return_trip_start = List.nth (List.nth points (height - 1)) (width - 2) in
    match return_trip_start with
    | Block | Space -> failwith "Bad start position"
    | Point ret_exp ->
    let ret_moves = [Some West; Some North; Some East; Some South; None] in
    let ret_consts = (0, 1, height - 1, all_blizzards, points, ret_moves, false) in
    let (ret_path_opt, _) = march ret_consts return_trip_blizzards 0 max_int ThrupleMap.empty ret_exp in
    match ret_path_opt with
    | None -> print_endline "No path found"
    | Some ret_path ->
    let second_trip_time = List.length ret_path - 1 in
    print_endline "second trip:";
    print_int second_trip_time;
    print_newline ();
    let both_trip_time = first_trip_time + second_trip_time in
    let blizzard_index_restart = both_trip_time mod List.length all_blizzards + 1 in
    let restart_trip_blizzards = List.drop blizzard_index_restart all_blizzards in
    let (res_path_opt, _) = march consts restart_trip_blizzards 0 max_int ThrupleMap.empty exp in
    match res_path_opt with
    | None -> print_endline "No path found"
    | Some res_path ->
    let third_trip_time = List.length res_path - 1 in
    print_endline "third trip:";
    print_int third_trip_time;
    print_newline ();
    print_endline "total:";
    print_int (third_trip_time + both_trip_time);
  );
  print_newline ();
  print_newline ();;
