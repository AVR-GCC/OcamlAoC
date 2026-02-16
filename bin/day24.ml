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

let rec print_path consts blizzards path =
  let (points, all_blizzards) = consts in
  let (b, bl) = match blizzards with
  | bliz :: rest -> (bliz, rest)
  | [] -> (List.hd all_blizzards, List.tl all_blizzards) in
  match path with
  | p :: pa -> print_field b p points; print_newline (); print_path consts bl pa
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
let moves = [Some East; Some South; Some West; Some North; None]

let move_possible bliz point = function
  | None -> if TupleMap.mem point.position bliz then None else Some point
  | Some ori ->
      let ({ position = (nx, ny); _ } as np, _) = get_neighbor point ori in
      if TupleMap.mem (nx, ny) bliz then None else
      let (cx, cy) = point.position in
      if cx = nx && cy = ny then None else
      if nx = 1 && ny = 0 then None else
      match ori with
      | North -> if cy < ny then None else Some np
      | South -> if cy > ny then None else Some np
      | West -> if cx < nx then None else Some np
      | East -> if cx > nx then None else Some np

let rec march consts blizzards step limit mem point =
  let (height, width, all_blizzards, _points) = consts in
  let { position = (x, y); _ } = point in
  if limit < (width - 2 - x) + (height - 1 - y) then (None, mem) else ( (* Longer than path already found *)
  if height - 1 = y then (Some [(x, y)], mem) else ( (* Reached end *)
  let map_key = (x, y, step mod (List.length all_blizzards)) in
  match ThrupleMap.find_opt map_key mem with
  | Some path-> (path, mem) (* Found memoized position *)
  | None -> (
  let (cur_blizzard, rem_blizzards) = match blizzards with
  | cur_blizzard :: rem_blizzards -> (cur_blizzard, rem_blizzards)
  | [] -> (List.hd all_blizzards, List.tl all_blizzards) in
  let possible_points_opts = List.map (move_possible cur_blizzard point) moves in
  let possible_points = List.filter_map (fun x -> x) possible_points_opts in
  if List.length possible_points = 0 then (None, mem) else ( (* Dead end *)
  let (path_opt, memo) = List.fold_left (fun (path_opt, memoi) next_point ->
    match path_opt with
    | None -> march consts rem_blizzards (step + 1) (limit - 1) memoi next_point
    | Some path ->
        match march consts rem_blizzards (step + 1) (List.length path - 1) memoi next_point with
        | (None, memoiz) -> (Some path, memoiz)
        | res -> res
  ) (None, mem) possible_points in
  let final_path = match path_opt with
  | None -> None
  | Some path -> Some ((x, y) :: path) in
  let memoi = ThrupleMap.add map_key final_path memo in
  (final_path, memoi)
  )
  )
  )
  )

let run () = print_newline ();
  print_endline "Day 24";
  let lines = read_file "./inputs/day24.real.txt" in
  let exploded = List.map explode lines in
  (* print_newline (); *)
  (* printlist (fun x -> printlist print_string x; print_newline ()) exploded; *)
  let width = List.length @@ List.hd exploded in
  let height = List.length exploded in
  let points = create_and_connect_points exploded width in
  let blizzard_cycle = lcm (width - 2) (height - 2) in
  let base_blizzards = collect_blizzards points in
  let all_blizzards = get_all_blizzard_positions base_blizzards blizzard_cycle in
  let exp_opt = List.hd @@ List.tl @@ List.hd points in
  match exp_opt with
  | Block | Space -> failwith "Bad start position"
  | Point exp -> (
    let s1 = Sys.time () in
    (* let consts = (height, width, all_blizzards, blizzard_cycle, points, s1) in *)
    (* let (minutes, path, _) = do_walk consts (List.tl all_blizzards) 0 max_int ThrupleMap.empty exp false in *)
    let consts = (height, width, all_blizzards, points) in
    let (path_opt, _) = march consts (List.tl all_blizzards) 0 max_int ThrupleMap.empty exp in
    match path_opt with
    | None -> print_endline "No path found"
    | Some path ->
    let s2 = Sys.time () in
    print_endline ("time:" ^ string_of_float (s2 -. s1));
    print_int @@ List.length path - 1;
    print_newline ();
    printlist (print_tuple print_int) path;
    print_newline ();
    print_path (points, all_blizzards) all_blizzards path;
  );
  print_newline ();
  print_newline ();;
