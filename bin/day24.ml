open Myutils

(* Print *)
let rec print_field_row blizzard_map (ex, ey) = function
  | [] -> ()
  | Point { position = (x, y); _ } :: t -> (
      if x = ex && y = ey then print_string "E" else (
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

(* Expidition *)
let moves = [Some South; Some East; Some West; Some North; None]

let move_possible bliz point = function
  | None -> if TupleMap.mem point.position bliz then None else Some point
  | Some ori ->
      let ({ position = (nx, ny); _ } as np, _) = get_neighbor point ori in
      if TupleMap.mem (nx, ny) bliz then None else
      let (cx, cy) = point.position in
      if cx = nx && cy = ny then None else
      match ori with
      | North -> if cy > ny then None else Some np
      | South -> if cy < ny then None else Some np
      | West -> if cx > nx then None else Some np
      | East -> if cx < nx then None else Some np

let rec do_walk bliz height exp =
  let { position = (x, y); _ } = exp in
  if y = height then 0 else
  let coming_bliz = move_blizzards bliz in
  let move_options = List.map (move_possible coming_bliz exp) moves in
  let next_points = List.filter_map Fun.id move_options in
  print_tuple print_int (x, y);
  print_string " -> ";
  printlist print_point_pos next_points;
  print_newline ();
  let walks = List.map (do_walk coming_bliz height) next_points in
  let minutes = 1 + min_list walks in
  print_string ": ";
  print_int minutes;
  minutes

let run () = print_newline ();
  print_endline "Day 24";
  let lines = read_file "./inputs/day24.test.txt" in
  let exploded = List.map explode lines in
  (* print_newline (); *)
  (* printlist (fun x -> printlist print_string x; print_newline ()) exploded; *)
  let width = List.length @@ List.hd exploded in
  let height = List.length exploded in
  let points = create_and_connect_points exploded width in
  let blizzards = collect_blizzards points in
  let exp_opt = List.hd @@ List.tl @@ List.hd points in
  match exp_opt with
  | Block | Space -> failwith "Bad start position"
  | Point exp -> (
    let minutes = do_walk blizzards height exp in
    print_int minutes;
  );
  print_newline ();
  print_newline ();;
