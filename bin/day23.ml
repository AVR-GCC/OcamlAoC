open Myutils

let orientations = [North; South; West; East]

let create_set exploded =
  let indecies = List.map (find_indecies (fun c -> c = "#")) exploded in
  let tuples = List.mapi (fun y row_ind -> List.map (fun x -> (x, y)) row_ind) indecies in
  let list = List.flatten tuples in
  TupleSet.of_list list

let not_clear set (x, y) =
  TupleSet.mem (x - 1, y - 1) set
  || TupleSet.mem (x, y - 1) set
  || TupleSet.mem (x + 1, y - 1) set
  || TupleSet.mem (x - 1, y) set
  || TupleSet.mem (x + 1, y) set
  || TupleSet.mem (x - 1, y + 1) set
  || TupleSet.mem (x, y + 1) set
  || TupleSet.mem (x + 1, y + 1) set

let make_ori_proposal set (x, y) = function
  | North -> if TupleSet.mem (x - 1, y - 1) set || TupleSet.mem (x, y - 1) set || TupleSet.mem (x + 1, y - 1) set then None else Some (x, y - 1)
  | South -> if TupleSet.mem (x - 1, y + 1) set || TupleSet.mem (x, y + 1) set || TupleSet.mem (x + 1, y + 1) set then None else Some (x, y + 1)
  | West -> if TupleSet.mem (x - 1, y - 1) set || TupleSet.mem (x - 1, y) set || TupleSet.mem (x - 1, y + 1) set then None else Some (x - 1, y)
  | East -> if TupleSet.mem (x + 1, y - 1) set || TupleSet.mem (x + 1, y) set || TupleSet.mem (x + 1, y + 1) set then None else Some (x + 1, y)

let rec make_proposal set ori_ord tup =
  if not_clear set tup then
    match ori_ord with
    | [] -> None
    | ori :: t -> match make_ori_proposal set tup ori with
        | None -> make_proposal set t tup
        | Some prop -> Some prop
  else None

let increment_orientation_list = function
  | [] -> failwith "orientations empty"
  | h :: t -> t @ [h]

let round_first_half set ori_ord coor (map, moved) =
  let prop = make_proposal set ori_ord coor in
  (* print_tuple print_int coor; *)
  (* print_string " -> "; *)
  (* print_opt (print_tuple print_int) prop; *)
  (* print_newline (); *)
  match prop with
  | None -> (TupleMap.add coor coor map, moved)
  | Some dest ->
      match TupleMap.find_opt dest map with
      | None -> (TupleMap.add dest coor map, moved + 1)
      | Some other ->
          let next_map = map
            |> TupleMap.remove dest
            |> TupleMap.add other other
            |> TupleMap.add coor coor in
          (next_map, moved - 1)

let round_second_half (x, y) _ set = TupleSet.add (x, y) set

let find_edges set =
  TupleSet.fold (fun (x, y) (minx, miny, maxx, maxy) -> (min x minx, min y miny, max x maxx, max y maxy)) set (max_int, max_int, min_int, min_int)

let print_grove set =
  let (minx, miny, maxx, maxy) = find_edges set in
  (* printlist print_int [minx; miny; maxx; maxy]; *)
  (* print_newline (); *)
  let yrange = List.init (maxy - miny + 1) (fun _ -> 0) in
  let xrange = List.init (maxx - minx + 1) (fun _ -> 0) in
  List.iteri (fun y _ ->
    List.iteri (fun x _ ->
      let str = if TupleSet.mem (x + minx, y + miny) set then "#" else "." in
      print_string str
    ) xrange;
    print_newline ()
  ) yrange;
  print_newline ()

let rec do_process set ori_ord x part_1 =
  (* printlist print_orientation ori_ord; *)
  (* print_newline (); *)
  let (map, moved) = TupleSet.fold (round_first_half set ori_ord) set (TupleMap.empty, 0) in
  (* print_grove set; *)
  if moved = 0 || (part_1 && x = 10) then (set, x) else
  let new_set = TupleMap.fold round_second_half map TupleSet.empty in
  let new_ord = increment_orientation_list ori_ord in
  do_process new_set new_ord (x + 1) part_1

let count_empty_tiles set = 
  let (minx, miny, maxx, maxy) = find_edges set in
  let yrange = List.init (maxy - miny + 1) (fun x -> x) in
  let xrange = List.init (maxx - minx + 1) (fun x -> x) in
  List.fold_left (fun accy y ->
    let tiles_in_row = List.fold_left (fun accx x ->
      let inc = if TupleSet.mem (x + minx, y + miny) set then 0 else 1 in
      accx + inc
    ) 0 xrange in
    accy + tiles_in_row
  ) 0 yrange

let run () = print_newline ();
  print_endline "Day 23";
  let lines = read_file "./inputs/day23.test.txt" in
  let exploded = List.map explode lines in
  print_newline ();
  (* printlist (fun x -> printlist print_string x; print_newline ()) exploded; *)
  let start_set = create_set exploded in
  let (end_set, rounds) = do_process start_set orientations 0 false in
  print_newline ();
  print_grove end_set;
  let empty_tiles = count_empty_tiles end_set in
  print_newline ();
  print_int empty_tiles;
  print_newline ();
  print_int @@ rounds + 1;
  (* TupleSet.iter (print_tuple print_int) start_set; *)
  print_newline ();;
