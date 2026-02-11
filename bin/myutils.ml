let printlist printelem lst = let rec middle = function
| [] -> ()
| h::t -> printelem h; if t = [] then () else print_string "; "; middle t in
print_string "["; middle lst; print_string "]"

let print_array printelem arr =
  print_string "";
  Array.iter printelem arr;
  print_string ""

let string_of_option string_of_elem = function
  | None -> "None"
  | Some elem -> string_of_elem elem

let list_to_string elem_to_string lst = let rec middle = function
| [] -> ""
| h::t -> elem_to_string h ^ (if t = [] then "" else ("; " ^ (middle t))) in
"[" ^ (middle lst) ^ "]"

let split_list manipulate delimiter lst =
  let rec get_chunk this_chunk remaining_list = match remaining_list with
  | [] -> (this_chunk, [])
  | h::t -> if h = delimiter
    then (this_chunk, t)
    else get_chunk ((manipulate h)::this_chunk) t in
  let rec get_chunks result_so_far remaining_list = match remaining_list with
  | [] -> result_so_far
  | _::_ -> let chunk = get_chunk [] remaining_list in
  match chunk with
  | (this_chunk, new_remaining_list) -> get_chunks (this_chunk::result_so_far) new_remaining_list in
  get_chunks [] lst

let sum = List.fold_left (+) 0

let min_list = List.fold_left (fun acc elem -> min acc elem) max_int
let max_list = List.fold_left (fun acc elem -> max acc elem) min_int
let min_max_list = List.fold_left (fun (smallest, largest) elem -> (min smallest elem, max largest elem)) (max_int, min_int)

let max_list_by f minval list = List.fold_left (fun acc elem -> if (f acc) > (f elem) then acc else elem) minval list
let max_list_2d lst = max_list (List.map max_list lst)
let min_list_2d lst = min_list (List.map min_list lst)

let rec sorted_lists_overlap l1 l2 =
  match (l1, l2) with
  | ([], _) | (_, []) -> []
  | (h1::t1, h2::t2) ->
      if h1 = h2 then h1::(sorted_lists_overlap t1 t2) else
      if h1 < h2 then sorted_lists_overlap t1 l2 else sorted_lists_overlap l1 t2

let print_tuple prnt tup = match tup with
  | (one, two) -> print_string "("; prnt one; print_string ", "; prnt two; print_string ")"

let explode str = List.rev (String.fold_left (fun acc elem -> (String.make 1 elem)::acc) [] str)
let explode_char str = List.rev (String.fold_left (fun acc elem -> elem::acc) [] str)

let read_file filename = 
  let in_channel = open_in filename in
  let rec read_lines acc = 
    try
      let line = input_line in_channel in
      read_lines (line::acc)
    with End_of_file -> List.rev acc in
  let result = read_lines [] in
  close_in in_channel;
  result

let split_on_chars str chars = let rec split_on_chars' list_acc str_acc i = if i = String.length str then List.rev (str_acc::list_acc) else
  match str.[i] with
  | c when List.mem c chars -> split_on_chars' (str_acc::list_acc) "" (i+1)
  | c -> split_on_chars' list_acc (str_acc ^ Char.escaped c) (i+1) in
  split_on_chars' [] "" 0

let add_to_char c i = Char.chr (Char.code c + i)

let fold_lefti f acc lst = let rec fold_lefti' acc lst i = match lst with
  | [] -> acc
  | h::t -> fold_lefti' (f acc h i) t (i + 1) in
  fold_lefti' acc lst 0

let my_merge_sort cmp lst = let rec merge_two_lists lst1 lst2 = match lst1, lst2 with
  | [], l2 -> l2
  | l1, [] -> l1
  | h1::t1, h2::t2 -> let cmp_res = cmp (h1, h2) in
    if cmp_res < 0 then h1::(merge_two_lists t1 lst2)
    else h2::(merge_two_lists lst1 t2) in
  let rec do_one_run lst = match lst with
  | [] -> []
  | [x] -> [x]
  | a::b::t -> (merge_two_lists a b)::(do_one_run t) in
  let rec do_all_runs mlst = match mlst with
  | [] -> []
  | [x] -> x
  | l -> do_all_runs (do_one_run l) in
  do_all_runs (List.map (fun x -> [x]) lst)

let find_indecies cond = fold_lefti (fun acc v i -> if cond v then i::acc else acc) []

let rec is_sub_str_from_index str sub_str index = match sub_str with
  | [] -> true
  | h::t -> if index >= String.length str then false
    else if h.[0] = str.[index] then is_sub_str_from_index str t (index + 1)
    else false

let split_on_string delim str = let delimiter = explode delim in
  let rec split_on_string' list_acc str_acc i = if i = String.length str then List.rev (str_acc::list_acc) else
  if is_sub_str_from_index str delimiter i then split_on_string' (str_acc::list_acc) "" (i + (String.length delim))
  else split_on_string' list_acc (str_acc ^ Char.escaped str.[i]) (i + 1) in
  split_on_string' [] "" 0

let map_2d mat fn = ignore (List.mapi (fun i row ->
  ignore (List.mapi (fun j v -> fn i j v ) row);
  print_endline ""
) mat)

let map_2d_arr mat fn = ignore (Array.mapi (fun i row ->
  ignore (Array.mapi (fun j v -> fn i j v ) row);
  print_endline ""
) mat)

let split_on_strings delims str = let delimiters = List.map explode delims in
  let rec split_on_string' list_acc str_acc i = if i = String.length str then List.rev (str_acc::list_acc) else
  match List.find_opt (fun delimiter -> is_sub_str_from_index str delimiter i) delimiters with
  | Some (delim) -> split_on_string' (str_acc::list_acc) "" (i + (List.length delim))
  | None -> split_on_string' list_acc (str_acc ^ Char.escaped str.[i]) (i + 1) in
  split_on_string' [] "" 0

let print_mixed_tuple prnt1 prnt2 (x, y) = 
  print_string "(";
  prnt1 x;
  print_string ", ";
  prnt2 y;
  print_string ")"

let print_mixed_triple prnt1 prnt2 prnt3 (x, y, z) = 
  print_string "(";
  prnt1 x;
  print_string ", ";
  prnt2 y;
  print_string ", ";
  prnt3 z;
  print_string ")"

let clear_duplicates lst = let rec clear_duplicates' acc lst = match lst with
  | [] -> List.rev acc
  | h::t -> if List.mem h acc then clear_duplicates' acc t
    else clear_duplicates' (h::acc) t in
  clear_duplicates' [] lst

let print_opt prnt opt = match opt with
  | Some x -> prnt x
  | None -> print_string "None"

module StringMap = Map.Make(String)

let print_string_map print_val map =
  StringMap.iter (fun key value ->
    print_string (key ^ ": ");
    print_val value;
  ) map

let rec apply_n_times f x = function
  | 0 -> x
  | t -> apply_n_times f (f x) (t - 1)

type 'a cycle_node = {
  cid: int;
  value: 'a;
  mutable prev: 'a cycle_node option;
  mutable next: 'a cycle_node option;
}

let print_cycle print_val = function
  | None -> print_endline "None"
  | Some cycle ->
      let rec print_cycle_rec stop_id = function
        | None -> ()
        | Some node ->
            print_string "[";
            print_int node.cid;
            print_string ": ";
            print_val node.value;
            print_string "]";
            print_string " -> ";
            match node.next with
              | None -> () | Some next when next.cid = stop_id -> ()
              | Some next -> print_cycle_rec stop_id (Some next) in
      print_cycle_rec cycle.cid (Some cycle)

let remove_from_cycle cycle_opt =
  match cycle_opt with
  | None -> None
  | Some cycle ->
      match (cycle.prev, cycle.next) with
      | (Some prev, Some next) ->
          prev.next <- Some next;
          next.prev <- Some prev;
          Some next
      | _ -> None

let add_before_node_to_cycle cycle_opt new_node =
  match cycle_opt with
  | None -> new_node
  | Some cycle ->
    new_node.next <- Some cycle;
    new_node.prev <- cycle.prev;
    Option.iter (fun prev ->
      prev.next <- Some new_node
    ) cycle.prev;
    cycle.prev <- Some new_node;
    new_node

let add_node_to_cycle cycle_opt new_node =
  match cycle_opt with
  | None -> new_node
  | Some cycle ->
    new_node.prev <- Some cycle;
    new_node.next <- cycle.next;
    Option.iter (fun next ->
      next.prev <- Some new_node
    ) cycle.next;
    cycle.next <- Some new_node;
    new_node

let add_value_to_cycle cycle_opt value =
  match cycle_opt with
  | None ->
    let new_node = { cid = 0; value = value; prev = None; next = None } in
    new_node.prev <- Some new_node;
    new_node.next <- Some new_node;
    Some new_node
  | Some cycle ->
    let new_node = { cid = cycle.cid + 1; value = value; prev = Some cycle; next = cycle.next } in
    Option.iter (fun next ->
      next.prev <- Some new_node
    ) cycle.next;
    cycle.next <- Some new_node;
    Some new_node

let get_next = function
  | None -> None
  | Some node -> node.next

let get_prev = function
  | None -> None
  | Some node -> node.prev

let navigate_in_cycle cycle size amount =
  if amount = 0 then cycle else
  let mod_amount = amount mod size in
  let (abs_amount, negative) = (abs mod_amount, amount < 0) in
  let func = if negative then get_prev else get_next in
  apply_n_times func cycle abs_amount

type 'a node = {
  id: string;
  value: 'a;
  mutable neighbors: ('a node * int) list;
  mutable distance_map: int array array;
}

let init_two_letter_map () = Array.init 26 (fun _ -> (Array.init 26 (fun _ -> max_int)))

let get_two_letter_map map id = match explode_char id with a::b::[] -> map.(Char.code a - 65).(Char.code b - 65) | _ -> max_int
let set_two_letter_map map id value = match explode_char id with a::b::[] -> map.(Char.code a - 65).(Char.code b - 65) <- value | _ -> ()

let build_graph tuples = let rec build_graph' processed_nodes tups = match tups with
  | [] -> processed_nodes
  | (id, value, neighbors)::t ->
    let new_node = {id = id; value = value; neighbors = []; distance_map = init_two_letter_map ()} in
    new_node.neighbors <- List.fold_left (
      fun acc (neighbor_id, weight) -> match List.find_opt (fun node -> node.id = neighbor_id) processed_nodes with
      | Some neighbor -> neighbor.neighbors <- (new_node, weight)::neighbor.neighbors; (neighbor, weight)::acc
      | None -> acc
    ) [] neighbors;
    build_graph' (new_node::processed_nodes) t in
  build_graph' [] tuples

let update_distance_maps nodes = let rec update_distance_maps_for_node distance origin (node, _) =
  if (distance = 0) then (List.iter (update_distance_maps_for_node 1 node.id) node.neighbors) else
  if not (origin = node.id) && get_two_letter_map node.distance_map origin > distance then (
    set_two_letter_map node.distance_map origin distance;
    List.iter (update_distance_maps_for_node (distance + 1) origin) node.neighbors
  ) in
  List.iter (fun node -> update_distance_maps_for_node 0 node.id (node, 0)) nodes

module StringSet = Set.Make(struct
  type t = string
  let compare = compare
end)

let find_node id node =
  let rec find_node' visited (current, _) =
    if current.id = id then Some current else
    if StringSet.mem current.id visited then None else
    let new_visited = StringSet.add current.id visited in
    let neighbor_results = List.filter_map (find_node' new_visited) current.neighbors in
    if List.length neighbor_results = 0 then None else Some (List.hd neighbor_results) in
  find_node' StringSet.empty (node, 0)

let print_graph prnt graph =
  let rec print_node indentation tree printed (node, weight) =
    if (List.length tree < 2 || not (node.id = (List.hd (List.tl tree)))) then (
      print_string (String.make indentation ' ' ^ node.id);
      if StringSet.mem node.id printed then (print_endline (" - " ^ (string_of_int weight) ^ " (cycle)"); printed)
      else (
        print_string (" - " ^ (string_of_int weight));
        prnt node.value;
        print_endline " ->";
        List.fold_left (fun acc cur -> (print_node (indentation + 2) (node.id::tree) (StringSet.add node.id acc) cur)) printed node.neighbors
      )
    ) else printed in
  ignore (print_node 0 [] StringSet.empty (graph, 0))

let cartesian_product f list1 list2 =
  List.concat_map (fun elem1 ->
    List.map (fun elem2 -> f elem1 elem2) list2
  ) list1

let append_to_file filename content =
  let flags = [Open_wronly; Open_creat; Open_append; Open_text] in
  let permissions = 0o666 in
  let oc = open_out_gen flags permissions filename in
  try
    Printf.fprintf oc "%s\n" content;
    close_out oc
  with e ->
    close_out_noerr oc;
    raise e

let rec merge_sorted comp lst1 lst2 =
  match (lst1, lst2) with
  | ([], _) -> lst2
  | (_, []) -> lst1
  | (h1 :: t1, h2 :: t2) -> if comp h1 h2 then h1 :: merge_sorted comp t1 lst2 else h2 :: merge_sorted comp lst1 t2

let join_list delimiter lst =
  let rec join_list_rec = function
  | [] -> []
  | x :: rest -> x :: delimiter :: join_list_rec rest in
  let pre_final = join_list_rec lst in
  pre_final |> List.rev |> List.tl |> List.rev

type orientation = North | East | South | West

module OrientationMap = Map.Make(struct
  type t = orientation
  let compare = compare
end)

let print_orientation = function
  | North -> print_string "North"
  | East -> print_string "East"
  | South -> print_string "South"
  | West -> print_string "West"

type point = {
  mutable neighbors: (point * orientation) OrientationMap.t;
  position: int * int;
  orientation: orientation option;
}

type point_opt = Point of point | Block | Space

type traveler = {
  orientation: orientation;
  point: point;
}

let print_point_pos { position; _ } = print_tuple print_int position

let print_point { neighbors; position; _ } =
  (* print_opt print_orientation orientation; *)
  (* print_string " "; *)
  print_tuple print_int position;
  print_string " - [";
  (match OrientationMap.find_opt North neighbors with
  | Some ({ position; _ }, _) -> print_string " N: "; print_tuple print_int position
  | _ -> ());
  (match OrientationMap.find_opt South neighbors with
  | Some ({ position; _ }, _) -> print_string " S: "; print_tuple print_int position
  | _ -> ());
  (match OrientationMap.find_opt East neighbors with
  | Some ({ position; _ }, _) -> print_string " E: "; print_tuple print_int position
  | _ -> ());
  (match OrientationMap.find_opt West neighbors with
  | Some ({ position; _ }, _) -> print_string " W: "; print_tuple print_int position
  | _ -> ());
  print_string "]"

let print_point_opt = function
  | Block -> print_string "Block"
  | Space -> ()
  | Point p -> print_point p

let print_traveler { orientation; point } =
  print_orientation orientation;
  print_string " ";
  print_point point

let flip = function
  | North -> South
  | East -> West
  | South -> North
  | West -> East

let get_neighbor point orientation =
  match OrientationMap.find_opt orientation point.neighbors with
  | None -> (point, orientation)
  | Some p -> p

let rec transpose mat = function
  | 0 -> []
  | i ->
      let (col, tail) = List.split @@ List.map (fun row ->
        match row with
        | h :: t -> (h, t)
        | _ -> failwith "row not equalized"
      ) mat in
      col :: transpose tail (i - 1)

let rec initialize_row row x y =
  match row with
  | "." :: rest ->
      let point = Point { neighbors = OrientationMap.empty; position = (x, y); orientation = None } in
      point :: initialize_row rest (x + 1) y
  | "v" :: rest ->
      let point = Point { neighbors = OrientationMap.empty; position = (x, y); orientation = Some South } in
      point :: initialize_row rest (x + 1) y
  | "^" :: rest ->
      let point = Point { neighbors = OrientationMap.empty; position = (x, y); orientation = Some North } in
      point :: initialize_row rest (x + 1) y
  | ">" :: rest ->
      let point = Point { neighbors = OrientationMap.empty; position = (x, y); orientation = Some East } in
      point :: initialize_row rest (x + 1) y
  | "<" :: rest ->
      let point = Point { neighbors = OrientationMap.empty; position = (x, y); orientation = Some West } in
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

let connect_edges mat tra =
    connect_row_ends mat;
    connect_col_ends tra

let connect_points ({ neighbors = ln; _ } as l) ({ neighbors = rn; _ } as r) ltr rtl =
    l.neighbors <- OrientationMap.add ltr (r, flip rtl) ln;
    r.neighbors <- OrientationMap.add rtl (l, flip ltr) rn

let rec connect_edge e1 e2 rtl ltr =
  match (e1, e2) with
  | (Point h1 :: t1, Point h2 :: t2) -> connect_points h1 h2 rtl ltr; connect_edge t1 t2 rtl ltr
  | (_ :: t1, _ :: t2) -> connect_edge t1 t2 rtl ltr
  | ([], []) -> ()
  | _ -> failwith "different sized squares"

let exploded_to_points exploded len = 
  let initialized = initialize_mat exploded in
  let transposed = transpose initialized len in
  connect_rows initialized;
  connect_cols transposed;
  (* printlist (fun l -> printlist print_point_opt l; print_newline ()) initialized; *)
  (initialized, transposed)

module TupleMap = Map.Make(struct
  type t = int * int
  let compare = compare
end)

module TupleSet = Set.Make(struct
  type t = int * int
  let compare = compare
end)
