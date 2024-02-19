open Myutils

let lines = read_file "./inputs/day8.txt"

let forest_strs = List.map explode lines

let forest = List.map (List.map int_of_string) forest_strs

let tserof = List.map List.rev forest

let rotate_matrix matrix =
  let rec rotate_matrix' matrix' new_matrix =
    match matrix' with
    | [] -> new_matrix
    | [] :: _ -> new_matrix
    | _ ->
      let new_row = List.map List.hd matrix' in
      let new_matrix' = List.map List.tl matrix' in
      rotate_matrix' new_matrix' (new_row :: new_matrix)
  in
  List.rev (rotate_matrix' matrix [])

let rotated_forest = rotate_matrix forest

let tserof_detator = List.map List.rev rotated_forest 

module PairMap = Map.Make(struct
  type t = int * int
  let compare = compare
end)

type pair_map_holder = { mutable cur: int PairMap.t }
type pair_map_holder_views = { mutable cur: int list list PairMap.t }

let forest_map: pair_map_holder = { cur = PairMap.empty }
let views_map: pair_map_holder_views = { cur = PairMap.empty }
let scenic_map: pair_map_holder = { cur = PairMap.empty }

let get_from_map key = if PairMap.mem key forest_map.cur
  then PairMap.find key forest_map.cur
  else 0

let get_from_views key = if PairMap.mem key views_map.cur
  then PairMap.find key views_map.cur
  else []

let get_from_scenic_scores key = if PairMap.mem key scenic_map.cur
  then PairMap.find key scenic_map.cur
  else 0
let increment_map i j = PairMap.update (i, j) (fun x -> match x with | None -> Some 1 | Some y -> Some (y + 1)) forest_map.cur

let map_2d mat fn = ignore (List.mapi (fun i row ->
  ignore (List.mapi (fun j v -> fn i j v ) row);
  print_endline ""
) mat)

let do_row fn row = let rec do_row' row' max j = match row' with
  | [] -> ()
  | t :: ts -> if t > max then (
      fn j; do_row' ts t (j + 1)
    ) else
    do_row' ts max (j + 1) in
  do_row' row (0 - 1) 0

let do_direction fn mat = let rec do_direction' mat' i = match mat' with
  | [] -> ()
  | r :: rs -> do_row (fun j -> forest_map.cur <- fn i j) r; do_direction' rs (i + 1) in
  do_direction' mat 0

let ltr i j = (i, j)

let rtl i j = (i, List.length forest - j - 1)

let ttb i j = (j, i)

let btt i j = (List.length (List.hd forest) - j - 1, i)

let get_increment_map_func fn i j = match fn i j with
  | (r, c) -> increment_map r c

let rotated_im = get_increment_map_func ttb

let reversed_im = get_increment_map_func rtl

let reversed_rotated_im = get_increment_map_func btt

let left_to_right () = do_direction increment_map forest

let right_to_left () = do_direction reversed_im tserof

let top_to_bottom () = do_direction rotated_im rotated_forest

let bottom_to_top () = do_direction reversed_rotated_im tserof_detator

let print_map () = print_newline (); map_2d forest (fun i j _ -> print_int (get_from_map (i, j))); print_newline ()
let print_scenic_scores () = print_newline (); map_2d forest (fun i j _ -> print_int (get_from_scenic_scores (i, j)); print_string " "); print_newline ()
let print_views () = print_newline (); map_2d forest (fun i j _ -> print_tuple print_int (i, j); print_string " -> "; printlist (printlist print_int) (get_from_views(i, j)); print_newline()); print_newline ()

let print_forest () = print_newline (); map_2d forest (fun _ _ v -> print_int v); print_newline ()

let count_zeros () = List.mapi (fun i row -> List.mapi (fun j _ -> if get_from_map (i, j) = 0 then 1 else 0) row) forest |> List.flatten |> List.fold_left (+) 0

let get_top_view () = List.mapi (fun i row -> List.mapi (fun j _ -> get_from_scenic_scores (i, j)) row) forest |> List.flatten |> max_list

let count_visible_trees () = List.length forest * List.length (List.hd forest) - count_zeros ()

let collect_views_row index_convert i row = let rec collect_views_row' row' j = match row' with
  | [] -> ()
  | _ :: ts -> views_map.cur <- PairMap.update (index_convert i j) (fun x -> match x with | None -> Some [ts] | Some y -> Some (ts :: y)) views_map.cur; collect_views_row' ts (j + 1) in
  collect_views_row' row 0

let collect_views_mat index_convert mat = let rec collect_views_mat' mat' i = match mat' with
  | [] -> ()
  | r :: rs -> collect_views_row index_convert i r; collect_views_mat' rs (i + 1) in
  collect_views_mat' mat 0

let collect_views () = collect_views_mat ltr forest; collect_views_mat rtl tserof; collect_views_mat ttb rotated_forest; collect_views_mat btt tserof_detator

let rec count_trees height row = match row with
  | [] -> 0
  | t :: ts -> if height > t then 1 + count_trees height ts else 1

let rec calculate_scenic_score height views = match views with
  | [] -> 1
  | v :: vs -> let trees = count_trees height v in trees * calculate_scenic_score height vs

let calculate_scenic_scores () = map_2d forest (fun i j v -> scenic_map.cur <- PairMap.update (i, j) (fun x -> let scenic_score = calculate_scenic_score v (PairMap.find (i, j) views_map.cur) in match x with | _ -> Some (scenic_score)) scenic_map.cur)

let run () =
  print_newline ();
  print_endline "Got forest:";
  print_forest ();
  left_to_right ();
  right_to_left ();
  top_to_bottom ();
  bottom_to_top ();
  print_endline "Trees visible for each direction:";
  print_map ();
  print_endline "Visible trees count:";
  print_int (count_visible_trees ());
  collect_views ();
  calculate_scenic_scores ();
  (* print_endline "The views for each tree:";
  print_views (); *)
  print_endline "The scenic scores for each tree:";
  print_scenic_scores ();
  print_endline "The top view:";
  print_int (get_top_view ());
  print_newline ();;