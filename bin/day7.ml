open Myutils

let lines = read_file "./inputs/day7.txt"

type fs_item =
  | File of string * int
  | Dir of string * int * fs_item list
  | None

let print_fs_item_name itm = match itm with
  | Dir (name, _, _) | File (name, _) -> print_string name
  | _ -> print_string "-"

let print_fs_item itm = let rec print_fs_item_ind indentation item = print_string indentation; match item with
  | File (_, size) -> print_string "- "; print_fs_item_name item; print_string " (file, size="; print_int size; print_string ") "; print_string "\n"
  | Dir (_, size, lst) -> print_string "- "; print_fs_item_name item; print_string " (dir, size="; print_int size; print_string ") "; print_string "\n"; List.fold_left (fun _ elem -> print_fs_item_ind (indentation ^ "  ") elem) () lst
  | None -> () in
  print_fs_item_ind "" itm

let root = Dir ("//", 0, [])

type fs_holder = { mutable cur: fs_item; }
type callstack_holder = { mutable cur: fs_item list; }
type sizes_holder = { mutable cur: int list; }

let fs: fs_holder = { cur = root }

let callstack: callstack_holder = { cur = [] }

let sizes_less_than_100000_holder: sizes_holder = { cur = [] }
let sizes_more_than_2080344_holder: sizes_holder = { cur = [] }

let print_fs title = print_endline title; print_fs_item fs.cur; print_endline ""

let add_item item dir = match dir with
  | Dir (name, size, lst) -> (match item with
    | Dir (child_name, child_size, child_lst) -> Dir (name, size + child_size, (Dir (child_name, child_size, child_lst))::lst)
    | File (child_name, child_size) -> Dir (name, size + child_size, (File (child_name, child_size))::lst)
    | None -> Dir (name, size, lst))
  | _ -> dir

let rec find_item name lst = match lst with
  | h::t -> (match h with
    | Dir (h_name, _, _) | File (h_name, _) -> if name = h_name then h else find_item name t
    | _ -> None)
  | _ -> None

let update_fs title fn = fs.cur <- fn fs.cur; print_fs (title); printlist print_fs_item_name callstack.cur; print_endline ""; print_endline ""

let push_callstack node = callstack.cur <- node::callstack.cur

let pop_callstack () = match callstack.cur with
  | [] -> callstack.cur <- []; None
  | h::t -> callstack.cur <- t; h

let cd path pwd = match pwd with
  | Dir (_, _, lst) -> if path = ".." then
    pop_callstack () else (push_callstack pwd; (find_item path lst))
  | _ -> None

let commands = List.tl (String.split_on_char '$' (String.concat "\n" lines))

let substring_deltas str start_delta end_delta = let sub_len = String.length str - end_delta - start_delta in String.sub str start_delta sub_len

let line_to_file_tuple line = let splitted = String.split_on_char ' ' line in if line.[0] = 'd'
  then (List.hd (List.tl splitted), -1)
  else (
    List.hd (List.tl splitted),
    int_of_string (List.hd splitted)
  )

let ls_lines_to_tuples = List.map line_to_file_tuple

let tuple_table = Hashtbl.create 200

let populate_tuple_table cmnds = let rec populate_tuple_table_ind path cmds = match cmds with
  | cd_line::ls_line::t -> (match String.split_on_char ' ' cd_line with
    | _::"cd"::name::_  ->
      if ((String.equal name "..\n") || (String.equal name "..")) then
        let new_path = (List.tl (List.tl path)) in
        populate_tuple_table_ind new_path (ls_line::t)
      else
        let new_path = "/"::(String.trim name)::path in
        let path_str = String.concat "" (List.rev new_path) in
        let splitted = String.split_on_char '\n' (String.trim ls_line) in
        Hashtbl.add tuple_table path_str (ls_lines_to_tuples (List.tl splitted));
        populate_tuple_table_ind new_path t
    | _ -> populate_tuple_table_ind path (ls_line::t))
  | _ -> () in
  print_endline "Populating tuple table...";
  printlist print_string cmnds;
  populate_tuple_table_ind [] cmnds

let print_tuple tup = match tup with
  | (name, size) -> Printf.printf "(%s, %i)" name size

let print_ht ht = Hashtbl.iter (fun x y -> print_endline ""; print_string x; print_string " -> "; printlist print_tuple y) ht

let add_dir_size new_size = sizes_less_than_100000_holder.cur <- new_size::sizes_less_than_100000_holder.cur
let add_dir_size2 new_size = sizes_more_than_2080344_holder.cur <- new_size::sizes_more_than_2080344_holder.cur

let rec tuple_to_fs_item path tup = match tup with
  | (name, size) -> if size = -1 then
    let new_path = path ^ name ^ "/" in
    let child_list = List.map (tuple_to_fs_item new_path) (Hashtbl.find tuple_table new_path) in
    let total_size = List.fold_left (fun acc elem -> match elem with
      | File (_, size) -> acc + size
      | Dir (_, size, _) -> acc + size
      | _ -> acc) 0 child_list in
    if total_size < 100000 then add_dir_size total_size;
    if total_size > 2080344 then add_dir_size2 total_size;
    Dir (name, total_size, child_list)
    else File (name, size)

let build_fs () = fs.cur <- tuple_to_fs_item "" ("/", -1)

(* let a_dir = Dir ("a", [])*)
(* let f_file = File ("f", 29116)
let g_file = File ("g", 2557)

let a_dir = Dir ("a", [])

let j_file = File ("j", 4060174)
let k_file = File ("k", 7214296)

let d_dir = Dir ("d", [])

let pack_a = add_item g_file (add_item f_file a_dir)

let pack_d = add_item j_file (add_item k_file d_dir) *)


(* let fs = Dir ("/", [
  Dir ("a", [
    Dir ("e", [
      File ("i", 584);
    ]);
    File ("f", 29116);
    File ("g", 2557);
    File ("h.lst", 62596);
  ]);
  File ("b.txt", 14848514);
  File ("c.dat", 8504156);
  Dir ("d", [
    File ("j", 4060174);
    File ("d.log", 8033020);
    File ("d.ext", 5626152);
    File ("k", 7214296);
  ])
]) *)

let run () =
  populate_tuple_table commands;
  print_endline "";
  print_endline "";
  print_endline "Directory table:";
  print_ht tuple_table;
  build_fs ();
  print_endline "";
  print_endline "";
  print_endline "File system:";
  print_fs_item fs.cur;
  print_endline "";
  print_endline "";
  print_endline "Directories with total size less than 100000:";
  printlist print_int sizes_less_than_100000_holder.cur;
  print_endline "";
  print_endline "";
  print_endline "With sum:";
  print_int (sum sizes_less_than_100000_holder.cur);
  print_endline "";
  print_endline "";
  print_endline "Directories with total size greater than 2080344:";
  printlist print_int sizes_more_than_2080344_holder.cur;
  print_endline "";
  print_endline "";
  print_endline "";
  print_endline "Smallest dir that would do:";
  print_int (List.fold_left (fun acc elem -> if acc > elem then elem else acc) max_int sizes_more_than_2080344_holder.cur);
  (* ignore (build_fs commands); *)
  print_endline "";;