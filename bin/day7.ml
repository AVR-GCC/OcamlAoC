let test_string = "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k"

type fs_item =
  | File of string * int
  | Dir of string * fs_item list
  | None

let print_fs_item_name itm = match itm with
  | Dir (name, _) | File (name, _) -> print_string name
  | _ -> print_string "-"

let print_fs_item itm = let rec print_fs_item_ind indentation item = print_string indentation; match item with
  | File (_, size) -> print_string "- "; print_fs_item_name item; print_string " (file, size="; print_int size; print_string ") "; print_string "\n"
  | Dir (_, lst) -> print_string "- "; print_fs_item_name item; print_string " (dir) "; print_string "\n"; List.fold_left (fun _ elem -> print_fs_item_ind (indentation ^ "  ") elem) () lst
  | None -> () in
  print_fs_item_ind "" itm

let root = Dir ("/", [])

type fs_holder = { mutable cur: fs_item; }
type callstack_holder = { mutable cur: fs_item list; }

let fs: fs_holder = { cur = root }

let callstack: callstack_holder = { cur = [] }

let print_fs title = print_endline title; print_fs_item fs.cur; print_endline ""

let add_item item dir = match dir with
  | Dir (name, lst) -> (match item with
    | Dir (child_name, child_lst) -> Dir (name, (Dir (child_name, child_lst))::lst)
    | File (child_name, child_size) -> Dir (name, (File (child_name, child_size))::lst)
    | None -> Dir (name, None::lst))
  | _ -> dir

let rec find_item name lst = match lst with
  | h::t -> (match h with
    | Dir (h_name, _) | File (h_name, _) -> if name = h_name then h else find_item name t
    | _ -> None)
  | _ -> None

let update_fs title fn = fs.cur <- fn fs.cur; print_fs (title); Day1.printlist print_fs_item_name callstack.cur; print_endline ""; print_endline ""

let push_callstack node = callstack.cur <- node::callstack.cur

let pop_callstack () = match callstack.cur with
  | [] -> callstack.cur <- []; None
  | h::t -> callstack.cur <- t; h

let cd path pwd = match pwd with
  | Dir (_, lst) -> if path = ".." then
    pop_callstack () else (push_callstack pwd; (find_item path lst))
  | _ -> None


let f_file = File ("f", 29116)
let g_file = File ("g", 2557)

let a_dir = Dir ("a", [])

let j_file = File ("j", 4060174)
let k_file = File ("k", 7214296)

let d_dir = Dir ("d", [])

let pack_a = add_item g_file (add_item f_file a_dir)

let pack_d = add_item j_file (add_item k_file d_dir)


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
  print_string "\n";
  print_string "\n";
  update_fs "Adding A" (add_item pack_a);
  update_fs "Adding D" (add_item pack_d);
  update_fs "CD A" (cd "a");
  update_fs "Go back" (cd "..");
  update_fs "CD D" (cd "d");
  update_fs  "Go back" (cd "..");
  print_string "\n";;