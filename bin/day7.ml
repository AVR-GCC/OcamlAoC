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
  | File of string * int * fs_item
  | Dir of string * fs_item list * fs_item
  | None

type fs_holder = { mutable cur: fs_item; }

let print_fs_item_name itm = match itm with
  | Dir (name, _, _) | File (name, _, _) -> print_string name
  | _ -> print_string "-"

let print_fs_item itm = let rec print_fs_item_ind indentation item = print_string indentation; match item with
  | File (_, size, parent) -> print_string "- "; print_fs_item_name item; print_string " (file, size="; print_int size; print_string ")"; print_fs_item_name parent; print_string "\n"
  | Dir (_, lst, parent) -> print_string "- "; print_fs_item_name item; print_string " (dir)"; print_fs_item_name parent; print_string "\n"; List.fold_left (fun _ elem -> print_fs_item_ind (indentation ^ "  ") elem) () lst
  | None -> () in
  print_fs_item_ind "" itm

let root = Dir ("/", [], None)

let fs: fs_holder = { cur = root }

let print_fs () = print_fs_item fs.cur; print_string "\n"

let add_item item dir = match dir with
  | Dir (name, lst, parent) -> (match item with
    | Dir (child_name, child_lst, _) -> Dir (name, (Dir (child_name, child_lst, dir))::lst, parent)
    | File (child_name, child_size, _) -> Dir (name, (File (child_name, child_size, dir))::lst, parent)
    | None -> Dir (name, None::lst, parent))
  | _ -> dir

let rec find_item name lst = match lst with
  | h::t -> (match h with
    | Dir (h_name, _, _) | File (h_name, _, _) -> if name = h_name then h else find_item name t
    | _ -> None)
  | _ -> None

let cd path pwd = match pwd with
  | Dir (_, lst, parent) -> print_string "cd - \n"; if path = ".." then
    (print_fs_item parent; parent) else (print_fs_item (find_item path lst); (find_item path lst))
  | _ -> None

let update_fs fn = fs.cur <- fn fs.cur; print_fs ()

let f_file = File ("f", 29116, None)
let g_file = File ("g", 2557, None)

let a_dir = Dir ("a", [], None)

let j_file = File ("j", 4060174, None)
let k_file = File ("k", 7214296, None)

let d_dir = Dir ("d", [], None)

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
  update_fs (add_item pack_a);
  update_fs (add_item pack_d);
  update_fs (cd "a");
  update_fs (cd "..");
  update_fs (cd "d");
  update_fs (cd "..");
  print_string "\n";;