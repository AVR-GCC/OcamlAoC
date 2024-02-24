open Myutils

let lines = read_file "./inputs/day14real.txt"

let process_line line = line |> split_on_string " -> " |> List.map (String.split_on_char ',') |> List.map (fun x -> match x with | [a; b] -> (int_of_string a, int_of_string b) | _ -> failwith "bad input")

let rec find_borders xs ys = match xs, ys with
  | [x], [y] -> ((x, x), (y, y))
  | x::xs', y::ys' -> let ((minx, maxx), (miny, maxy)) = find_borders xs' ys' in ((min x minx, max x maxx), (min y miny, max y maxy))
  | _, _ -> failwith "empty list"

let draw_wall bottomx grid (xstart, ystart) (xend, yend) =
  (* print_newline (); print_string "("; print_int xstart; print_string ","; print_int ystart; print_string ") -> ("; print_int xend; print_string ","; print_int yend; print_endline ")"; *)
  if xstart = xend then
    if ystart < yend then
      for y = ystart to yend do grid.(y).(xstart - bottomx) <- '#' done else
      for y = yend to ystart do grid.(y).(xstart - bottomx) <- '#' done else
    if xstart < xend then
      for x = xstart to xend do grid.(ystart).(x - bottomx) <- '#' done else
      for x = xend to xstart do grid.(ystart).(x - bottomx) <- '#' done

let rec draw_walls bottomx grid walls = match walls with
  | [] -> ()
  | startt::endt::ws -> draw_wall bottomx grid startt endt;
      draw_walls bottomx grid (endt::ws)
  | (_, _)::[] -> ()

let drop_grain_one_step grid (x, y) =
  if y + 1 > Array.length grid - 1 then
    (-1, -1)
  else if grid.(y + 1).(x) = '.' then
    (x, y + 1)
  else if x = 0 then (-1, -1) else if grid.(y + 1).(x - 1) = '.' then
    (x - 1, y + 1)
  else if x = Array.length grid.(0) then (-1, -1) else if grid.(y + 1).(x + 1) = '.' then
    (x + 1, y + 1)
  else
    (x, y)

let rec drop_grain grid (x, y) =
  let (x', y') = drop_grain_one_step grid (x, y) in
  if y' = -1 then true else
  if x = x' && y = y' then (if grid.(y).(x) = '+' then true else (grid.(y).(x) <- 'o'; false))
  else drop_grain grid (x', y')

let print_grid grid = map_2d_arr grid (fun i j _ -> print_char grid.(i).(j))

let rec drop_all_grains spout grid grains = if (drop_grain grid spout) then grains else
  drop_all_grains spout grid (grains + 1)

let run () =  print_newline ();
  let tuples = List.map process_line lines in
  printlist (printlist (print_tuple print_int)) tuples;
  print_newline ();
  print_newline ();
  let (xs, ys) = List.split (List.flatten tuples) in
  let ((minx, maxx), (_, maxy)) = find_borders xs ys in
  let height = maxy + 1 in
  let width = maxx - minx + 1 in
  let grid = Array.make_matrix height width '.' in
  grid.(0).(500 - minx) <- '+';
  print_newline ();
  print_newline ();
  print_int minx; print_string " "; print_int maxx; print_string " "; print_int maxy;
  print_newline ();
  print_int (Array.length grid); print_string " "; print_int (Array.length grid.(0));
  print_newline ();
  print_newline ();
  ignore (List.map (fun tups -> draw_walls minx grid tups) tuples);
  print_newline ();
  print_grid grid;
  print_newline ();
  print_int (drop_all_grains (500 - minx, 0) grid 0);
  print_newline ();
  print_grid grid;
  let maxy = maxy + 2 in
  let minx = 500 - maxy in
  let maxx = 500 + maxy in
  let height = maxy + 1 in
  let width = maxx - minx + 1 in
  let grid = Array.make_matrix height width '.' in
  ignore (List.map (fun tups -> draw_walls minx grid tups) (([(minx, maxy); (maxx, maxy)])::tuples));
  grid.(0).(500 - minx) <- '+';
  print_newline ();
  print_grid grid;
  print_newline ();
  print_int (drop_all_grains (500 - minx, 0) grid 0);
  print_newline ();
  print_grid grid;
  print_newline ();;