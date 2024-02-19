let printlist printelem lst = let rec middle = function
| [] -> ()
| h::t -> printelem h; if t = [] then () else print_string "; "; middle t in
print_string "["; middle lst; print_string "]"
