let rec print_lines lines =
  match lines with
  | [] -> ();
  | h::t -> print_endline h; print_lines t

let () = 
  print_lines (Plumbing.get_out (Plumbing.git (Array.sub Sys.argv 1 (Array.length Sys.argv - 1))))
