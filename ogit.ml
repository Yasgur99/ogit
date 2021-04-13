let run win initial_st =
  let rec aux st win =
    Renderer.render st win;
    let key = Curses.wgetch win in
    let cmd = Command.parse_key key in
    aux (State.exec st cmd) win 
  in 
  aux initial_st win

let run_git args =
  List.iter print_endline (Plumbing.get_out (Plumbing.git args))

let () =
  if Array.length Sys.argv > 1
  then
    run_git (Array.sub Sys.argv 1 (Array.length Sys.argv - 1))
  else
  try
    let win = Renderer.init () in
    let initial_st = State.init_state "." in
    run win initial_st 
  with Command.Program_terminate ->
    Renderer.cleanup ();
    exit 0
