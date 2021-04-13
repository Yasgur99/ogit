let rec run win st =
  Renderer.render st win;
  let key = Curses.wgetch win in
  let cmd = Command.parse_key key in
  run win (State.exec st cmd) 

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
