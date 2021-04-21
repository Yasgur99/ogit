let rec run win st =
  let msg =
    match State.get_mode st with
    | State.Normal ->
        Renderer.render st win;
        ""
    | State.CommitMode -> Renderer.render_commit_mode st win
  in
  let key = Curses.wgetch win in
  let cmd = Command.parse_key key in
  let full_cmd =
    match cmd with Command.Commit _ -> Command.Commit msg | _ -> cmd
  in
  let new_st = State.update_mode st cmd in
  run win (State.exec new_st full_cmd)

let run_git args =
  List.iter print_endline (Plumbing.get_out (Plumbing.git args))

let () =
  if Array.length Sys.argv > 1 then
    run_git (Array.sub Sys.argv 1 (Array.length Sys.argv - 1))
  else
    try
      let win = Renderer.init () in
      let initial_st = State.init_state "." in
      run win initial_st
    with Command.Program_terminate ->
      Renderer.cleanup ();
      exit 0
