let rec run win st =
  let msg =
    match State.get_mode st with
    | State.CommitMode -> Renderer.render_commit_mode st win
    | State.CommitDone ->
        Renderer.render_commit_done st win;
        ""
    | State.CommitFailed ->
        Renderer.render_commit_failed st win;
        ""
    | State.Normal ->
        Renderer.render st win;
        ""
  in
  let key =
    if State.get_mode st <> CommitMode then Curses.wgetch win else 0
  in
  let cmd =
    if State.get_mode st <> CommitMode then Command.parse_key key
    else Command.Commit msg
  in
  let new_st = State.update_mode st cmd in
  run win (State.exec new_st cmd)

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
