let run_commit_mode win st =
  let msg = Renderer.render_commit_mode st win in
  let cmd = Command.Commit msg in
  State.exec st cmd

let run_diff_mode win st =
  Renderer.render_diff_mode st win;
  State.exec st Command.Diff

let run_normal win st render_fun =
  render_fun st win;
  let key = Curses.wgetch win in
  let cmd = Command.parse_key key in
  let new_st = State.update_mode st cmd in
  State.exec new_st cmd

let rec run win st =
  match State.get_mode st with
  | State.CommitMode -> run win (run_commit_mode win st)
  | State.DiffMode _ ->
      run win (run_normal win st Renderer.render_diff_mode)
  | State.CommitDone _ -> run win (run_normal win st Renderer.render)
  | State.PushMode ->
      run win (run_normal win st Renderer.render_push_mode)
  | State.PullMode ->
      run win (run_normal win st Renderer.render_pull_mode)
  | State.Normal -> run win (run_normal win st Renderer.render)

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
