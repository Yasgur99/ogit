open Plumbing
open State
open Renderer
module MPlumbing = ProdPlumbing
module MyState = StateImpl (MPlumbing)
module MyRenderer = RendererImpl (MyState)
open MPlumbing
open MyState
open MyRenderer

let run_commit_mode win (st : MyState.t) =
  let msg = MyRenderer.render_commit_mode st win in
  let cmd = Command.Commit msg in
  MyState.exec st cmd

let run_normal win st parse_fun =
  MyRenderer.render st win;
  let max_y = fst (Curses.getmaxyx win) in
  let key = Curses.wgetch win in
  let cmd = parse_fun key in
  let full_cmd =
    match cmd with
    | Command.NavUp _ ->
        if
          MyState.get_curs st >= max_y - 1
          && not (!MyRenderer.top_line <= 1)
        then Command.NavUp false
        else Command.NavUp true
    | Command.NavDown _ ->
        if MyState.get_curs st >= max_y - 1 then Command.NavDown false
        else Command.NavDown true
    | _ -> cmd
  in
  let new_st = MyState.update_mode st full_cmd in
  MyState.exec new_st full_cmd

let run_checkout_get_branch_mode win st =
  let branch = MyRenderer.render_checkout_get_branch_mode st win in
  let cmd = Command.CheckoutBranch branch in
  MyState.exec st cmd

let run_create_get_branch_mode win st =
  let branch = MyRenderer.render_create_get_branch_mode st win in
  let cmd = Command.CreateBranch branch in
  MyState.exec st cmd

let run_delete_get_branch_mode win st =
  let branch = MyRenderer.render_delete_get_branch_mode st win in
  let cmd = Command.DeleteBranch branch in
  MyState.exec st cmd

let rec run win (st : MyState.t) =
  match MyState.get_mode st with
  | MyState.CommitMode -> run win (run_commit_mode win st)
  | MyState.DiffMode _ ->
      run win (run_normal win st Command.parse_key_diff_mode)
  | MyState.CommitDone _ ->
      run win (run_normal win st Command.parse_key)
  | MyState.PushMode ->
      run win (run_normal win st Command.parse_key_push_mode)
  | MyState.PullMode ->
      run win (run_normal win st Command.parse_key_pull_mode)
  | MyState.Normal -> run win (run_normal win st Command.parse_key)
  | MyState.BranchMode ->
      run win (run_normal win st Command.parse_key_branch_mode)
  | MyState.CheckoutGetBranchNameMode ->
      run win (run_checkout_get_branch_mode win st)
  | MyState.CreateGetBranchNameMode ->
      run win (run_checkout_get_branch_mode win st)
  | MyState.DeleteGetBranchNameMode ->
      run win (run_checkout_get_branch_mode win st)

let run_git args =
  List.iter print_endline (MPlumbing.get_out (MPlumbing.git args))

let () =
  if Array.length Sys.argv > 1 then
    run_git (Array.sub Sys.argv 1 (Array.length Sys.argv - 1))
  else
    try
      let win = MyRenderer.init () in
      let initial_st = MyState.init_state "." in
      run win initial_st
    with Command.Program_terminate ->
      MyRenderer.cleanup ();
      exit 0
