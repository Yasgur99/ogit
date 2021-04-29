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

let run_diff_mode win (st : MyState.t) =
  MyRenderer.render_diff_mode st win;
  MyState.exec st Command.Diff

let run_normal win st render_fun parse_fun =
  render_fun st win;
  let key = Curses.wgetch win in
  let cmd = parse_fun key in
  let new_st = MyState.update_mode st cmd in
  MyState.exec new_st cmd

let rec run win (st : MyState.t) =
  match MyState.get_mode st with
  | MyState.CommitMode -> run win (run_commit_mode win st)
  | MyState.DiffMode _ ->
      run win
        (run_normal win st MyRenderer.render_diff_mode Command.parse_key)
  | MyState.CommitDone _ ->
      run win (run_normal win st MyRenderer.render Command.parse_key)
  | MyState.PushMode ->
      run win
        (run_normal win st MyRenderer.render_push_mode
           Command.parse_key_push_mode)
  | MyState.PullMode ->
      run win
        (run_normal win st MyRenderer.render_pull_mode
           Command.parse_key_pull_mode)
  | MyState.Normal ->
      run win (run_normal win st MyRenderer.render Command.parse_key)

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
