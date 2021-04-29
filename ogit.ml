open Plumbing
open State
open Renderer

module MPlumbing = ProdPlumbing
module MyState = StateImpl (MPlumbing)
module MRenderer = RendererImpl (MyState)
open MPlumbing
open MyState
open MRenderer

let run_commit_mode win (st : MyState.t) =
  let msg = MRenderer.render_commit_mode st win in
  let cmd = Command.Commit msg in
  MyState.exec st cmd

let rec run win (st : MyState.t) =
  match MyState.get_mode st with
  | MyState.CommitMode -> run win (run_commit_mode win st)
  | _ ->
      MRenderer.render st win;
      let key = Curses.wgetch win in
      let cmd = Command.parse_key key in
      let new_st = MyState.update_mode st cmd in
      run win (MyState.exec new_st cmd)

let run_git args =
  List.iter print_endline (MPlumbing.get_out (MPlumbing.git args))

let () =
  if Array.length Sys.argv > 1 then
    run_git (Array.sub Sys.argv 1 (Array.length Sys.argv - 1))
  else
    try
      let initial_st = MyState.init_state "." in
      let win = MRenderer.init () in
      run win initial_st
    with Command.Program_terminate ->
      MRenderer.cleanup ();
      exit 0
