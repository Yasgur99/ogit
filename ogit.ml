open Plumbing
open State
open Renderer

module Plumbing = ProdPlumbing
module State = StateImpl (Plumbing)
module Renderer = Renderer (State)


let run_commit_mode win st =
  let msg = Renderer.render_commit_mode st win in
  let cmd = Command.Commit msg in
  State.exec st cmd

let rec run win st =
  match State.get_mode st with
  | State.CommitMode -> run win (run_commit_mode win st)
  | _ ->
      Renderer.render st win;
      let key = Curses.wgetch win in
      let cmd = Command.parse_key key in
      let new_st = State.update_mode st cmd in
      run win (State.exec new_st cmd)

let run_git args =
  List.iter print_endline (Plumbing.get_out (Plumbing.git args))

let () =
  if Array.length Sys.argv > 1 then
    run_git (Array.sub Sys.argv 1 (Array.length Sys.argv - 1))
  else
    try
      let initial_st = State.init_state "." in
      let win = Renderer.init () in
      run win initial_st
    with Command.Program_terminate ->
      Renderer.cleanup ();
      exit 0
