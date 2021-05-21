open OUnit2
open Plumbing
open Porcelain
open Renderer
open State

(*
(** Some Helper Methods *)

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

(** [check_err_raises_test n err] constructs an OUnit test named [n] that asserts [Command.check_err] raises [Command.Program_terminate] *)
(*let check_err_raises_test
  (name : string)
  (err : bool) : test =
name >:: fun _ ->
assert_raises (Command.Program_terminate) (fun () -> Renderer.check_err err)
  *)

(** [exec_test cmd] constructs an OUnit test named [n] that asserts 
    [Command.exec cmd] *)
let exec_test
  (name : string)
  (cmd : Command.t)
  (exp: string) : test =
name >:: fun _ ->
assert_equal exp ""


(** [check_err_test err] constructs an OUnit test named [n] that asserts [Command.check_err] is unit *)
(*let check_err_test
  (name : string)
  (err : bool) : test =
name >:: fun _ ->
assert_equal (Renderer.check_err err) ()
*)

(** 

Porcelain Helper Functions

let add_to_status_t_test
    (name : string)
    (status : status_t)
    (lines : string)
    (exp_output : status_t) : test =
  name >:: fun _ ->
  assert_equal exp_output (Porcelain.add_to_status_t status lines)

let init_test (name : string) (dir : string option) (exp_output : unit)
    : test =
  name >:: fun _ -> assert_equal exp_output (Porcelain.init dir)

let hash_object_test
    (name : string)
    (file : string)
    (exp_output : string) : test =
  name >:: fun _ -> assert_equal exp_output Porcelain.hash_object

let cat_file_test
    (name : string)
    (hash : object_id)
    (exp_output : object_content) : test =
  name >:: fun _ -> assert_equal exp_output (Porcelain.cat_file hash)

let cat_file_type_test
    (name : string)
    (hash : object_id)
    (exp_output : object_type) : test =
  name >:: fun _ ->
  assert_equal exp_output (Porcelain.cat_file_type hash)

let update_index_test
    (name : string)
    (hash : object_id)
    (filename : string)
    (exp_output : unit) : test =
  name >:: fun _ ->
  assert_equal exp_output (Porcelain.update_index hash filename)

let write_tree_test
    (name : string)
    (nothing : unit)
    (exp_output : object_id) : test =
  name >:: fun _ ->
  assert_equal exp_output (Porcelain.write_tree nothing)

let read_tree_test
    (name : string)
    (hash : object_id)
    (exp_output : unit) : test =
  name >:: fun _ -> assert_equal exp_output (Porcelain.read_tree hash)

let read_tree_prefix_test
    (name : string)
    (ob_id : object_id)
    (prefix : string)
    (exp_output : unit) : test =
  name >:: fun _ ->
  assert_equal exp_output (Porcelain.read_tree_prefix ob_id prefix)

let commit_tree_test
    (name : string)
    (hash : object_id)
    (msg : string)
    (exp_output : object_id) : test =
  name >:: fun _ ->
  assert_equal exp_output (Porcelain.commit_tree hash msg)

let log_test
    (name : string)
    (ob_id : object_id option)
    (exp_output : commit_object list) : test =
  name >:: fun _ -> assert_equal exp_output (Porcelain.log ob_id)

let add_test
    (name : string)
    (filenames : string list)
    (exp_output : unit) : test =
  name >:: fun _ -> assert_equal exp_output (Porcelain.add filenames)

let commit_test (name : string) (msg : string) (exp_output : unit) :
    test =
  name >:: fun _ -> assert_equal exp_output (Porcelain.commit msg)

let show_test (name : string) (nothing : unit) (exp_output : unit) :
    test =
  name >:: fun _ -> assert_equal exp_output (Porcelain.show nothing)

let diff_test (name : string) (nothing : unit) (exp_output : unit) :
    test =
  name >:: fun _ -> assert_equal exp_output (Porcelain.diff nothing)

*)

(** Tests for [Plumbing ] module *)
let plumbing_tests = init_tests
(*@ hash_object_tests @ cat_file_tests @ update_index_tests @
  write_tree_tests @ read_tree_tests @ commit_tree_tests*)
(*@ log_tests @ add_tests @ commit_tests @ show_tests @ diff_tests @
  status_tests *)

let get_untracked_test
    (name : string)
    (setup_func : unit -> unit)
    (exp_output : string list)
    (clean_up : unit -> unit) : test =
  name >:: fun _ ->
    try
      setup_func ();
      let status = Porcelain.status () in
      let ans = (assert_equal exp_output (Porcelain.get_untracked status) 
      ~printer:(pp_list pp_string) )in
      clean_up ();
      ans
    with Failure f ->
      clean_up ();
      raise (Failure f)


      
let get_tracked_test
    (name : string)
    (setup_func : unit -> unit)
    (exp_output : string list)
    (clean_up : unit -> unit) : test =
  name >:: fun _ ->
    setup_func ();
    try
      let status = Porcelain.status() in 
      let ans = (assert_equal exp_output (Porcelain.get_tracked status) 
      ~printer:(pp_list pp_string) )in
      clean_up ();
      ans
    with Failure f ->
      clean_up ();
      raise (Failure f)



let get_staged_test
    (name : string)
    (setup_func : unit -> unit)
    (exp_output : string list)
    (clean_up : unit -> unit) : test =
  name >:: fun _ ->
    setup_func ();
    try
      let status = Porcelain.status() in 
      let ans = (assert_equal exp_output (Porcelain.get_staged status) 
      ~printer:(pp_list pp_string) )in
      clean_up ();
      ans
    with Failure f ->
      clean_up ();
      raise (Failure f)

  (* let get_tracked_and_staged_test
    (name : string)
    (func : string -> unit)
    (clean_up : string -> unit)
    (stat : status_t)
    (exp_output : string list) : test =
  name >:: fun _ -> assert_equal exp_output (Porcelain.get_staged stat) 
  (* append get_tracked *) *)


let create_file filename =
   let op = open_out (filename) in
   close_out op


let setup_untracked_test filename =
  (*init_repo "testing";*)
  create_file filename
  (*Sys.chdir "testing"*)
  

let setup_tracked_test filename =
   (*init_repo "testing";*)
   create_file filename;
   ignore (Plumbing.add [| filename |]);
   ignore (Plumbing.commit [|"-m"; "adding file for tracked test"|]);
   let op = open_out filename in
     Printf.fprintf op "%s\n" "Modify";
     close_out op


let setup_staged_test filename = 
   (*init_repo "testing"; *)
   create_file filename;
   ignore (Plumbing.add [| filename |])

let setup_tracked_and_staged_test filename =
   (*init_repo "testing";*)
   create_file filename;
   ignore (Plumbing.add [| filename |]);
   let op = open_out filename in
     Printf.fprintf op "%s\n" "Modify";
     close_out op

let status_tests = [
    get_untracked_test "One untracked file" (fun () -> setup_untracked_test 
      "untracked.txt") ["untracked.txt"] 
      (fun () -> rmr "untracked.txt");

    get_tracked_test "One tracked file" (fun () -> setup_tracked_test "tracked.txt") 
      ["tracked.txt"] (fun () -> rmr "tracked.txt");

    get_staged_test "One staged file" (fun () -> setup_staged_test "staged.txt")
      ["staged.txt"] (fun () -> rmr "staged.txt")
  ]

(** Tests for [Porcelain] module *)
let porcelain_tests = status_tests
*)
(*****************************************************)
(* State Tests *)
(*****************************************************)

let init_state_test 
  (name : string) : test =
name >:: fun _ ->
assert_equal true false

let exec_test
  (name : string) : test =
name >:: fun _ ->
assert_equal true false

let init_state_tests = [
  init_state_test "not a git dir";
  init_state_test "no commit history";
  init_state_test "some commit history";
  init_state_test "no tracked";
  init_state_test "some tracked";
  init_state_test "no staged";
  init_state_test "some staged";
  init_state_test "is in normal mode";
]

let commit_history_tests = []
let exec_tests = [
  exec_test "stage staged file";
  exec_test "stage untracked file";
  exec_test "stage tracked file";
  exec_test "unstage staged file back to tracked";
  exec_test "unstage file back to untracked";
  exec_test "stage while not on file";
  exec_test "navup at top of file";
  exec_test "navup in middle of file";
  exec_test "navdown at bottom of file";
  exec_test "navdown in middle of file";
  exec_test "quit"
]

let printable_of_state_tests = []
let get_curs_tests = []
let set_curs_tests = []

let state_tests = 
  init_state_tests
  @ commit_history_tests
  @ exec_tests
  @ printable_of_state_tests
  @ get_curs_tests
  @ set_curs_tests

(*****************************************************)
(* Command Tests *)
(*****************************************************)

(** [parse_key_test k exp] constructs an OUnit test named [n] that asserts [Command.parse_key k] is exp*)
let parse_key_test
  (name : string)
  (key : int)
  (exp: string) : test =
name >:: fun _ ->
assert_equal exp (Command.string_of_cmd (Command.parse_key key))

(** [parse_key_diff_mode_test k exp] constructs an OUnit test named [n] that asserts [Command.parse_key_diff_mode k] is exp*)
let parse_key_diff_mode_test
  (name : string)
  (key : int)
  (exp: string) : test =
name >:: fun _ ->
assert_equal exp (Command.string_of_cmd (Command.parse_key_diff_mode key))

(** [parse_key_diff_mode_test k exp] constructs an OUnit test named [n] that asserts [Command.parse_key_pull_mode k] is exp*)
let parse_key_pull_mode_test
  (name : string)
  (key : int)
  (exp: string) : test =
name >:: fun _ ->
assert_equal exp (Command.string_of_cmd (Command.parse_key_pull_mode key))

(** [parse_key_push_mode_test k exp] constructs an OUnit test named [n] that asserts [Command.parse_key_push_mode k] is exp*)
let parse_key_push_mode_test
  (name : string)
  (key : int)
  (exp: string) : test =
name >:: fun _ ->
assert_equal exp (Command.string_of_cmd (Command.parse_key_push_mode key))

(** [parse_key_branch_mode_test k exp] constructs an OUnit test named [n] that asserts [Command.parse_key_branch_mode k] is exp*)
let parse_key_branch_mode_test
  (name : string)
  (key : int)
  (exp: string) : test =
name >:: fun _ ->
assert_equal exp (Command.string_of_cmd (Command.parse_key_branch_mode key))

(** Tests for [Command.parse_key] *)
let parse_key_tests = [
  parse_key_test "s is stage" (int_of_char 's') "stage";
  parse_key_test "u is unstage" (int_of_char 'u') "unstage";
  parse_key_test "k is NavUp" (int_of_char 'k') "navup";
  parse_key_test "j is NavDown" (int_of_char 'j') "navdown";
  parse_key_test "Up is NavUp" (Curses.Key.up) "navup";
  parse_key_test "Down is NavDown" (Curses.Key.down) "navdown";
  parse_key_test "q is quit" (int_of_char 'q') "quit";
  parse_key_test "unsupported is nop" (int_of_char '[') "nop"
]

let parse_key_diff_mode_tests = [
  parse_key_diff_mode_test "s is diff" (int_of_char 's') "diff";
  parse_key_diff_mode_test "t is diff" (int_of_char 't') "diff";
  parse_key_diff_mode_test "a is diff" (int_of_char 'a') "diff";
  parse_key_diff_mode_test "f is diff" (int_of_char 'f') "diff"
]

let parse_key_pull_mode_tests = [
  parse_key_pull_mode_test "p is pull" (int_of_char 'p') "pull";
  parse_key_pull_mode_test "u is pull" (int_of_char 'u') "pull";
  parse_key_pull_mode_test "e is pull" (int_of_char 'e') "pullelsewhere"
]

let parse_key_push_mode_tests = [
  parse_key_push_mode_test "p is push" (int_of_char 'p') "push";
  parse_key_push_mode_test "u is push" (int_of_char 'u') "push";
  parse_key_push_mode_test "e is push" (int_of_char 'e') "push"
]

let parse_key_branch_mode_tests = [
  parse_key_branch_mode_test "b is branch" (int_of_char 'b') "checkout branch prompt";
  parse_key_branch_mode_test "c is branch" (int_of_char 'c') "create branch prompt";
  parse_key_branch_mode_test "x is branch" (int_of_char 'x') "delete branch prompt"
]

(** Tests for [Command] module *)
let command_tests =
   parse_key_tests
   @ parse_key_diff_mode_tests
   @ parse_key_pull_mode_tests
   @ parse_key_push_mode_tests
   @ parse_key_branch_mode_tests

(*****************************************************)
(* Mock Plumbing *)
(*****************************************************)
(*module TestPorcelain = PorcelainImpl (MockPlumbing)
module TestState = StateImpl (MockPlumbing)*)
(*****************************************************)
(* Test Suite *)
(*****************************************************)

let suite =
  "test suite for ogit"
  >::: List.flatten [ command_tests; state_tests(*plumbing_tests; state_tests; porcelain_tests *)]

let _ = run_test_tt_main suite
