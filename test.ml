open OUnit2
open Plumbing
open Porcelain
open Renderer
open State

(*****************************************************)
(* Mock Plumbing *)
(*****************************************************)
module TestPorcelain = PorcelainImpl (MockPlumbing)
module TestState = StateImpl (MockPlumbing)

(*****************************************************)
(* Porcelain Tests *)
(*****************************************************)

(** [porcelain_test n f] constructs an OUnit test named [n] that
    asserts no exception is thrown when [f] executed *) 
let porcelain_test
  (name : string)
  (f) : test =
name >:: fun _ ->
  f ();
  assert_bool "no exception thrown" true 

let porcelain_tests = [
  porcelain_test "pull none" (fun () -> TestPorcelain.pull None);
  porcelain_test "pull some" (fun () -> TestPorcelain.pull (Some "master"));
  porcelain_test "push none" (fun () -> TestPorcelain.push None);
  porcelain_test "push some" (fun () -> TestPorcelain.push (Some "master"));
  porcelain_test "log none" (fun () -> TestPorcelain.log None);
  porcelain_test "get_head" (fun () -> TestPorcelain.get_head);
  porcelain_test "get_upstream" (fun () -> TestPorcelain.get_upstream);
  porcelain_test "get_push" (fun () -> TestPorcelain.get_push);
  porcelain_test "commit" (fun () -> TestPorcelain.commit "msg");
  porcelain_test "diff" (fun () -> TestPorcelain.diff);
  porcelain_test "status" (fun () -> TestPorcelain.status);
  porcelain_test "checkout" (fun () -> TestPorcelain.checkout "ha");
  porcelain_test "create branch" (fun () -> TestPorcelain.create_branch"hah");
  porcelain_test "delete branch" (fun () -> TestPorcelain.delete_branch"hah");
]

(*****************************************************)
(* State Tests *)
(*****************************************************)

let init_state_test 
  (name : string)
  (setup)
  (check)
  : test =
name >:: fun _ ->
setup ();
let a = TestState.init_state "dummy" in
assert_bool "commit history empty" (check a)

let empty_commit_history () = 
  MockPlumbing.set_log_data ["fatal"] [] ["fatal"]

let some_commit_history () = 
  MockPlumbing.set_log_data 
    ["59689ce (setup project files, 2021-03-22)";
     "b92c19e (Initial commit, 2021-03-04)"] 
    [] 
    ["59689ce (setup project files, 2021-03-22)";
     "b92c19e (Initial commit, 2021-03-04)"]

let is_commit_history_empty st =
  match TestState.commit_history st with
  | [] -> true
  | h :: t -> false

let is_commit_history_not_empty st =
  match TestState.commit_history st with
  | [] -> false 
  | h :: t -> true  

let no_tracked_data () =
  MockPlumbing.set_status_data [] [] []

let some_tracked_data () =
  MockPlumbing.set_status_data [" M test.txt"] [] [" M test.txt"]

let is_no_tracked st =
  match TestState.tracked st with
  | [] -> true
  | h :: t -> false

let is_tracked st =
  match TestState.tracked st with
  | [] -> false 
  | h :: t -> true 

let no_staged_data () =
  MockPlumbing.set_status_data [] [] []

let some_staged_data () =
  MockPlumbing.set_status_data ["M  test.txt"] [] ["M  test.txt"]

let some_untracked_data () =
  MockPlumbing.set_status_data ["?? test.txt"] [] ["?? test.txt"]

let is_no_staged st =
  match TestState.staged st with
  | [] -> true
  | h :: t -> false

let is_staged st =
  match TestState.staged st with
  | [] -> false 
  | h :: t -> true 

let is_normal_mode st =
  match TestState.get_mode st with
  | Normal -> true
  | _ -> false

let set_head () =
  MockPlumbing.set_head_data ["refs/heads/master"] [] ["refs/heads/master"]

let head_exists st =
  match TestState.head st with
  | "" -> false
  | _ -> true

let init_state_tests = [
  init_state_test "no commit history" empty_commit_history 
    is_commit_history_empty;
  init_state_test "some commit history" some_commit_history 
    is_commit_history_not_empty;
  init_state_test "no tracked" no_tracked_data is_no_tracked;
  init_state_test "some tracked" some_tracked_data is_tracked;
  init_state_test "no staged" no_staged_data is_no_staged;
  init_state_test "some staged" some_staged_data is_staged;
  init_state_test "is in normal mode" (fun () -> ()) is_normal_mode;
  init_state_test "head is populated" (set_head) head_exists;
]

let is_curs c st =
  TestState.get_curs st = c

let exec_test
  (name : string) 
  (cmd)
  (setup)
  (effect)
  (check) : test =
name >:: fun _ ->
  setup();
  let st = TestState.init_state "as" in
  let st' = TestState.exec st cmd in
  effect ();
  assert_bool "exec check" (check st') 

let exec_tests = [
  exec_test "stage staged file" Command.Stage some_staged_data 
    some_staged_data  is_staged;

  exec_test "stage untracked file" Command.Stage some_untracked_data 
    some_staged_data is_staged;

  exec_test "stage tracked file" Command.Stage some_tracked_data
    some_staged_data is_staged;

  exec_test "unstage file" Command.Unstage some_staged_data
    some_staged_data is_no_staged;

  exec_test "navup at top of file" (Command.NavUp true) (fun () -> ()) 
    (fun () -> ()) (is_curs 0);

  exec_test "navdown top of file" (Command.NavUp true) (fun () -> ())
    (fun () -> ()) (is_curs 1);
]

let printable_of_state_tests = []

let set_mode_test
  (name : string) 
  (mode) : test =
name >:: fun _ ->
  let st = TestState.init_state "as" in
  let st' = TestState.set_mode st mode in
  assert_bool "set mode" (TestState.get_mode st' = mode) 

let set_mode_tests = [
  set_mode_test "normal" TestState.Normal;
  set_mode_test "commit mode" TestState.CommitMode;
  set_mode_test "commit done" (TestState.CommandDone "");
  set_mode_test "diff" (TestState.DiffMode "");
  set_mode_test "push" TestState.PushMode;
  set_mode_test "push elsewhere" TestState.PushElsewhereMode;
  set_mode_test "push elsewhere done" (TestState.PushElsewhereDone "");
  set_mode_test "pull" TestState.PullMode;
  set_mode_test "branch" TestState.BranchMode;
  set_mode_test "checkout branch" TestState.CheckoutGetBranchNameMode;
  set_mode_test "create branch" TestState.CreateGetBranchNameMode;
  set_mode_test "delete branch" TestState.DeleteGetBranchNameMode;
  set_mode_test "pull elsewhere" TestState.PullElsewhereMode;
  set_mode_test "pull elsewhere done" (TestState.PullElsewhereDone "")
]

let state_tests = 
  init_state_tests
  @ exec_tests
  @ set_mode_tests

(*****************************************************)
(* Command Tests *)
(*****************************************************)

(** [parse_key_test k exp] constructs an OUnit test named [n] that
    asserts [Command.parse_key k] is exp*)
let parse_key_test
  (name : string)
  (key : int)
  (exp: string) : test =
name >:: fun _ ->
assert_equal exp (Command.string_of_cmd (Command.parse_key key))

(** [parse_key_diff_mode_test k exp] constructs an OUnit test named [n]
    that asserts [Command.parse_key_diff_mode k] is exp*)
let parse_key_diff_mode_test
  (name : string)
  (key : int)
  (exp: string) : test =
name >:: fun _ ->
assert_equal exp (Command.string_of_cmd (Command.parse_key_diff_mode key))

(** [parse_key_diff_mode_test k exp] constructs an OUnit test named [n]
    that asserts [Command.parse_key_pull_mode k] is exp*)
let parse_key_pull_mode_test
  (name : string)
  (key : int)
  (exp: string) : test =
name >:: fun _ ->
assert_equal exp (Command.string_of_cmd (Command.parse_key_pull_mode key))

(** [parse_key_push_mode_test k exp] constructs an OUnit test named [n]
    that asserts [Command.parse_key_push_mode k] is exp*)
let parse_key_push_mode_test
  (name : string)
  (key : int)
  (exp: string) : test =
name >:: fun _ ->
assert_equal exp (Command.string_of_cmd (Command.parse_key_push_mode key))

(** [parse_key_branch_mode_test k exp] constructs an OUnit test named [n] 
    that asserts [Command.parse_key_branch_mode k] is exp*)
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
  parse_key_branch_mode_test "b is branch" (int_of_char 'b')
    "checkout branch prompt";
  parse_key_branch_mode_test "c is branch" (int_of_char 'c')
    "create branch prompt";
  parse_key_branch_mode_test "x is branch" (int_of_char 'x')
    "delete branch prompt"
]

(** Tests for [Command] module *)
let command_tests =
   parse_key_tests
   @ parse_key_diff_mode_tests
   @ parse_key_pull_mode_tests
   @ parse_key_push_mode_tests
   @ parse_key_branch_mode_tests

(*****************************************************)
(* Test Suite *)
(*****************************************************)

let suite =
  "test suite for ogit"
  >::: List.flatten [
    command_tests; 
    state_tests; 
    porcelain_tests
  ]

let _ = run_test_tt_main suite
