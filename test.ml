open OUnit2
open Plumbing
(*open Porcelain*)

(** [plumbing_test n a ch cl] constructs an OUnit test named [n] that 
    asserts [ch] to check side effects and then calls [cl] which cleans
    up the side effects *)
let plumbing_test
    (name : string)
    (f : string array -> 'a)
    (args : string array)
    (check_side_effect : unit -> bool)
    (clean_side_effect : unit -> unit) : test =
  name >:: fun _ ->
    f args;
    try
      assert_bool "side effect did not occur" (check_side_effect ())
    with Failure f -> 
      clean_side_effect (); 
      raise (Failure f)

(** Tests for [Plumbing.init] *)
let init_tests = [
  (*plumbing_test "init no args" Plumbing.init [||] (fun () -> Sys.file_exists ".git") (fun () -> Sys.remove ".git"); *)
  plumbing_test "init tmp" Plumbing.init [|"tmp"|] (fun () -> Sys.file_exists "tmp/.git") (fun () -> Sys.remove "tmp")
]

(** Tests for [Plumbing.hash_object] *)
let hash_object_tests = [
]

(** Tests for [Plumbing.cat_file] *)
let cat_file_tests = [
]

(** Tests for [Plumbing.update_index] *)
let update_index_tests = [
]

(** Tests for [Plumbing.write_tree] *)
let write_tree_tests = [
]

(** Tests for [Plumbing.read_tree] *)
let read_tree_tests = [
]

(** Tests for [Plumbing.commit_tree] *)
let commit_tree_tests = [
]

(** Tests for [Plumbing.log] *)
let log_tests = [
]

(** Tests for [Plumbing.add] *)
let add_tests = [
]

(** Tests for [Plumbing.commit] *)
let commit_tests = [
]

(** Tests for [Plumbing.show] *)
let show_tests = [
]

(** Tests for [Plumbing.diff] *)
let diff_tests = [
]

(** Tests for [Plumbing.status] *)
let status_tests = [
]

(** Tests for [Plumbing ] module *)
let plumbing_tests =
  init_tests (*@ hash_object_tests @ cat_file_tests @ update_index_tests
  @ write_tree_tests @ read_tree_tests @ commit_tree_tests @ log_tests
  @ add_tests @ commit_tests @ show_tests @ diff_tests @ status_tests*)


(** Tests for [Porcelain] module *)
let porcelain_tests = []


let suite =
  "test suite for ogit"
  >::: List.flatten [ plumbing_tests (*; porcelain_tests*) ]

let _ = run_test_tt_main suite
