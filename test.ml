open OUnit2
open Plumbing
open Porcelain

(** [init_test n a ch cl] constructs an OUnit test named [n] that 
    asserts [ch] to check side effects and then calls [cl] which cleans
    up the side effects *)
let init_test
    (name : string)
    (args : string list)
    (check_side_effect : fun unit -> bool)
    (clean_side_effect : fun unit -> unit) : test =
  name >:: fun _ ->
    Plumbing.init args;
    assert_true check_side_effect;
    clean_side_effect;

(** Tests for [Plumbing.init] *)
let init_tests = [
  init_test "init no args" []
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


(** Tests for [Plumbing ] module *)
let plumbing_tests = [
  init_tests (*@ hash_object_tests @ cat_file_tests @ update_index_tests
  @ write_tree_tests @ read_tree_tests @ commit_tree_tests @ log_tests
  @ add_tests @ commit_tests @ show_tests @ diff_tests *)
]

(** Tests for [Porcelain] module *)
let porcelain_tests = [

]

let suite =
  "test suite for ogit"
  >::: List.flatten [ plumbing_tests (*; porcelain_tests*) ]

let _ = run_test_tt_main suite
