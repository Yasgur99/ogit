(** Types and methods to access and construct the type *)
type result = {
  stdout : string list;
  stderr : string list;
  out_and_err : string list; (*exit_code : int;*)
}

let make_result out err out_and_err =
  { stdout = out; stderr = err; out_and_err }

let get_stdout result = result.stdout

let get_stderr result = result.stderr

let get_out result = result.out_and_err

(** Helper Methods *)

(** [read fd] is the lines of file referenced by descriptor [fd] *)
let read (fd : Unix.file_descr) : string list =
  let in_ch = Unix.in_channel_of_descr fd in
  let lines = ref [] in
  try
    while true do
      lines := input_line in_ch :: !lines
    done;
    !lines
  with End_of_file ->
    close_in in_ch;
    !lines

(** [fork_and_execvp e a] is the result of executing program [exe] with
    arguments [args]*)
let fork_and_execv (exe : string) (args : string array) : result =
  let inp_stdout, out_stdout = Unix.pipe () in
  (* Pipe for stdout *)
  let inp_stderr, out_stderr = Unix.pipe () in
  (* Pipe for stderr *)
  let inp, out = Unix.pipe () in
  (* Pipe for both stdout and stderr *)
  let pid = Unix.fork () in
  if pid = 0 then (
    Unix.close inp_stdout;
    (* Not used by child *)
    Unix.close inp_stderr;
    (* Not used by child *)
    Unix.close inp;
    (* Not used by child *)
    Unix.dup2 out_stdout Unix.stdout;
    (* Bind stdout pipe to stdout *)
    Unix.dup2 out_stderr Unix.stderr;
    (* Bind stderr pipe to stderr *)
    Unix.dup2 out Unix.stdout;
    (* Bind out pipe to stdout *)
    Unix.dup2 out Unix.stderr;
    (* Bind out pipe to stderr *)
    Unix.execvp exe args)
  else (
    Unix.close out_stdout;
    (* Not used by parent*)
    Unix.close out_stderr;
    (* Not used by parent*)
    Unix.close out;
    (* Not used by parent*)
    let stdout = read inp_stdout in
    let stdin = read inp_stderr in
    let out_and_err = read inp in

    (* Does not close the pipes because [read fd] does that when it
       closes the input channel it creates.

       Unix.close inp_stderr; Unix.close inp_stdout; Unix.close inp; *)
    make_result stdout stdin out_and_err)

let init (args : string array) =
  fork_and_execv "git" (Array.append [| "git"; "init" |] args)

let pull (args : string array) =
  fork_and_execv "git" (Array.append [| "git"; "pull" |] args)

let push (args : string array) =
  fork_and_execv "git" (Array.append [| "git"; "pull" |] args)

let hash_object (args : string array) =
  fork_and_execv "git" (Array.append [| "git"; "hash-object" |] args)

let cat_file (args : string array) =
  fork_and_execv "git" (Array.append [| "git"; "cat-file" |] args)

let update_index (args : string array) =
  fork_and_execv "git" (Array.append [| "git"; "update-index" |] args)

let write_tree (args : string array) =
  fork_and_execv "git" (Array.append [| "git"; "write-tree" |] args)

let read_tree (args : string array) =
  fork_and_execv "git" (Array.append [| "git"; "read-tree" |] args)

let commit_tree (args : string array) =
  fork_and_execv "git" (Array.append [| "git"; "commit-tree" |] args)

let log (args : string array) =
  fork_and_execv "git"
    (Array.append
       [| "git"; "--no-pager"; "log"; "--format=reference" |]
       args)

let add (args : string array) =
  fork_and_execv "git" (Array.append [| "git"; "add" |] args)

let restore (args : string array) =
  fork_and_execv "git" (Array.append [| "git"; "restore" |] args)

let commit (args : string array) =
  fork_and_execv "git" (Array.append [| "git"; "commit" |] args)

let show (args : string array) =
  fork_and_execv "git"
    (Array.append [| "git"; "--no-pager"; "show" |] args)

let revparse (args : string array) =
  fork_and_execv "git" (Array.append [| "git"; "rev-parse" |] args)

let diff (args : string array) =
  fork_and_execv "git"
    (Array.append [| "git"; "--no-pager"; "diff" |] args)

let status (args : string array) =
  fork_and_execv "git" (Array.append [| "git"; "status" |] args)

let head (args : string array) =
  fork_and_execv "git"
    (Array.append [| "git"; "symbolic-ref"; "HEAD" |] args)

let git (args : string array) =
  fork_and_execv "git" (Array.append [| "git" |] args)
