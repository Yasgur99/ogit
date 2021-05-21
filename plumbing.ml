(*******************************************)
(* Plumbing *)
(*******************************************)
module type Plumbing = sig
  (** Representation of raw git commands.

      This module represents the calling of calling git commands with
      any command line arguments *)

  (** The representation of the result of executing a git command *)
  type result

  (* [make result o e] is a result where [o] is the lines of stdout and
     [e] is the lines of stderr *)
  val make_result : string list -> string list -> string list -> result

  (* [get_stdout r] is the lines of stdout *)
  val get_stdout : result -> string list

  (* [get_stdout r] is the lines of stderr *)
  val get_stderr : result -> string list

  (* [get_out r] is the lines of of both stdout and stderr in the order
     that they were sent to their respective streams.

     For example, if data was written to stdout, then stderr, and then
     stdout, again, then [get_out r] follows that same order. *)
  val get_out : result -> string list

  (** [init args] calls git init with arguments [args] *)
  val init : string array -> result

  (** [push] calls git push with arguments [args]*)
  val push : string array -> result

  (** [pull args] calls git pull with arguments [args] *)
  val pull : string array -> result

  (** [hash_object args] calls git hash-object with arguments [args] and
      is the output to standard output *)
  val hash_object : string array -> result

  (** [cat_file args] calls git cat-file with arguments [args] *)
  val cat_file : string array -> result

  (** [update_index args] calls git update-index with arguments [args] *)
  val update_index : string array -> result

  (** [write_tree args] calls git write-tree with arguments [args] *)
  val write_tree : string array -> result

  (** [read_tree args] calls git read-tree with arguments [args] *)
  val read_tree : string array -> result

  (** [commit_tree args] calls git commit-tree with arguments [args] *)
  val commit_tree : string array -> result

  (** [log args] calls git update-index with arguments [args] *)
  val log : string array -> result

  (** [add args] calls git add with arguments [args] *)
  val add : string array -> result

  (** [restore args] calls git restore with arguments [args] *)
  val restore : string array -> result

  (** [commit] calls git commit with arguments [args] *)
  val commit : string array -> result

  (** [show args] calls git show with arguments [args] *)
  val show : string array -> result

  (** [diff args] calls git diff with arguments [args] *)
  val diff : string array -> result

  (** [revparse args] calls git rev-parse with arguments [args] *)
  val revparse : string array -> result

  (** [status args] calls git status with arguments [args] *)
  val status : string array -> result

  (** [head args] calls git symbolic-ref HEAD with arguments [args]*)
  val head : string array -> result

  (** [checkout args] calls git checkout with arguments [args]*)
  val checkout : string array -> result

  (** [git args] calls git with arguments [args] *)
  val git : string array -> result
end

module type PlumbingWithSet = sig
  include Plumbing

  val set_log_data : string list -> string list -> string list -> unit

  val set_status_data :
    string list -> string list -> string list -> unit

  val set_head_data : string list -> string list -> string list -> unit
end

module ProdPlumbing : Plumbing = struct
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

  (** [fork_and_execvp e a] is the result of executing program [exe]
      with arguments [args]*)
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

  let push (args : string array) =
    fork_and_execv "git" (Array.append [| "git"; "pull" |] args)

  let pull (args : string array) =
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

  let diff (args : string array) =
    fork_and_execv "git"
      (Array.append [| "git"; "--no-pager"; "diff" |] args)

  let revparse (args : string array) =
    fork_and_execv "git" (Array.append [| "git"; "rev-parse" |] args)

  let status (args : string array) =
    fork_and_execv "git" (Array.append [| "git"; "status" |] args)

  let head (args : string array) =
    fork_and_execv "git"
      (Array.append [| "git"; "symbolic-ref"; "HEAD" |] args)

  let checkout (args : string array) =
    fork_and_execv "git" (Array.append [| "git"; "checkout" |] args)

  let git (args : string array) =
    fork_and_execv "git" (Array.append [| "git" |] args)
end

module MockPlumbing : PlumbingWithSet = struct
  (** Types and methods to access and construct the type *)
  type result = {
    stdout : string list;
    stderr : string list;
    out_and_err : string list; (*exit_code : int;*)
  }

  let get_stdout result = result.stdout

  let get_stderr result = result.stderr

  let get_out result = result.out_and_err

  let make_result out err out_and_err =
    { stdout = out; stderr = err; out_and_err }

  let git (args : string array) = failwith "git unimplemented"

  let init (args : string array) =
    make_result
      [ "Initialized empty Git repository in /home/fake/.git/" ]
      []
      [ "Initialized empty Git repository in /home/fake/.git/" ]

  let push (args : string array) =
    let new_args = Array.of_list ("push" :: Array.to_list args) in
    git new_args

  let pull (args : string array) =
    let new_args = Array.of_list ("pull" :: Array.to_list args) in
    git new_args

  let hash_object (args : string array) =
    let new_args =
      Array.of_list ("hash_object" :: Array.to_list args)
    in
    git new_args

  let cat_file (args : string array) =
    let new_args = Array.of_list ("cat_file" :: Array.to_list args) in
    git new_args

  let update_index (args : string array) =
    let new_args =
      Array.of_list ("update-index" :: Array.to_list args)
    in
    git new_args

  let write_tree (args : string array) =
    let new_args = Array.of_list ("write-tree" :: Array.to_list args) in
    git new_args

  let read_tree (args : string array) =
    let new_args = Array.of_list ("read-tree" :: Array.to_list args) in
    git new_args

  let commit_tree (args : string array) =
    let new_args =
      Array.of_list ("commit-tree" :: Array.to_list args)
    in
    git new_args

  let log_data =
    ref
      {
        stdout =
          [
            "59689ce (setup project files, 2021-03-22)";
            "b92c19e (Initial commit, 2021-03-04)";
          ];
        stderr = [];
        out_and_err =
          [
            "59689ce (setup project files, 2021-03-22)";
            "b92c19e (Initial commit, 2021-03-04)";
          ];
      }

  let set_log_data out err out_and_err =
    log_data := make_result out err out_and_err

  let log (args : string array) = !log_data

  let add (args : string array) =
    let new_args = Array.of_list ("add" :: Array.to_list args) in
    git new_args

  let head (args : string array) = failwith "unimplemented"

  let checkout (args : string array) = failwith "unimplemented"

  let git (args : string array) = failwith "unimplemented"

  let restore (args : string array) =
    let new_args = Array.of_list ("restore" :: Array.to_list args) in
    git new_args

  let commit (args : string array) =
    let new_args = Array.of_list ("commit" :: Array.to_list args) in
    git new_args

  let show (args : string array) =
    let new_args = Array.of_list ("show" :: Array.to_list args) in
    git new_args

  let diff (args : string array) =
    let new_args = Array.of_list ("diff" :: Array.to_list args) in
    git new_args

  let head_data =
    ref
      {
        stdout = [ "origin/master" ];
        stderr = [];
        out_and_err = [ "origin/master" ];
      }

  let set_head_data out err out_and_err =
    head_data := make_result out err out_and_err

  let head (args : string array) = !head_data

  let checkout (args : string array) =
    let new_args = Array.of_list ("push" :: Array.to_list args) in
    git new_args

  let revparse (args : string array) =
    {
      stdout = [ "origin/master" ];
      stderr = [];
      out_and_err = [ "origin/master" ];
    }

  let set_status_data out err out_and_err =
    log_data := make_result out err out_and_err

  let status_data = ref { stdout = []; stderr = []; out_and_err = [] }

  let status (args : string array) = !status_data
end
