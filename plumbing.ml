(** Types and methods to access and construct the type *)
type result = { 
  stdout : string list;
  stderr : string list;
  (*exit_code : int;*)
}

let make_result out err =
  {
    stdout =  out;
    stderr = err;
  }

let get_stdout result = 
  result.stdout

let get_stderr result =
  result.stderr

(** Helper Methods *)

(** [read fd] is the lines of file referenced by descriptor [fd] *)
let read (fd : Unix.file_descr) : string list =
  let in_ch = Unix.in_channel_of_descr fd in
  let lines = ref [] in
  try 
      while true do
        lines:= input_line in_ch :: !lines
      done;
      !lines
    with End_of_file ->
      close_in in_ch;
      !lines

(** [fork_and_execvp e a] is the result of executing program [exe] 
    with arguments [args]*)
let fork_and_execv (exe : string) (args : string array) : result = 
  let inp_stdout, out_stdout = Unix.pipe() in
  let inp_stderr, out_stderr= Unix.pipe() in
  let pid = Unix.fork () in
  if pid = 0 then (
    Unix.close inp_stdout; (* Not used by child *)
    Unix.close inp_stderr; (* Not used by child *)
    Unix.dup2 out_stdout Unix.stdout; (* Bind pipe to stdout *)
    Unix.dup2 out_stderr Unix.stderr; (* Bind pipe to stderr *)
    Unix.execvp exe args 
  ) else (
    Unix.close out_stdout;
    Unix.close out_stderr;
    let stdout = read inp_stdout in
    let stdin = read inp_stderr in
      Unix.close inp_stderr;
      Unix.close inp_stdout;
      make_result stdout stdin
  )

let init (args : string array) =
  fork_and_execv "git" (Array.append [|"git"; "init"|] args)

let hash_object (args : string array) =
  fork_and_execv "git" (Array.append [|"git"; "hash-object"|] args)

let cat_file (args : string array) =
  fork_and_execv "git" (Array.append [|"git"; "cat-file"|] args)

let update_index (args : string array) =
  fork_and_execv "git" (Array.append [|"git"; "update-index"|] args)

let write_tree (args : string array) = 
  fork_and_execv "git" (Array.append [|"git"; "write-tree"|] args)

let read_tree (args : string array) =
  fork_and_execv "git" (Array.append [|"git"; "read-tree"|] args)

let commit_tree (args : string array) =
  fork_and_execv "git" (Array.append [|"git"; "commit-tree"|] args)

let log (args : string array) =
  fork_and_execv "git" (Array.append [|"git"; "--no-pager"; "log"|] args)

let add (args : string array) =
  fork_and_execv "git" (Array.append [|"git"; "add"|] args)
  
let commit (args : string array) =
  fork_and_execv "git" (Array.append [|"git"; "commit"|] args)

let show (args : string array) =
  fork_and_execv "git" (Array.append [|"git"; "--no-pager"; "show"|] args)

let diff (args : string array) =
  fork_and_execv "git" (Array.append [|"git"; "--no-pager"; "diff"|] args)
 
let status (args : string array) =
  fork_and_execv "git" (Array.append [|"git"; "status"|] args)
 

