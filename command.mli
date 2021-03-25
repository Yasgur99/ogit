(** Parsing of git user commands. *)

(** The type [file_name] represents the file name that can be part of a
    user command. *)
type file_name = string

(** The type [commit_msg] represents the commit message that can be part
    of a user command. *)
type commit_msg = string

(** The type [branch_name] represents the branch name that can be part
    of a user command. *)
type branch_name = string

(** The type [cmd] represents a git command that is decomposed into a
    verb and possibly a file_name, commit_msg, or branch_name. *)
type cmd

(** Raised when an invalid command is parsed. *)
exception Invalid_cmd

(** Raised when an empty command is parsed. *)
exception Empty_cmd

(** [parse str] parses a user's input into a [cmd], as follows. The
    first word (i.e., consecutive sequence of non-space characters) of
    [str] becomes the verb. The rest of the words, if any, become either
    a file name or a commit message.

    Requires: [str] begins with "add", "rm", "commit", "branch",
    "checkout", "push", "pull", "status", "init", or "fetch". If [str]
    begins with "add", it must be followed by a file name. If [str]
    begins with "rm", it must be followed by a file name. If [str]
    begins with "commit", it must be followed by a commit message. If
    [str] begins with "branch", it must be followed by a branch name. If
    [str] begins with "checkout", it must be followed by a branch name.

    Examples of valid commands: "add file.ml", "rm file.ml", "commit
    implemented file", "branch master", "checkout master", "push",
    "pull", "status", "init", "fetch"

    Raises: [Empty_cmd] if [str] is the empty string.

    Raises: [Invalid_cmd] if the command is not a valid git command.*)
val parse : string -> cmd
