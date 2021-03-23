# Git Notes

**Git Plumbing commands:**
  * commands that do low-level work and were designed to be chained together
**Git Porcelain commands:**
  * commands that are user friendly
  * ex) `git add [filenames...]`, `git commit -m ""`, `git push`

## Git Init

`git init` creates the following files:

(Mainly) Unimportant parts (to us)
  * config
    * project-specific config options 
  * decsription
    * built for GitWeb program
    * we can ignore this
  * hooks/
    * client or server side hooks scripts
  * info/
    * global exclude file for ignored patterns

Core parts:
  * objects/
    * stores all content for oru database
  * refs/
    * pointers into commit objects in that data (branches, tags, remotes, and more)
  * HEAD
    * the branch we have currently checked out
  * index
    * staging area information

## Git Objects

Git is a simple key-value data store

We insert content into a repository and it hands us back a unique key to later retrieve that content

**Plumbing Command:** `git hash-object`
  * takes some data, and stores it into `.git/objects` 
  * gives us back unique key that refers to that data object

ex)
```
$ echo "test" | git hash-object -w --stdin
9daeafb9864cf43055ae93beb0afd6c7d144bfa4
```

The `-w` option tells it to write the object to the database
The `--stdin` tells `git hash-object` to parse from stdin (otherwise it expects a filename argument)

It outputs a header plus a SHA-1 hash
The first two characters are the header
In the example above the header is `9d` and the SHA-1 hash is `aeafb9864cf43055ae93beb0afd6c7d144bfa4`

If we look in the `.git/objects` directory:
  * there is a dir with the header
  * and a file with the name of the hash
  * the contents of the file is "test"

If we create a file and save it in our database:
```
$ echo "version 1" > test.txt
$ git hash-object -w test.txt
```
and update the file
```
$ echo "version 2" > test.txt
$ git hash-object -w test.txt
```

Then both objects are still stored

At this point we can delete `test.txt` and retrieve either version:
`get cat-file -p [hash]`

However, we are not storing the filename in our system, just the content
This object type is called a **blob**
We can check that its a blob using `git cat-file -t [hash]`

## Tree Objects

Solves the problem of storing filename
Also allows us to store a group of files together

* Context is stored as a tree and blob objects
* Trees correspond to directory entries
* Blobs correspond to inodes or file contents
* Each tree contains one or more entries
* An entry is
    * sha-1 hash of a blob or subtree
    * associated mode, type, and filename
ex) 
```
$ git cat-file -p master^{tree}
100644 blob 8f94139338f9404f26296befa88755fc2598c289      Rakefile
040000 tree 99f1a6d12cb4b6f19c8655fca46c3ecf317074e0      lib
```

Note that lib is a pointer to another tree
The `master^{tree}` specifies tree objected pointed to by last commit on master

We could check whats in lib with:
`git cat-file 99f1a6d12cb4b6f19c8655fca46c3ecf317074e0`

Git creates tree by taking state of staging area or index and writes a series of tree objects from it

So to create a tree object we need to set up an index by staging some files

We can do this with the plumbing command `git update-index`
We must pass it the `--add` option because the file does not yet exist in our staging area
We must also pass `--cacheinfo` because the file were adding ins't in our directory but is in our database

ex) `git update-index --add --cacheinfo 10644 [hash] [filename]`

`10644` is the mode of a normal file
`100755` is an executable file
`120000` is a symbolic link
These are the only 3 modes that are valid for blobs

The has was generated with `git hash-object`

Now we can use `git write-tree` to write the staging area out to a tree object
    * generates a tree object from the state of the index if the tree does not yet exist

We can preform multiple `git update-index` to stage multiple files and write out that tree with a single `git write-tree`

We can read trees into staging area with `git read-tree`
If we pass it the `--prefix` option to give the tree a name
ex)
`$ git read-tree --prefix=bak [hash]`

## Commit Objects

In order to recall the snapshots we must remember the SHA-1 values
We don't have any info about who saved, when, or why
The commit object stores this for us

`git commit-tree [hash-prefix]` where the `hash-prefix` belongs to a tree

ex) `echo 'First commit' | git commit-tree d8329f`

This will give us a new hash.
We can look at the new commit object with `git cat-file -p [new-hash]`

A commit message has following format
    * top level tree for snapshot of project at that point
    * parent commits if any
    * author/commiter info
    * blank line
    * commit message

We can specify a parent commit with the `-p` option
ex) `$ echo 'Second commit' | git commit-tree 0155eb -p fdf4fc3`

We can new view the history with `git log --stat [hash]`

We now have the low level operations to build up a git history for `git add` and `git commit`

# Object Storage

Previously we mentioned there was a header stored with every object added to object database

Its of the form:
header = type sizeInBytes\0
ex) "blob 16\u0000"

git concatenats header and original content and calculates SHA-1 of the new content

Now we have the  valid `git hash-object` result

Now its time to store the data:

compress content with zlib
write zlib-deflated content to an object on disk
the first two chars of sha-1 value are directory
the last 38 are filename

if we do `git cat-file -p [hash]` where hash is the full 40, we should get the contents we originally saved
