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


