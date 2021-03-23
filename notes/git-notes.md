# Git Notes

**Git Plumbing commands:**
  * commands that do low-level work and were designed to be chained together
**Git Porcelain commands:**
  * commands that are user friendly
  * ex) `git add [filenames...]`, `git commit -m ""`, `git push`

# Git Init

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
