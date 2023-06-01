
We're on branch _dev_. We want to create a new branch _hotfix_ off trunk. But we don't want to
clone a new repo. And we want to leave our WIP on _dev_ alone. We create a new worktree (clone) in
`~/code/hotfix`, with a new branch _hotfix_ (`-b hotfix`) from branch _trunk_.
```sh
$ git branch | tee
* dev
trunk
$ git worktree add -b hotfix ~/code/hotfix trunk
Preparing ../hotfix (identifier hotfix)
HEAD is now at 62a2daf commit
```
If we don't want to create a new branch, we just want to navigate around or interact with a
different branch in a different directory hierarchy, we can omit `-b hotfix`:
```sh
$ git worktree add -b hotfix ~/code/hotfix trunk
```
