
Variously from
https://stackoverflow.com/questions/13064613/how-to-prune-local-tracking-branches-that-do-not-exist-on-remote-anymore

```sh
git remote prune origin
```

Remove all branches that do not exist on master:
```sh
git branch -r | awk '{print $1}' | egrep -v -f /dev/fd/0 <(git branch -vv | grep origin) | awk '{print $1}' | xargs git branch -d
```

Remove all local branches that are merged into master:
```sh
git branch --merged master | egrep -v '^\s*\*?\s*master$' | xargs git branch -d
```
