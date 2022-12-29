Use [git-filter-repo](https://github.com/newren/git-filter-repo):
You will lose any stashed changes, and any uncommitted changes. Make sure you commit any changes
you'd like to keep, and apply and commit any stashed changes you'd like to keep. You will lose
tracking information and need to re-add your remotes. It's best to make sure you've merged any
changes from upstream before proceeding.
```sh
git filter-repo --invert-paths --path PATH-TO-YOUR-FILE-WITH-SENSITIVE-DATA
```
References:
- _git-filter-repo_ usage: https://github.com/newren/git-filter-repo#how-do-i-use-it
- https://help.github.com/en/articles/removing-sensitive-data-from-a-repository
