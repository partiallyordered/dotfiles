
# Amending local changes
Don't do this with pushed commits. More [here](https://www.git-tower.com/learn/git/ebook/en/command-line/advanced-topics/undoing-things#start).

## Amending the most recent commit

### Correct the most recent commit message
```bash
git commit --amend -m "This is the correct message"
```

### Add some files to the most recent commit
```bash
git add some/changed/file.ext
git commit --amend -m "commit message"
```

### Change some files in the most recent commit
```bash
# make changes to files, then:
git add /path/to/file
git commit --amend
```

### Remove a file from the most recent commit
```bash
git reset HEAD\^ /path/to/file
git commit --amend
```

# Amending pushed changes

### Undo the most recent commit, revert files to staging
From [here](https://stackoverflow.com/questions/927358/how-do-i-undo-the-most-recent-local-commits-in-git).
```bash
git commit -m "Something terribly misguided"             # (1)
git reset HEAD~                                          # (2)
# edit files as necessary >>                             # (3)
git add ...                                              # (4)
git commit -c ORIG_HEAD                                  # (5)
```

### Force overwrite upstream
Generally not acceptable when you don't own upstream
```sh
git commit -m "Something terribly misguided"
# careful, make sure to stash any local changes you want to keep
git reset HEAD~ --hard
git push origin HEAD --force
```

# See also
https://github.blog/2015-06-08-how-to-undo-almost-anything-with-git/
