
## Configure a remote for upstream

From:
* https://help.github.com/en/articles/configuring-a-remote-for-a-fork
* https://help.github.com/en/articles/syncing-a-fork

1. List the current configured remote repository for your work.
```bash
git remote -v
```

2. Specify a new remote upstream repository that will be synced with the fork.
```bash
git remote add upstream https://github.com/ORIGINAL_OWNER/ORIGINAL_REPOSITORY.git
```
or
```bash
git remote add upstream ssh://git@github.com/ORIGINAL_OWNER/ORIGINAL_REPOSITORY.git
```

3. Fetch from upstream
```bash
git fetch upstream
```

4. Check out your fork's local master branch
```bash
git checkout master
```

5. Merge the changes from upstream/master into your local master branch.
```bash
git merge upstream/master
```
