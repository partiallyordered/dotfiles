
From: https://stackoverflow.com/a/46726955

1. Create new branch with your changes.
```sh
git checkout -b mybranch
```
2. (Optionally) push new branch code to remote.
```sh
git push origin mybranch
```
3. Checkout back to master branch.
```sh
git checkout master
```
4. Reset master branch code with remote server and remove local commit. Take care to stash or back
   up any uncommitted changes.
```sh
git reset --hard origin/master
```
