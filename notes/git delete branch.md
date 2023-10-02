# remotely
```sh
git push --delete <remote_name> <branch_name>
# e.g.
git push --delete origin feature/shiny-new-feature
```
or
```sh
git push <remote_name> :refs/heads/<branch_name>
# e.g.
git push origin :refs/heads/feature/shiny-new-feature
```
The second method can be used to remove tags and branches at the same time:
```sh
git push origin :refs/heads/release/1.3 :refs/tags/1.3.5-rc0
```

# locally
```sh
git branch -d <branch_name>
```
