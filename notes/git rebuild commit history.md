```sh
# 1. back up my changes upstream
git push
# 2. Set my local "pointer" to the upstream branch I am targeting for my merge. All my local
# changes will now appear unstaged + uncommitted.
git reset origin/develop
# 3. (optionally) create a new branch so I don't lose my changes
git switch -c my-feature-tidy
# 4. commit by commit, rebuild my changes more tidily (nicer to use lazygit or similar for this
#    step, as it's easier to add line by line or block by block interactively)
git add -p some/file.ext some/related/change.ext
git commit -m "made some related changes to two different files"
git add -p unrelated/changes.ext
git commit -m "made another change that is semantically distinct from the earlier changes, but semantically connected by the larger change we're making, i.e. the PR/issue"
# 5a. push my changes
git push -u origin my-feature-tidy
# 5b. or if I didn't create a new branch, or want to force push onto upstream
git push --force origin my-feature-tidy:my-feature-branch-upstream
```