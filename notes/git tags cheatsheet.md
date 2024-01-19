
### Add lightweight tag- just a pointer to a revision
```sh
git tag v1.4
```

### Add annotated tag- full git object, with creator, message
```sh
git tag -a v1.4 -m "my version 1.4"
```

### Push tag
```sh
git push --tags
```

### "Copy" tag
```sh
git tag <new> <old>
```

### Delete tag locally
```sh
git tag -d <tag>
```

### Delete tag remotely
Either delete the tags locally first, or run `git fetch --tags --prune-tags` after deleting
remotely.
```sh
git push origin :refs/tags/tagName
```

### Make sure other repo users also get rid of the tag
```sh
git pull --prune --tags
```

### Find commit pointed at by a given tag
```sh
git rev-list -n 1 $TAG
```

### Create tag remotely but not locally
```sh
# e.g.
git push origin HEAD:refs/tags/tagName
# which is the same as
git push origin $(git rev-parse HEAD):refs/tags/tagName
# or
git push origin 54f73e39d546be22ac000300ed1bf938b3793bca:refs/tags/tagName
# or
git push origin 54f73e3:refs/tags/tagName
```
