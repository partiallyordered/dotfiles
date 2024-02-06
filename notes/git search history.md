Search diffs for the term `searchstring` with the `-S` ("pickaxe") option:

```sh
git log -S searchstring
```

Options:

    -p: print patch
    -p file: save patch to file
    -G: like -S but with a regex search term, e.g. `-G [Mm]k[Ff]ifo`
    --all: searches all branches and tags
    --branches[=<pattern>]
    --tags[=<pattern>]

