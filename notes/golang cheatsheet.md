### Install a specific git revision of a dependency/package
```sh
go get github.com/org/repo@af044c0995ff
```

### Use a relative import
In `go.mod`, to replace e.g. the `golang.org/pkgs/errors` package:
```sh
replace golang.org/pkg/errors => /path/to/local/package
```
