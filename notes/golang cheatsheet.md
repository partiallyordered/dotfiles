### Run a single test
where `[regexp]` matches the test/s you'd like to run:
```sh
go test -run [regexp]
```

### Install a specific git revision of a dependency/package
```sh
go get github.com/org/repo@af044c0995ff
```

### Pretty-print variables

JSON:
```go
x := map[string]interface{}{"a": 1, "b": 2}
b, err := json.MarshalIndent(x, "", "  ")
if err != nil {
    fmt.Println("error:", err)
}
fmt.Print(string(b))
```

`fmt`:
```go
fmt.Printf("%v", myVar)
```

From the Go doc:
>   %v the value in a default format. when printing structs, the plus flag (%+v) adds field names
>
>   %#v a Go-syntax representation of the value

### Replace the version of an import during development
```
replace github.com/org/repo/pkg => github.com/org/repo/pkg febc665
```
or
```
replace github.com/org/repo/pkg => github.com/org/repo/pkg branch-name
```
followed by `go mod tidy`

### Use a relative/local import
In `go.mod`, to replace e.g. the `golang.org/pkgs/errors` package:
```
replace golang.org/pkg/errors => /path/to/local/package
```

### Create submodules
From the [Go Wiki](https://go.dev/wiki/Modules#publishing-a-release):

> A new module version may be published by pushing a tag to the repository that contains the module
> source code. The tag is formed by concatenating two strings: a prefix and a version.
>
> The version is the semantic import version for the release. It should be chosen by following the
> rules of semantic import versioning.
>
> The prefix indicates where a module is defined within a repository. If the module is defined at
> the root of the repository, the prefix is empty, and the tag is just the version. However, in
> multi-module repositories, the prefix distinguishes versions for different modules. The prefix is
> the directory within the repository where the module is defined. If the repository follows the
> major subdirectory pattern described above, the prefix does not include the major version suffix.
>
> For example, suppose we have a module example.com/repo/sub/v2, and we want to publish version
> v2.1.6. The repository root corresponds to example.com/repo, and the module is defined in
> sub/v2/go.mod within the repository. The prefix for this module is sub/. The full tag for this
> release should be sub/v2.1.6.

### Use private modules
Set the `GOPRIVATE` environment variable to contain the private module prefixes. E.g.
```sh
GOPRIVATE="github.com/partiallyordered/myprivaterepo"
```
Or for an entire organisation:
```sh
GOPRIVATE="github.com/partiallyordered
```

### Clear module cache
```sh
go clean -modcache
```

### Multi-line strings
```go
`backticks
will
  not ignore whitespace
nor newlines
therefore
  this string consists of seven lines and
   this line is indented three spaces`

"concatenation" +
"works" +
"with the usual caveats" +
"and of course this string is just two\n" +
"lines"
```

### Create ed25519 keys
```go
func assert(e error) {
  if e != nil {
    panic(e)
  }
}
keyParams := generateEd25519SshKeyParams(t, sshUsername)
pub, priv, err := ed25519.GenerateKey(nil)
assert(err)
pubKey, err := ssh.NewPublicKey(pub)
assert(err)

pemBlock, err := ssh.MarshalPrivateKey(crypto.PrivateKey(priv), "")
assert(err)
privKey, err := ssh.ParseRawPrivateKey(pem.EncodeToMemory(pemBlock))
assert(err)
signer, err := ssh.NewSignerFromKey(privKey)
assert(err)

f, err := os.OpenFile("./id_ed25519", os.O_CREATE|os.O_TRUNC|os.O_RDWR, 0600)
assert(err)
assert(pem.Encode(f, pemBlock))
```
