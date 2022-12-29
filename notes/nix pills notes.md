
No notes probably means that chapter hasn't been read.

### Chapter 2
Only skimmed this chapter so far.

#### Nix Database

Nix database exists at `/nix/var/nix/db`.

The schema is very simple: there's a table of valid paths, mapping from an auto increment integer
to a store path.

Then there's a dependency relation from path to paths upon which they depend. Paths exist in
/nix/store.

You can inspect the database by installing sqlite (nix-env -iA sqlite -f '<nixpkgs>') and then
running sqlite3 /nix/var/nix/db/db.sqlite.


### Chapter 4

Language basics: types, control structures, syntax etc.

https://nixos.org/nixos/nix-pills/basics-of-language.html

### Chapter 5

#### Functions
- Take single argument: `x: x*2`
- Are first-class: `double = x: x*2`
- Therefore support multiple arguments by currying: `mul = a: b: a*b`
- Therefore support partial application: `(mul 5) 6`
- Argument sets: `mul { a, b }: a*b`. Only a set with exactly the attributes required by the
    function is accepted, nothing more, nothing less.
- Default attributes: `mul = { a, b ? 2 }: a*b`
- Variadic attributes: `mul = { a, b, ... }: a*b`
- Accessing variadic attributes:
    ```
    nix-repl> mul = s@{ a, b, ... }: a*b*s.c
    nix-repl> mul { a = 3; b = 4; c = 2; }
    24
    ```

#### Imports

a.nix:
```nix
3
```
b.nix:
```nix
4
```
mul.nix:
```nix
a: b: a*b
```

```
nix-repl> a = import ./a.nix
nix-repl> b = import ./b.nix
nix-repl> mul = import ./mul.nix
nix-repl> mul a b
12
```

Import a file, and it gets parsed as expression. The scope of the imported file does not inherit
the scope of the importer. Pass information to the module by defining a top-level function in that
file.

### Chapter 6

#### Derivations

##### The Derivation Function

Manual page for the derivation function: https://nixos.org/nix/manual/#ssec-derivation
```nix
# Lazily (not-)executed
derivation { name = "myderivation"; builder = "mybuilder"; system = builtins.currentSystem; }
# Executed
d = derivation { name = "myderivation"; builder = "mybuilder"; system = builtins.currentSystem; }
```
The derivation function returns a plain set, containing build environment. But not any environment
that might make the build non-deterministic. Inspect the output:
```nix
builtins.attrNames d
[ "all" "builder" "drvAttrs" "drvPath" "name" "out" "outPath" "outputName" "system" "type" ]
```


