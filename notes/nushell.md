
#### Escaping to the system

> Escaping with the caret prefix calls the command that's in the user's PATH (e.g. /bin/ls instead
> of Nu's internal ls command).
```nu
^ls
which ^ls
```

Reference: https://www.nushell.sh/book/escaping.html

#### Compare dates

ls | where created < ((date now) - 30day)

See also:
- https://www.nushell.sh/book/types_of_data.html#durations
- https://www.nushell.sh/book/types_of_data.html#dates

#### Augmenting tables
```nu
ls */.github/**/*.yml
| get name
| path parse
| insert repo {|row| (git -C $row.parent rev-parse --show-toplevel)}
```

#### Handling external commands in a structured way
Use the `complete` command to run an external to completion and get a record with `stdout`,
`stderr`, and `exit_code` fields:
```sh
do { git rev-parse --show-toplevel } | complete
```

#### Type conversion
```nu
help into
```

#### Loading data
In general, use `split`:
```nu
git for-each-ref
    | lines
    | split column -r '\s+' rev type ref
```
Sometimes, `detect columns` is useful also:
```nu
git for-each-ref | detect columns --no-headers | rename rev type ref
```
See also:
- `help rename`
- `help split`
- `help detect columns`
- [Working with lists](https://www.nushell.sh/book/working_with_lists.html)
- [Loading data](https://www.nushell.sh/book/loading_data.html#handling-strings)
