
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
