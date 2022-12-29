
Create a relative date
```sh
date --date 'tomorrow 0830'
date --date 'today 0830'
date --date '-3 days'
date --date '+3 days'
```

Format a date ISO8601
```sh
date -Iseconds
date -Iminutes
```

Format a date ISO8601 Zulu
```sh
date -u '+%FT%TZ'
date -u '+%FT%T.000Z'
FMT='+%FT%TZ'
date -u $FMT --date '-3 days'
```

Format a date "Mojaloop API time"
```sh
date -u '+%a, %d %b %Y %H:%M:%S GMT'
```

See also:
`info date`
`man date`
https://www.linuxjournal.com/content/doing-date-math-command-line-part-i
