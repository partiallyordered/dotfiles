
From: https://stackoverflow.com/questions/3895453/how-do-i-make-a-git-commit-in-the-past

```sh
# The `date --date` command accepts a wider range of inputs and produces output in a format
# acceptable to git
export DATE="$(date --date "yesterday 13:22:43")"
export GIT_AUTHOR_DATE="$DATE"
export COMMITTER_DATE="$GIT_AUTHOR_DATE"
git commit -m "stuff"
```

As a one-liner:
```sh
GIT_AUTHOR_DATE="$(date --date "yesterday 13:22:43")" GIT_COMMITTER_DATE="$GIT_AUTHOR_DATE" git commit -m "stuff"
```


It's possible to view "organic" historical examples with:
```sh
git show HEAD~ --format=fuller
```
Results look like:
```
commit a4db254d33c8a30e2729b3d6bcdb46a3a51e0528
Author:     msk- <mattkingston@gmail.com>
AuthorDate: Tue Oct 5 19:02:40 2021 +0100
Commit:     msk- <mattkingston@gmail.com>
CommitDate: Tue Oct 5 19:02:40 2021 +0100
```
Note that the AuthorDate and CommitDate are the same.


From the `git commit` man page:
```
DATE FORMATS
       The GIT_AUTHOR_DATE and GIT_COMMITTER_DATE environment variables support the following date
       formats:

       Git internal format
           It is <unix timestamp> <time zone offset>, where <unix timestamp> is the number of seconds
           since the UNIX epoch.  <time zone offset> is a positive or negative offset from UTC. For
           example CET (which is 1 hour ahead of UTC) is +0100.

       RFC 2822
           The standard email format as described by RFC 2822, for example Thu, 07 Apr 2005 22:13:13
           +0200.

       ISO 8601
           Time and date specified by the ISO 8601 standard, for example 2005-04-07T22:13:13. The
           parser accepts a space instead of the T character as well. Fractional parts of a second will
           be ignored, for example 2005-04-07T22:13:13.019 will be treated as 2005-04-07T22:13:13.

               Note
               In addition, the date part is accepted in the following formats: YYYY.MM.DD, MM/DD/YYYY
               and DD.MM.YYYY.

       In addition to recognizing all date formats above, the --date option will also try to make sense
       of other, more human-centric date formats, such as relative dates like "yesterday" or "last
       Friday at noon".
```
