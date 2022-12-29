Note that git will refer to revisions as "good" or "bad"; these correspond to "old" and "new",
respectively.
```sh
git bisect start
# Set the current revision as the "new" revision
git bisect new
# Find an old revision that contains the thing you're looking for
git bisect old 1234abc
# Git will now check out the repo repeatedly using a binary search pattern in response to input,
# e.g. tell git that the current revision does not contain the thing you're looking for:
git bisect new
# and it will check out a new revision.
# Tell git that the current revision does contain the thing you're looking for:
git bisect old
# Tell git to do the work for you
git bisect run ./test --name=some_test
```
