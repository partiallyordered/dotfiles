
### Get a GH token:
https://github.com/settings/tokens/new

### Curl files from github
Create a personal access token with `repo` (_Full control of private repositories_) permissions.
https://github.com/settings/tokens
```sh
TOKEN='abcde-the-token'
HOST='raw.githubusercontent.com'
ORG='redis-io'
REPO='redis'
FPATH='src/networking.c'
curl -LO "https://$TOKEN@$HOST/$ORG/$REPO/$FPATH"
# or
curl -LO \
    -H "Authorization: token $TOKEN" \
    -H 'Accept: application/vnd.github.v3.raw' \
    "https://$HOST/$ORG/$REPO/$FPATH"
```

### Configure repos using the CLI
In this example: disable merge commits on PRs. Docs on `gh repo edit`: https://cli.github.com/manual/gh_repo_edit
```sh
ORG_NAME=partiallyordered
gh repo list "$ORG_NAME" -L 100 --json 'owner,name' -q '.[] | "\(.owner.login)/\(.name)"' | xargs -P10 -I{} -- gh repo edit {} --enable-merge-commit=false
```

### Link to some lines easily:
https://stackoverflow.com/questions/23821235/how-to-link-to-specific-line-number-on-github
TL; DR: Either..
1.
  - Select lines of interest
  - (press Shift-Insert to disable tridactyl)
  - Press 'y' to toggle the canonical URL in your browser address bar
  - Do as you please with the URL
  - (press Shift-Insert to enable tridactyl)
or
2.
  - Select lines of interest
  - Click dots to the top-left of the selected lines
  - Select 'Copy permalink'

### Link to compare view between two commits:
```
https://github.com/<org>/<repo>/compare/<lhs-commit>..<rhs-commit>
```
Example:
```
https://github.com/modusbox/mojaloop-sdk-scheme-adapter/compare/9c86bd6..614f03f
```
Note that whenever you push you'll see most of this URL is constructed for you at the end of the
output of `git push`:
```
To github.com:casablanca-project/admin-portal-web-ui-backend.git
   5c93d4d..a00b452  feature/MOWDEV-1333 -> feature/MOWDEV-1333
```

More info here: https://github.blog/2010-03-01-introducing-github-compare-view/

### Edit and delete releases
https://help.github.com/en/articles/editing-and-deleting-releases

### Embed image somewhere
![Alt text](https://giphy.com/media/image-link.gif)

### See also
https://github.com/tiimgreen/github-cheat-sheet
