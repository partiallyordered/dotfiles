
#### Specific file:
```sh
git log ./filename
```

#### Specific file, specific name:
Example:
```sh
git log -L 12,12:.gitlab-ci.yml
```
Generally:
```sh
git log -L start,end:filename
```
Another example, looks for changes for lines 14 through 14+5 in .gitlab-ci.yml:
```sh
git log -L 14,+5:.gitlab-ci.yml
```
