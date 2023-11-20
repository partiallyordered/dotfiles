#### Without cloning
```sh
git ls-remote --symref git@github.com:pre-commit/pre-commit.github.io HEAD
```

#### In a local clone
```sh
git symbolic-ref refs/remotes/origin/HEAD --short
```

##### Stripped in nu
```sh
git symbolic-ref refs/remotes/origin/HEAD --short | str replace -r '^origin\/' ''
```

#### References
https://stackoverflow.com/questions/28666357/how-to-get-default-git-branch
