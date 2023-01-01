
#### Messages indicating the disk is full?

Check disk usage:
```sh
df -h
```

Check inode usage:
```sh
df -i
```

Check inode usage by directory:
```sh
find / -xdev -printf '%h\n' | sort | uniq -c | sort -k 1 -n
```

#### Garbage-collect Nix

Nix uses a lot of inodes.

Note that the following instructions regarding garbage collecting Nix may not work as it may not be
possible to lock the database due to the disk full condition. In this case, you will need to make
some space with `docker system prune` or by manually deleting some files.

Looks like it might be quite tricky to increase the number of supported inodes without a reformat.

##### Garbage collect Nix:
```sh
nix-collect-garbage
```
Clear out stuff older than a certain date. E.g. 30 days:
```sh
nix-collect-garbage --delete-older-than 30d
```

##### Delete old Nix generations:
```sh
nix-collect-garbage -d
```

#### Other options

##### Docker

Often it's possible to regain some space from docker:
```sh
docker system prune
```
Or
```sh
docker system prune --all=true -f --volumes=true
```

`docker system prune` options:
```
OPTIONS
       -a, --all[=false]      Remove all unused images not just dangling ones

       --filter=      Provide filter values (e.g. 'label==')

       -f, --force[=false]      Do not prompt for confirmation

       -h, --help[=false]      help for prune

       --volumes[=false]      Prune volumes
```

See docker cheatsheet for more deletion.

##### Delete some stuff

Check inode usage by directory:
```sh
find / -xdev -printf '%h\n' | sort | uniq -c | sort -k 1 -n
```

##### Nothing ever good came from NPM anyway

Delete `node_modules`:
```sh
find -type d -name 'node_modules' | xargs -I{} rm -rf {}
```