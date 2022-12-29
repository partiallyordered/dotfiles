```sh
tr -dc '[:print:]' < /dev/urandom | head -c 20
```

# From here:
# https://www.howtogeek.com/howto/30184/10-ways-to-generate-a-random-password-from-the-command-line/

# Yep, this works.. Presumably the echo is for a new line.
```sh
< /dev/urandom tr -dc _A-Za-z0-9- | head -c${1:-32}; echo;
tr -dc _A-Z-a-z-0-9 < /dev/urandom | head -c${1:-32}
tr -cd '[:alnum:]' < /dev/urandom | fold -w30 | head -n1
openssl rand -base64 30
openssl rand -hex 30
< /dev/urandom tr -dc _A-Za-z0-9- | head -c6
dd if=/dev/urandom bs=1 count=32 2>/dev/null | base64 -w 0 | rev | cut -b 2- | rev
```

# Words
See: https://superuser.com/a/1246245
```sh
nix-shell -p xkcdpass --command xkcdpass
# interactive
nix-shell -p xkcdpass --command 'xkcdpass -i'
# from acrostic 'blah'
nix-shell -p xkcdpass --command 'xkcdpass -i -a blah'
```
Also: `nix-shell -p diceware` (but no examples because I used `xkcdpass` at the time of writing)
