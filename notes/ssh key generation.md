
#### General

From: https://blog.g3rt.nl/upgrade-your-ssh-keys.html

Tl;dr: Generate your new key with
```bash
ssh-keygen -o -a 100 -t ed25519
```

Parameters

| Parameter | Explanation                                                                                  | Example                                  |
| --------- | -------------------------------------------------------------------------------------------- | ---------------------------------------- |
| -f        | Output file.                                                                                 | `ssh-keygen -t rsa -f ./id_rsa`          |
| -P        | Passphrase. Supply an empty string to automate empty passphrase creation.                    | `ssh-keygen -P ''`                       |
| -C        | Comment.                                                                                     | `ssh-keygen -C "mattkingston@gmail.com"` |
| -a        | Number of KDF (key derivation function) rounds used. Higher is stronger. See man ssh-keygen. | `ssh-keygen -a 100`                      |
| -t        | Type                                                                                         | `ssh-keygen -t {rsa, ed25519}`           |
| -o        | RFC4716 format. See https://flak.tedunangst.com/post/new-openssh-key-format-and-bcrypt-pbkdf | `ssh-keygen -o {rsa, ed25519}`           |


#### CircleCI

Does not recognise:
- ed25519
- non-PEM keys
Use, e.g.:
```sh
ssh-keygen -m PEM -b 4096 -a 100 -t rsa -C "mattkingston@gmail.com" -P '' -f ./rsa
```

The above may no longer be true. You probably want to read this:
https://circleci.com/docs/2.0/gh-bb-integration/#creating-a-github-deploy-key

See: https://discuss.circleci.com/t/adding-ssh-keys-fails/7747/24

#### JWS key generation
From: https://gist.github.com/ygotthilf/baa58da5c3dd1f69fae9
See also: https://blog.miguelgrinberg.com/post/json-web-tokens-with-public-key-signatures
```sh
ssh-keygen -t rsa -b 4096 -m PEM -f jwtRS256.key
# Don't add passphrase
openssl rsa -in jwtRS256.key -pubout -outform PEM -out jwtRS256.key.pub
```
