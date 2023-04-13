Forward local port 3001 to remote port 3000
```sh
ssh -L 3001:127.0.0.1:3000 user@example.org
```
The `-L` switch can be supplied multiple times to forward multiple ports simultaneously.
