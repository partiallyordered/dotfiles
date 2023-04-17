Forward local port 3001 to remote port 3000
```sh
ssh -L 3001:127.0.0.1:3000 user@example.org
```
The `-L` switch can be supplied multiple times to forward multiple ports simultaneously.

`gcloud` version:
```
gcloud compute ssh --zone "europe-west2-c" gcloud-instance-name -- -L 3001:127.0.0.1:3000
```

#### sshuttle

##### Tunnel through a remote machine
Note that this excludes localhost:
```sh
sshuttle --method=nft -r remote-machine-username@remote-machine-address -e 'gcloud compute ssh --zone "europe-west2-c" --project="my-project"' 0/0
```
Replace sshuttle's nft rules to also capture localhost. Note the port number used here by sshuttle,
12300, can be specified with the `-l` option to sshuttle:
```sh
nft flush chain inet sshuttle-ipv4-12300 sshuttle-ipv4-12300
nft add rule inet sshuttle-ipv4-12300 sshuttle-ipv4-12300 meta l4proto tcp ip daddr 0.0.0.0/0 redirect to :12300
```
It's also possible to use e.g. `10.0.0.1` as the destination address to capture, meaning all
requests to `10.0.0.1` will be forwarded to the remote machine:
```sh
sshuttle --method=nft -r remote-machine-username@remote-machine-address -e 'gcloud compute ssh --zone "europe-west2-c" --project="my-project"' 10.0.0.1
```
