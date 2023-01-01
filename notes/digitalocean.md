
### Create a kubernetes cluster on a single node:
```sh
doctl kubernetes cluster create msk \
    --region lon1 \
    --count 1 \
    --size 's-2vcpu-4gb' \
    --wait
```

Config options:

| View available | with                                |
| -------------- | ----------------------------------- |
| droplet sizes  | `doctl compute size list`           |
| regions        | `doctl compute region list`         |
| images         | `doctl compute image list-user`     |
| k8s versions   | `doctl kubernetes options versions` |

Delete the cluster:
```sh
doctl kubernetes cluster delete msk
```

#### Existing clusters

##### View existing clusters
```sh
doctl k cluster list
```
##### Save an existing cluster kubeconfig locally
```sh
doctl k cluster config save $NAME
```

### Create a DO droplet
```sh
DROPLET_NAME=fungible
doctl compute droplet create $DROPLET_NAME \
  --image ubuntu-20-04-x64 \
  --size s-2vcpu-4gb-intel \
  --region lon1 \
  --ssh-keys "$(ssh-keygen -l -E md5 -f ~/.ssh/id_ecdsa.pub | grep -oe '\([a-f0-9]\{2\}:\)\{15\}[a-f0-9]\{2\}')"
```

#### SSH to our new droplet:
Get our new droplet's public IPv4 address:
```sh
IP=$(doctl compute droplet get $DROPLET_NAME --template '{{.PublicIPv4}}')
```
```
ssh root@$IP
```

#### Docker

You probably want to use docker. This is how to install it from Ubuntu repos:
```sh
sudo apt update
sudo apt install docker.io
sudo systemctl enable --now docker
```

#### k3d
```sh
curl -Lo k3d https://github.com/rancher/k3d/releases/download/v4.4.1/k3d-linux-amd64
sudo install k3d /usr/local/bin/
```

#### Skaffold
```sh
curl -Lo skaffold https://storage.googleapis.com/skaffold/releases/v1.21.0/skaffold-linux-amd64
sudo install skaffold /usr/local/bin/
```

#### kubectl
```sh
snap install kubectl --classic
```