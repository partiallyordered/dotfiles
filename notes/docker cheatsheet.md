### Share a network with other running containers
Note this can be combined with the following note on accessing the host using a specific host name.
I.e. a container can access the host using the reserved name, as well as other containers on its
network with their host names. Ports on the containers can still be published to the host.
```sh
NETWORK_NAME=my-net
ECHO_SERVER_NAME=echoserver
docker network create --driver=bridge "$NETWORK_NAME"
docker run --rm -d \
    --network="$NETWORK_NAME" \
    --name="$ECHO_SERVER_NAME" \
    hashicorp/http-echo \
    -text="hello world"
docker run --rm \
    --network="$NETWORK_NAME" \
    curlimages/curl \
    --silent \
    "$ECHO_SERVER_NAME":5678
docker stop "$ECHO_SERVER_NAME"
docker network rm "$NETWORK_NAME"
```

### Access the host through a specific host name
#### Docker
It's possible to combine this technique with the previous one. See the note above.
```sh
docker run --rm \
    --add-host=host.docker.internal:host-gateway \
    busybox \
    ping host.docker.internal
```
#### Podman
Podman has a default entry in `/etc/hosts` _host.containers.internal_ which maps to the host.

### Bind mount docker host engine
Prints itself:
```sh
docker run -it \
    --mount type=bind,source=/var/run/docker.sock,destination=/var/run/docker.sock \
    docker \
    docker ps
```
Replace `docker ps` with `sh` for an interactive shell.

### Go Templates
Use the `json` function to help figure out what you can access.
```sh
docker network ls --format='{{json .}}' | jq '.'
```
Example output:
```json
{
  "CreatedAt": "2021-04-05 15:07:15.916798359 +0100 BST",
  "Driver": "bridge",
  "ID": "db79a8bc93df",
  "IPv6": "false",
  "Internal": "false",
  "Labels": "",
  "Name": "bridge",
  "Scope": "local"
}
```
More at: https://golang.org/pkg/text/template/

### List all image labels
```sh
podman inspect --format='{{ range $k, $v := .Config.Labels }}{{ $k }}:{{ $v }}\n{{ end }}' $image
# with a little more class
podman inspect --format='{{ range $k, $v := .Config.Labels }}{{ $k }}:{{ $v }}\n{{ end }}' ghcr.io/mojaloop/finance-portal-v2-ui | column -s':' -t -l2
# remotely, without pulling the image
nix-shell -p regctl --command 'regctl image config regclient/regsync:latest --format "{{ jsonPretty .Config.Labels }}"'
```

### Alpine package manager

Virtual package (called `native-deps` in this example):
```dockerfile
RUN apk --no-cache add --virtual native-deps \
  g++ gcc libgcc libstdc++ linux-headers make python && \
  yarn install --quiet node-gyp -g && \
  yarn install --prod && \
  apk del native-deps
```

### Clean house
```sh
docker container rm -f $(docker container ls -a | tail -n+2 | awk '{print $1}')
docker rmi -f $(docker image ls -a | tail -n+2 | awk '{print $3}')
docker system prune --all=true -f --volumes=true
systemctl stop docker
systemctl disable docker
rm -rf /run/docker
rm -rf /etc/docker
rm -rf /var/lib/docker
```

### Correctly handle signals
Use the _exec_ form, not the _shell_ form:
```Dockerfile
CMD "this is shell form, don't use this"
CMD ["this", "is", "exec", "form,", "use", "this"]
```

If using a script, you need to exec inside the script:
```sh
exec nginx -g "daemon off;"
```

Your exec target must handle signals:
```sh
# Won't work:
exec ./ignore-signals.sh
# Should work, as long as you've handled signals:
exec ./handles-signals-appropriately.sh
```

### Examine image layers
```sh
nix-shell -p dive
dive nginx:stable-alpine
# And, separately:
podman unshare
podman image mount $image_name
```

### Podman share images with docker
```sh
podman run --rm -it docker-daemon:alpine echo "hello, world"
podman push alpine docker-daemon:alpine
podman pull docker-daemon:alpine
```