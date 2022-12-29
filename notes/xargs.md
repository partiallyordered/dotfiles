
#### Use redirection with xargs
From: https://stackoverflow.com/a/845928
```sh
kubectl get deploy | grep test-centralledger | awk '{print $1}' | xargs -I{} sh -c 'kubectl get deploy -o yaml "$1" > "$1".yaml' -- {}
```
