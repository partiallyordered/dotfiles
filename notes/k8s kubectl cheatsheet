
### Create a temporary container (actually deployment or job) in your cluster

#### Deploy the `busybox:musl` container. Call the deployment `msk-temp-busybox-deployment`.
```bash
# if DEPLOYMENT_NAME is not supplied, a default will be used
export DEPLOYMENT_NAME=msk-temp-busybox-deployment
kubectl run --generator=run-pod/v1 --rm --image=busybox:musl -it $DEPLOYMENT_NAME
```

#### Deploy the `alpine` container into the ci namespace, install socat, port-forward
* Running multiple commands must be done via `sh -c`, otherwise the connection to the container is
    lost. (Does `&&` fork?).
* if DEPLOYMENT_NAME is not supplied, a default (in this case, 'socat') will be used
```bash
export DEPLOYMENT_NAME=msk-socat
kubectl run --rm --generator=run-pod/v1 --image=alpine -it --namespace=ci $DEPLOYMENT_NAME \
    -- sh -c "apk --no-cache add socat && socat -d -d 'TCP-LISTEN:80,reuseaddr,fork' 'TCP:localhost:8765'"
```

#### Port-forward to your new deployment:
```bash
# see above for the value of DEPLOYMENT_NAME
kubectl port-forward deployments/$DEPLOYMENT_NAME <port-number>
```

### Simulate service unavailability
Bring the pods down (assuming a single replica at present):
```sh
kubectl scale --current-replicas=1 --replicas=0 deployment/$DEPLOYMENT_NAME
```
Bring the pods back up (assuming a single replica is desired):
```sh
kubectl scale --current-replicas=0 --replicas=1 deployment/$DEPLOYMENT_NAME
```

### Bring configmap data into your environment
_*FIRST*_ make sure there's nothing untoward in the configmap:
```sh
CONFIGMAPNAME="whatever"
kubectl get -o json configmap "$CONFIGMAPNAME"
```
Load to your env:
```sh
eval $(kubectl get -o json configmap "$CONFIGMAPNAME" | jq -r '.data | to_entries | .[] | .key + "=\"" + .value + "\""')
```

### Create a job from a cronjob
```bash
kubectl create job --from=cronjob/<cronjob-name> <job-name>
```


### Print all values of a given secret base64 decoded
```bash
kubectl get secrets <secret-name> -o json | jq '.data | map_values(@base64d)'
```

### View log files for crashed pod
```bash
kubectl logs <podname> -p # or --previous
```

### Nodes
```sh
kubectl get nodes
```

#### Apply node role
```sh
kubectl label node $node kubernetes.io/role=$role
```
I.e. to set the worker role to a node worker-0:
```sh
kubectl label node worker-0 kubernetes.io/role=worker
```

### Wait for pod ready
Note: this will wait for 300 seconds for _each resource_. This means for ten pods that are failing,
the following request will wait for 3000s. See timeout below.
```sh
kubectl wait --for=condition=Ready pod --all --timeout=300s
kubectl wait --for=condition=Available deploy --all --timeout=300s
```
```sh
timeout 300 kubectl wait --for=condition=Ready pod --all --timeout=300s
```

### Force delete
```sh
kubectl delete pod mysql-abcde --force --grace-period=0
```

### Delete all persistent volumes, persistent volume claims
```sh
kubectl delete -A pv,pvc --all
```

### Get data from the cluster into your shell environment
"Import" a configmap's data to your shell env:
```sh
eval $(kcg configmaps -o json | jq -r '.items[] | select(.metadata.name | test("^oauth-app-credentials-")).data | to_entries[] | "export \(.key)=\"\(.value)\""')
```
Note that this searches configmaps for the desired one- this will correctly handle multiple result
configmaps. Remove everything starting at `.data` and the `eval` to test the configmaps you'll
"import".

Duplicate a pod's environment locally. WARNING: this will include things like `PATH`:
```sh
eval $(kubectl exec deploy/portal-backend -- env)
```

### Delete some stuff that sometimes causes trouble with some k8s distributions
```sh
kubectl delete --all -A pvc,pv
# get rid of finalizers to allow pvc to be removed
kubectl patch pvc mysql -p '{"metadata":{"finalizers": []}}' --type=merge
kubectl delete --all -A clusterrole{,binding}
kubectl wait --for=delete clusterrole{,binding}
```

### Print all container images in a cluster
Note: this doesn't print init containers. It's probably possible to concatenate
`.spec.template.spec.initContainers[]` with `.containers[]`.
```sh
kcg deploy -n mojaloop -o json | jq '.items[] | .spec.template.spec.containers[] | .image' | sort | uniq
```

### More
https://kubernetes.io/docs/reference/kubectl/cheatsheet/
