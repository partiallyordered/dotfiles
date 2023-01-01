
# Helm template functions
https://masterminds.github.io/sprig/

# Generate k8s object representation in yaml from a chart, including release name
```sh
helm template --name $RELEASE_NAME .
# Only a specific file:
helm template -x ./templates/configmap.yaml --name $RELEASE_NAME .
# Supply your own values file:
helm template -f /path/to/values.yaml --name $RELEASE_NAME .
```

# Install/update the k8s objects in-place
```sh
helm template --name $RELEASE_NAME . | kubectl apply -f -
```

# Get list of available helm charts from added repos
```sh
# show all charts
helm search
# show all charts from stable repository
helm search stable/
```

# Download a chart
```sh
helm fetch $REPONAME/$CHARTNAME
# or, if you haven't aliased the repo locally with `helm repo add`
helm fetch --repo=$REPOURL $CHARTNAME
# e.g.
helm fetch percona-xtradb-cluster --repo=https://kubernetes-charts.storage.googleapis.com
```

# Install Tiller
```sh
helm init
# Wait until Tiller pod is ready
kubectl -n kube-system wait --for=condition=Ready pod -l name=tiller --timeout=300s
# Set up Tiller cluster role
# WARNING: This gives Tiller the role cluster-admin (like root for the cluster). Do NOT do this in production.
kubectl create serviceaccount -n kube-system tiller
kubectl create clusterrolebinding tiller-cluster-admin --clusterrole=cluster-admin --serviceaccount=kube-system:tiller
kubectl --namespace kube-system patch deploy tiller-deploy -p '{"spec":{"template":{"spec":{"serviceAccount":"tiller"}}}}'
```

# Upgrade/downgrade Tiller
```sh
helm init --upgrade
# or
helm init --force-upgrade
```

# Uninstall Tiller
```sh
helm reset
# or
helm reset --force
```

# An example of cleaning up a cluster that doesn't go down easily
```sh
kubectl delete --all -A pvc,pv

kubectl delete ns casa-backend fxp ext-svcs

kubectl delete clusterrole{,binding}/local-test-ingress

kubectl wait --for=delete \
    namespace/casa-backend \
    namespace/fxp \
    namespace/ext-svcs \
    clusterrole{,binding}/local-test-ingress

kubectl create ns casa-backend
kubectl create ns fxp
```

# Fix a manually modified helm deployment
```sh
# the output of helm status should show you that something is awry
helm status
# inspect the release name and revision of interest
helm list
# now "roll back" $release to $revision
helm rollback $release $revision
```