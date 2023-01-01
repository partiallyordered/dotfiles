
## jsonlines/ndjson

### Delimit pseudo jsonlines/ndjson records
Given a log that contains pretty-printed json with some errors, we can add some delimiters and
use `--seq` to instruct `jq` to ignore failed parses:
```sh
kubectl log -f deploy/some-application | sed 's/^{/'`echo "\036"`'{/' | jq --seq '.'
```
See `--seq` in the `jq` manual for more.

### Filter out k8s health checks for structured logs
```sh
kubectl logs deploy/my-deployment | jq 'select(.ctx.request.path != "/healthz")'
```

## General

### Return a non-zero exit code
```sh
echo '{ "k": "v" }' | jq -e '.k == "v"'
```

From: https://stedolan.github.io/jq/manual/
```
    -e / --exit-status:
```
Sets the exit status of jq to 0 if the last output values was neither false nor null, 1 if the last
output value was either false or null, or 4 if no valid result was ever produced. Normally jq exits
with 2 if there was any usage problem or system error, 3 if there was a jq program compile error,
or 0 if the jq program ran.

### Filter an array on content
```sh
jq '.path.to.array[] | select(.property == "value-of-interest")'
jq '.path.to.array[] | select((.property == "value-of-interest") and (.otherProp == "other-val-of-interest"))'
# Check whether .property is ValueOne, ValueTwo, or ValueThree
jq '.path.to.array[] | select(.property | index("ValueOne", "ValueTwo", "ValueThree"))'
```

If you get a message like
> Cannot iterate over null
try a `?` after `[]`.
```sh
jq '.path.to.array[]? ... etc ...'
```

### Filter an object based on content
Object like:
```json
{
  "auths": {
    "ghcr.io": {
      "auth": "blah"
    },
    "https://index.docker.io/v1/": {
      "auth": "whatever"
    }
  }
}
```
```sh
jq '.auths | with_entries(select(.key == "ghcr.io"))' ~/.docker/config.json
```
### String interpolation
```sh
echo '{ "a": "hello" }' | jq '"\(.a) world"'
```
More complex, bring a mysql root password into the environment for usage with the CLI:
```sh
eval $(kubectl get secrets mysql -o json | jq -r '.data | "export MYSQL_PWD=\(map_values(@base64d)."mysql-root-password")"')
```

### Split a string
```sh
echo '"hello world"' | jq '. | split(" ")'
echo '"hello world"' | jq '. | split(" ")[0]'
```

### Modify an object based on content
Object like:
```json
{
  "auths": {
    "ghcr.io": {
      "auth": "blah"
    },
    "https://index.docker.io/v1/": {
      "auth": "whatever"
    }
  }
}
```
```sh
jq '.auths | with_entries(select(.key == "ghcr.io"))' ~/.docker/config.json
```
Or
```sh
jq '.auths |= { "ghcr.io": ."ghcr.io" }' ~/.docker/config.json > .dockerconfigjson
```

### Filter an array on regex
```sh
jq '.path.to.array[] | select(.property | test(".*nginx.*"))'
```

### Update a value in an array based on content
```sh
jq '(.path.to.array[] | select(.property == "value-of-interest") | .propertyToUpdate) |= "hello"'
```
See also: https://stedolan.github.io/jq/manual/#Assignment

### Load key-value pairs to your env:
```sh
eval $(echo '{ "key": "value" }' | jq -r '. | to_entries | .[] | .key + "=\"" + .value + "\""')
```

### Base64 decode values
```sh
echo '{ "data": "$base64encoded value" }' | jq '.data | @base64d'
```

### Map over an object
```sh
echo '{ "data": { "key": "$base64encoded value" } }' | jq '.data | map_values(@base64d)'
```

### Examples
Filters a list for Kubernetes Ingress resources, filters their rules and interpolates content in a
string to output `"$host/path service:port"`.
```sh
kubectl get ingress -o json | jq -r '.items[] | select(.kind == "Ingress") | .spec.rules[] | "\(.host)\(.http.paths[0].path) \(.http.paths[0].backend.serviceName):\(.http.paths[0].backend.servicePort)"' | sort | column -t
```

Delete a range of resources in default namespace.
```sh
kubectl delete $(kubectl get all -n default -o json | jq '.items[] | "\(.kind)/\(.metadata.name)"' -r)
```

### In-place sort on object keys
Where the data has a key called `values` containing the array we're interested in, and each element
of said array has a `.key` property:
```sh
jq '.values|=sort_by(.key)' data.json
```

### Extract keys
```sh
jq '.path.to.object.of.interest | keys' data.json
```

### Walk tree
```sh
jq 'walk(if type == "object" and has("someProperty") then del(.someProperty) else . end)'
```

### Put an object in your environment
Note: these examples with `export` in your environment, as that's often the desired behaviour, but
this has the effect that subprocesses started in that shell will inherit these vars. If that's not
what you want, remove the "export" from these commands:

Roughly `export USER="JIM" PASSWORD="HORSE"`:
```sh
eval $(echo '{ "USER": "JIM", "PASSWORD": "HORSE" }' | jq -r '. | to_entries[] | "export \(.key)=\"\(.value)\""')
```
In this example, "import" a k8s configmap into your env:
```sh
eval $(kcg configmaps -o json | jq -r '.items[] | select(.metadata.name | test("^oauth-app-credentials-")).data | to_entries[] | "export \(.key)=\"\(.value)\""')
```
Note that this searches configmaps for the desired one- this will correctly handle multiple result
configmaps. Remove everything starting at `.data` and the `eval` to test the configmaps you'll
"import".

## References
- Search `man jq` for
  - `^BASIC FILTERS`
  - `^TYPES AND VALUES`
  - `^BUILTIN OPERATORS AND FUNCTIONS`