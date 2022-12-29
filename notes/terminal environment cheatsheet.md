
From: https://stackoverflow.com/a/30969768

`-o allexport` enables all following variable definitions to be exported. +o allexport disables
this feature. `set -a` and `set +a` are equivalent to `set -o allexport` and `set +o allexport`.

```sh
set -a
source ./.env
set +a
```
