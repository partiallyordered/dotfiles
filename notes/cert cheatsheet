
### Single self-signed cert + key
```sh
openssl req -nodes -x509 -newkey rsa:4096 -keyout key.pem -out cert.pem -days 365 -subj '/CN=payment-mgr-dev'
```

### Decode cert
```sh
openssl x509 -in cert.pem -text
```

### Create a CA and self-signed cert
Adapted from https://superuser.com/a/126165
```sh
#!/usr/bin/env bash
# requires bash features: process substitution
set -euxo pipefail
EXTENSIONS="basicConstraints=critical,CA:TRUE,pathlen:0"
CA_SUBJ='/CN=my-ca'
CLIENT_SUBJ='/CN=my-client'
KEY_SIZE="4096"
CA_EXPIRY_DAYS=3650
CLIENT_EXPIRY_DAYS=365
# 1. Create a Certificate Authority private key
openssl req  -extensions v3_req -addext "$EXTENSIONS" -nodes -new -newkey "rsa:$KEY_SIZE" -out ca.csr \
    -keyout ca.key -subj "$CA_SUBJ"

# 2. Create your CA self-signed certificate:
openssl x509 -trustout -signkey ca.key -days "$CA_EXPIRY_DAYS" -req -in ca.csr -out ca.pem \
    -extfile <(echo "$EXTENSIONS")

# 3. Issue a client certificate by first generating the key, then certificate signing request (or use
#    one provided by an external system):
openssl genrsa -out client.key "$KEY_SIZE"
openssl req -new -key client.key -out client.csr -subj "$CLIENT_SUBJ"

# 4. Sign the certificate using private key of your CA. Note that this creates a `ca.srl` file to
#    keep track of the cert serial.
openssl x509 -req -days "$CLIENT_EXPIRY_DAYS" -in client.csr -CA ca.pem -CAcreateserial -CAkey ca.key \
    -out client.pem
```
