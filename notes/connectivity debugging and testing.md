
### Hardware
What exists?
```sh
lspci -k
```
Look for "network controller".
```
3b:00.0 Network controller: Qualcomm Atheros QCA6174 802.11ac Wireless Network Adapter (rev 32)
	Subsystem: Rivet Networks Killer Wireless-n/a/ac 1535 Wireless Network Adapter
	Flags: bus master, fast devsel, latency 0, IRQ 16
	Memory at ed200000 (64-bit, non-prefetchable) [size=2M]
	Capabilities: <access denied>
	Kernel driver in use: ath10k_pci
	Kernel modules: ath10k_pci
```
Note the bus address (I guess this is what it is..?), in this case `3b:00.0`. Remove the device and
rescan:
```
sudo su
echo "1" > /sys/bus/pci/devices/0000:3b:00.0/remove
echo "1" > /sys/bus/pci/rescan
```
More info on PCI bus control here: https://unix.stackexchange.com/questions/73908/how-to-reset-cycle-power-to-a-pcie-device/245184#245184

Now check the kernel module loaded for the device. See if the module is loaded, and whether there
are any users of the module:
```sh
lsmod | grep ath10k
```
Result (column 2 is "used by"):
```
ath10k_pci             45056  0
ath10k_core           409600  1 ath10k_pci
ath                    32768  1 ath10k_core
mac80211              847872  1 ath10k_core
cfg80211              868352  3 ath,mac80211,ath10k_core
```

What's failed?
```sh
# ath10k is the name of the kernel module from the previous steps
dmesg | grep ath10k
```
Reload:
```sh
sudo modprobe -r ath10k_pci
sudo modprobe ath10k_pci
```
```sh
systemctl --state=failed
```
What's running?
```sh
systemctl status
```
Look for `wpa_supplicant` or `network` related services.

### Restart interfaces
First, list interfaces
```sh
ip a
```
Restart the interface by setting it down, then up:
```sh
sudo ip link set wlp1s0 down
sudo ip link set wlp1s0 up
```

### Allow via rfkill
If you see a message like
> Operation not possible due to RF-kill

Run:
```sh
sudo rfkill unblock all
```

### Routing table
Is traffic on the current machine being routed somewhere odd?
```sh
route
```

### ICMP (ping)
```sh
ping google.com # or whatever you're trying to access
```


### DNS
In the following sections, drill can normally be replaced with dig with no change of syntax. Note
that dig and drill _do not_ check `/etc/hosts` before resolving DNS, but curl _does_. If there
appears to be a discrepancy in resolution behaviour between the two, check `/etc/hosts`.

#### Try resolving google.com and cloudflare.com via the services respective DNS services.
Look in the ANSWER section. If there are no results, the server couldn't resolve the name.
```sh
drill google.com @8.8.8.8
drill cloudflare.com @1.1.1.1
```

#### What's in /etc/resolv.conf?
Note that sometimes `/etc/resolv.conf` contains some additional resolution information. It's worth
inspecting the file.
Try
```sh
drill google.com @<nameserver-from-resolv-conf>
```

### Trace
```sh
tracepath $IP
```
or, root required:
```sh
sudo traceroute $IP
```


### TCP/UDP
#### Test TCP connectivity
```sh
telnet google.com 80 # http
telnet google.com 443 # https
telnet github.com 22 # ssh (normally)
nc -vz google.com 80
nc -vz google.com 443
# Systems without nc, telnet, but with reasonably modern bash:
# From: https://stackoverflow.com/a/19866239
bash -c "cat < /dev/null > /dev/tcp/$hostname/$port"
bash -c "cat < /dev/null > /dev/tcp/google.com/80"
# With a timeout for failure
timeout 1 bash -c "cat < /dev/null > /dev/tcp/$hostname/$port"
```

### TLS
#### openssl s_client
Example:
```sh
openssl s_client -connect google.com:443
```
Some useful args:
```sh
openssl s_client -connect $hostname:443 -cert ./clientcert.pem -key ./clientkey.pem -CAfile ./pathfinderchain.pem
```
If you have a CA cert and some intermediate certs you can just use `cat` to combine them:
```sh
cat ./cacert.pem ./intermediate.pem ./anotherintermediate.pem > chain.pem
```
Manual
```sh
man openssl-s_client
```

#### Mutual TLS
From: https://asciinema.org/a/173370
Server:
```sh
openssl s_server -accept 8443 \
    -CAfile ca.crt.pem \
    -cert server.crt.pem \
    -key server.key.pem \
    -Verify 10 -tls1_2 -state -quiet
```
Client:
```sh
openssl s_client -connect 127.0.0.1:8443 \
    -CAfile ca.crt.pem \
    -cert client.crt.pem \
    -key client.key.pem \
    -tls1_2 -state -quiet
```


### HTTP
#### Test TCP connectivity
```sh
telnet google.com 80 # http
telnet google.com 443 # https
nc -vz google.com 80
nc -vz google.com 443
```

#### Curl tricks
##### Spoof a hostname with curl
From (much more info): https://daniel.haxx.se/blog/2018/04/05/curl-another-host/
```sh
curl --resolve example.com:8080:127.0.0.1 example.com:8080/healthz
```
Also (from the manual):
```sh
curl --connect-to example.com:443:example.net:8443 https://example.com
```

#### Test a route
Note that dig and drill _do not_ check `/etc/hosts` before resolving DNS, but curl _does_. If there
appears to be a discrepancy in resolution behaviour between the two, check `/etc/hosts`.
```sh
# GET /
curl -i -w '\n' service.com
# PUT json in a local file to /users
curl service.com/users \
    -X PUT \
    --data @./local-data-file.json \
    -H 'accept: application/json' \
    -H 'content-type: application/json'
# PUT inline json in a local file to /users
curl service.com/users \
    -X PUT \
    --data '{ "name": "Jimbob" }' \
    -H 'accept: application/json' \
    -H 'content-type: application/json'
```

### Alpine Notes
Install telnet, dig:
```sh
apk update
apk add busybox-extras # telnet on Alpine 3.7+
apk add bind-tools # dig
apk add drill
```
