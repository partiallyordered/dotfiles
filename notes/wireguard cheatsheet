#### Get config from running VPN/interface
```sh
wg showconf <interface>
```

The above doesn't produce DNS config- the current DNS server can be obtained from the _SERVER_
section of a drill/dig query.
```sh
$ drill ifconfig.co | grep SERVER
;; SERVER: 10.25.0.2
```
Put this in the `[Interface]` section:
```ini
[Interface]
DNS = 10.25.0.2
...
```

It also won't contain an `Address` field. This should be visible on the interface:
```sh
$ ip a
182: <interface-name>: <POINTOPOINT,NOARP,UP,LOWER_UP> mtu 1420 qdisc noqueue state UNKNOWN group default qlen 1000
    link/none
    inet 192.168.100.21/32 scope global interface-name
       valid_lft forever preferred_lft forever
```
The relevant address is `192.168.100.21/32`. Put this in the `[Interface]` section:
```ini
[Interface]
Address = 192.168.100.21/32
...
```
