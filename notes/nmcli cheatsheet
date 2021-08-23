### Enable/disable wifi
```sh
nmcli radio wifi on
```

```sh
nmcli radio wifi off
```

### List networks
```sh
nmcli dev wifi list
```

### Connect
```sh
nmcli dev wifi connect somenetwork password somepassword
```

### Use
```sh
nmcli con up id somenetwork
```

### Set connection priority
```sh
nmcli con mod fast-connection ipv4.route-metric 20 # This connection is preferred
nmcli con mod slow-connection ipv4.route-metric 40
nmcli con mod fast-connection ipv6.route-metric 20 # This connection is preferred
nmcli con mod slow-connection ipv6.route-metric 40
nmcli con down fast-connection
nmcli con down slow-connection
nmcli con up fast-connection
nmcli con up slow-connection
```
