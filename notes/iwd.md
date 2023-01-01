
Check the device isn't blocked:
```sh
rfkill
```
If it is, unblock it:
```sh
sudo rfkill unblock wlan
```

List devices:
```sh
iwctl device list
```

Scan:
```sh
iwctl station <device> scan
```

See scan result:
```sh
iwctl station wlan0 get-networks
```

Inspect:
```sh
iwctl station <device> show
```

Connect:
```sh
iwctl station <device> connect <network-name>
```

Power cycle:
```sh
iwctl adapter list
iwctl adapter phy0 set-property Powered off
iwctl adapter phy0 set-property Powered on
```

Short reset procedure:
```sh
sudo systemctl restart iwd.service
rfkill
sudo rfkill unblock wlan
iwctl station wlan0 get-networks
iwctl station wlan0 connect <network-name>
```

Reset procedure
```sh
iwctl adapter phy0 set-property Powered off
sudo systemctl stop iwd.service
sudo systemctl start iwd.service
iwctl adapter phy0 set-property Powered on
iwctl device wlan0 set-property Powered on
rfkill
sudo rfkill unblock wlan
iwctl station wlan0 show
iwctl station wlan0 scan
iwctl station wlan0 get-networks
iwctl station wlan0 connect <network-name>
```