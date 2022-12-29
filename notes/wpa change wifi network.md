List networks:
```sh
wpa_cli list_networks
```
Change network:
```sh
wpa_cli select_network 0
```
Or, to specify the interface:
```sh
wpa_cli -g /run/wpa_supplicant/wlp59s0 select_network 0
```
See also: https://nixos.wiki/wiki/Wpa_supplicant
