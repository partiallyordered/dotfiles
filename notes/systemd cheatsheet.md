#### Programmatic access to systemd
The systemd developers recommend using dbus for programmatic access to systemd. Discussion:
https://github.com/systemd/systemd/issues/83

See what methods are available:
```sh
nix-shell -p glib.bin --command 'gdbus introspect --system --dest org.freedesktop.systemd1 --object-path /org/freedesktop/systemd1'
```

List units:
```sh
dbus-send --system --print-reply --dest=org.freedesktop.systemd1 /org/freedesktop/systemd1 org.freedesktop.systemd1.Manager.ListUnits
```
Get a unit:
```
dbus-send --system --print-reply --dest=org.freedesktop.systemd1 /org/freedesktop/systemd1 org.freedesktop.systemd1.Manager.GetUnit string:kanata-xps.service
```
The dbus unit name will be returned by the above call. It looks like this is predictable, in this
case `kanata_2dxps_2eservice`. We can introspect it:
```sh
gdbus introspect --system --dest org.freedesktop.systemd1 --object-path /org/freedesktop/systemd1/unit/kanata_2dxps_2eservice
```
Note that in the output we see that this object implements some interfaces, as listed, and has
properties corresponding to these interfaces. List its properties for interface
'org.freedesktop.systemd1.Unit' (these are also visible in the preceding introspect call):
```sh
dbus-send --system --print-reply  --dest=org.freedesktop.systemd1 /org/freedesktop/systemd1/unit/kanata_2dxps_2eservice org.freedesktop.DBus.Properties.GetAll string:org.freedesktop.systemd1.Unit
```
In this example, the property we're interested in is `ActiveState`. Retrieve:
```
dbus-send --system --print-reply --dest=org.freedesktop.systemd1 /org/freedesktop/systemd1/unit/kanata_2dxps_2eservice org.freedesktop.DBus.Properties.Get string:org.freedesktop.systemd1.Unit string:ActiveState
```
See property documentation here: https://www.freedesktop.org/wiki/Software/systemd/dbus/

Documentation: https://www.freedesktop.org/wiki/Software/systemd/dbus/
