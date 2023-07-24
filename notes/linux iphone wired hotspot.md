
### General Instructions

1. Plug the phone in to the computer
2. Install libimobiledevice and set up the usbmuxd service
3. `idevicepair pair` and follow the phone's on-screen instructions
4. Turn on _Personal Hotspot_ and reconnect the device to the computer

### Troubleshooting

It's good to check the status of usbmuxd.service; it might sometimes need restarting.

### Mounting the phone's disk
```sh
ifuse $mount_directory
# see man ifuse for help
```

#### Mounting an app's documents folder
The following example mounts the documents folder of the VLC app to /mnt:
```sh
ifuse --documents org.videolan.vlc-ios /mnt
```

### References

https://wiki.archlinux.org/title/IPhone_tethering
https://wiki.archlinux.org/title/IOS
https://nixos.wiki/wiki/IOS
