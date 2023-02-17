```sh
nix-shell -p python311Packages.subliminal
subliminal download -l en $MEDIA_FILE_NAME
# Or, with opensubtitles credentials (see password manager)
subliminal --opensubtitles $USERNAME $PASSWORD download -l en $MEDIA_FILE_NAME
```
