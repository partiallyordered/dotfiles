#!/usr/bin/env sh

PROP="_XMONAD_WORKSPACE_LOG"

exec \
    stdbuf -oL xprop -notype -root -spy -f "$PROP" 8t '$0\n' "$PROP" | \
            stdbuf -oL sed -e "s/\(^$PROP\"\|\"$\)//g"
