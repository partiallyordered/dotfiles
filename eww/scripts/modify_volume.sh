#!/usr/bin/env sh

if [ "$1" = "up" ]; then
    exec wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 1%+
elif [ "$1" = "down" ]; then
    exec wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 1%-
fi


