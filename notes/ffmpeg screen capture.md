
Record with:
```sh
nix-shell -p simplescreenrecorder
```
Then re-encode for sharing with:
```sh
ffmpeg -i input.mkv -c:v libx264 -c:a aac output.mp4
```

https://wiki.archlinux.org/index.php/FFmpeg#Screen_capture
Note that these examples all assume an HD 16:9 viewport. To record an entire viewport with a
different resolution, you'll need to actually supply that viewport's resolution. For example, for
a 4K 16:9 viewport, the required resolution would be 3840x2160. Moreover, the resolution could be
affected by randr scaling.

To get the correct size, check xrandr:
```sh
$ xrandr
Screen 0: minimum 320 x 200, current 3840 x 2160, maximum 16384 x 16384
```
The important value here is _current 3840 x 2160_. Scaling will affect this size if using xrandr
for example.

A further note: when recording, take a very short capture first to check all parameters are
correct, instead of recording the whole video then finding out the configuration was wildly wrong.

To take a screenshot screen.png:
```bash
ffmpeg -f x11grab -video_size 1920x1080 -i $DISPLAY -vframes 1 screen.png
```
where -video_size specifies the size of the area to capture.

Without audio, lossless encoding:
```bash
ffmpeg -f x11grab -video_size 1920x1080 -framerate 25 -i $DISPLAY -c:v ffvhuff screen.mkv
```

With audio, lossy encoding:
```bash
ffmpeg -f x11grab -video_size 1920x1080 -framerate 25 -i $DISPLAY -f alsa -i default -c:v libx264 -preset ultrafast -c:a aac screen.mp4
```

Record the RHS monitor from the LHS monitor. Notice the x,y offset after `$DISPLAY`:
```sh
ffmpeg -f x11grab -video_size 1920x1080 -framerate 25 -i "$DISPLAY.0+1920,0" -f alsa -i default -c:v libx264 -preset ultrafast -c:a aac screen.mp4
```