# source x init scripts
if [ -d /etc/X11/xinit/xinitrc.d ]; then
    for f in /etc/X11/xinit/xinitrc.d/*; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi
# Set up gnome-keyring
eval $(/usr/bin/gnome-keyring-daemon --start --components=gpg,pkcs11,secrets,ssh)
export GPG_AGENT_INFO SSH_AUTH_SOCK
# start pulseaudio
start-pulseaudio-x11
# set up terminal colours etc.
xrdb -merge ~/.Xresources
# set the cursor
xsetroot -cursor_name left_ptr
# set keyboard layout
setxkbmap -layout gb
# set up xscreensaver to load when the system comes out of suspend
xset s 120 # 120 seconds of inactivity before screensaver
xscreensaver -no-splash &
xss-lock -- xscreensaver-command -lock &
xscreensaver-command -watch | /home/msk/bin/unlock &
# Start xcompmgr for transparency
compton -b
# Share the selection and C-C C-X clipboards
autocutsel -fork &
autocutsel -selection PRIMARY -fork &
# Load wallpaper
if [ -f ~/.cache/himawaripy/latest.png ]; then
    feh --bg-max ~/.cache/himawaripy/himawari-*.png
fi
# start xmonad
exec xmonad
