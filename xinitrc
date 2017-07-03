#!/bin/zsh

#~/.xinitrc

# # start some nice programs

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

# Load up Xresources
xrdb -load ~/.Xresources

# Dual monitor setup
xrandr --output DVI-0 --right-of DVI-1

# Start background
WALLPATH=/home/bumshakabum/Pictures/wallpapers/minimal
feh --bg-scale "$(find ${WALLPATH} -type f|shuf|head -n 1)" --bg-scale "$(find ${WALLPATH}|shuf|head -n 1)"

# Autostart Dropbox
dropbox start

# Japanese input
export GTK_IM_MODULE='uim'
export QT_IM_MODULE='uim'
uim-xim &
export XMODIFIERS='@im=uim'

# Turn off screensaver, dimmer, etc.
xset s off
xset -dpms
xset s noblank

# Set keyboard layout to dvorak and switch caps and control
setxkbmap -layout dvorak -option ctrl:swapcaps

exec i3
