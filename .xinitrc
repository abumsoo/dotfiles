#!/bin/bash

#~/.xinitrc

mpd

# Load X resources
xrdb -load ~/.Xresources

#custom keys
setxkbmap -option ctrl:swapcaps

feh --bg-scale '/home/bumshakabum/Pictures/doitnow.png'

exec i3
