#!/bin/bash

# Starting Display compositors
picom --experimental-backend &
# For xcompmgr display compositor uncomment line below
# xcompmgr &

# Starting xmobar
# xmobar $HOME/.config/xmobar/xmobarrc &

# Starting lxsession
lxsession &

# Starting Dunst for nitifications
dunst &

# Restoring the wallpaper
nitrogen --restore &

# Setting DPI manually, it's wrong after last update
xrandr --dpi 96 &


# Setting keyboard layouts with variants
setxkbmap -layout us,am -variant ,phonetic &