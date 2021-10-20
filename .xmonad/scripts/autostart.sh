#!/bin/bash

# Starting Display compositors
picom --experimental-backend &
# For xcompmgr display compositor uncomment line below
# xcompmgr &

# Starting xmobar
# xmobar $HOME/.config/xmobar/xmobarrc &

# Starting Dunst for nitifications
dunst &

# Restoring the wallpaper
nitrogen --restore