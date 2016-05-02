#!/bin/sh

export PATH="$PATH:$HOME/bin:$HOME/.cabal/bin/"

xrdb $HOME/.Xresources

# Reset screen resolution
xrandr -s 0

# Set up screen resolution
xrandr --dpi  96x96

# Set arrow cursor
xsetroot -cursor_name left_ptr

# Default background color
xsetroot -solid '#80a0af'

# Turn off console beep
xset b off

# Set typematic rate and delay
xset r rate 200 30

# Set keyboard layout
setxkbmap -layout en_US

# Load key mappings
xmodmap .Xmodmap

# Start XMonad
exec dbus-launch --exit-with-session --sh-syntax xmonad
