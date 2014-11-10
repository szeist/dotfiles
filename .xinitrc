# .xinitrc

xrdb $HOME/.Xresources

# Reset screen resolution
xrandr -s 0

# Set up external display (laptop only)
if [ $(hostname) == 'szena-laptop' ]; then
    xrandr  --output VGA1 --right-of eDP1 --auto;
fi

# Set arrow cursor
xsetroot -cursor_name left_ptr

# Default background color
xsetroot -solid '#80a0af'
if [ -e ~/.Xbackground.png ]; then
    xloadimage -onroot -fullscreen ~/.Xbackground.png;
fi

# Turn off console beep
xset b off

# Set typematic rate and delay
xset r rate 200 30

# Set keyboard layout
setxkbmap -layout hu

# Load key mappings
xmodmap ~./.Xmodmap

# Load key bindings
xbindkeys

# Start XMonad
xmonad
