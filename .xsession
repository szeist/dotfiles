# .xinitrc

xrdb $HOME/.Xresources

# Reset screen resolution
xrandr -s 0

# Set up screen resolution
xrandr --dpi  96x96

# Set up up external monitor (if connected)
if [ $(xrandr -q | grep "^VGA1\s\+connected" | wc -l) -eq 1 ]; then
    xrandr  --output VGA1 --right-of eDP1 --auto;
fi

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
