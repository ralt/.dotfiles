#!/bin/bash
 
# Load resources
 
xrdb -merge .Xresources
 
# Set up an icon tray
 
trayer --edge top --align right --SetDockType true --SetPartialStrut true \
 --expand true --width 10 --transparent false --tint 0x191970 --height 12 &
 
# Set the background color<
 
 
# Fire up apps
~/local/bin/sb
xscreensaver -no-splash &
urxvtd -q -o -f &
thunar --daemon &
bluetooth-applet &
nm-applet --sm-disable &
gnome-sound-applet &
 
exec xmonad
