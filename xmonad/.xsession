#!/bin/bash
 
# Fire up apps
~/local/bin/sb
thunar --daemon &
bluetooth-applet &
nm-applet --sm-disable &
gnome-sound-applet &
 
exec awesome
