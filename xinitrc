#!/bin/sh
#
# This is a startup script for xmonad and related stuff.
# Should be changed when moving xmonad to my actual laptop, so as to support
# multiple wms. Ie: check what wm is being started and do stuff that's
# particular to that one.
# 

# xinitrc
#

# from arch wiki
#xsetroot -cursor_name left_ptr

# from xmonad begginger's guide
xrdb -merge .Xresources

# fix the keys (specifically Return)
xmodmap ~/.Xmodmap

# background color so we know xmonad is running on start
xsetroot -solid midnightblue

# start trayer
~/.xmonad/start_trayer.sh

# start xmonad
xmonad

