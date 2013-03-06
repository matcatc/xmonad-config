#!/bin/sh
#
# This is a startup script for xmonad and related stuff.
# Should be changed when moving xmonad to my actual laptop, so as to support
# multiple wms. Ie: check what wm is being started and do stuff that's
# particular to that one.
# 

# xinitrc
#
# from xmonad begginger's guide
xrdb -merge .Xresources

# from arch wiki/xmonad documentation
# http://www.haskell.org/haskellwiki/Xmonad/Frequently_asked_question#Setting_the_X_cursor
xsetroot -cursor_name left_ptr

# fix the keys (specifically Return)
xmodmap ~/.Xmodmap

# background color so we know xmonad is running on start
xsetroot -solid midnightblue

# start trayer
~/.xmonad/start_trayer.sh

# start xmonad
xmonad

