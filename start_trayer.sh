#!/bin/sh
#
# Simply starts trayer for us with all our desired options
#
# When running in the xmonad testing vm, using --transparent false (default)
# prevented --tint from working. But setting transparent to true did. So to
# ensure that the proper color was being displayed, set transparent to true,
# but make alpha 0 so that only the tint color shows through.
#
# The height is determined to match xmobar's height. And xmobar's height is
# based on the font used. So we'll need to change the height whenever we change
# the font being used.

trayer --edge bottom --align right --SetDockType true --SetPartialStrut true \
	--expand true --width 10 --heighttype pixel --height 17 \
	--transparent true --alpha 0 --tint 0x285577 &

