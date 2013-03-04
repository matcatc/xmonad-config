#!/bin/sh
#
# Simply starts trayer for us with all our desired options
#
# When running in the xmonad testing vm, using --transparent false (default)
# prevented --tint from working. But setting transparent to true did. So to
# ensure that the proper color was being displayed, set transparent to true,
# but make alpha 0 so that only the tint color shows through.


trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 10 --height 19 --transparent true --alpha 0 --tint 0x285577 &

