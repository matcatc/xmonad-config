#!/bin/bash
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
# Matthew Todd

# From timeout's man page
# exit code when the timeout timesout.
TERM_TIMEOUT=124
KILL_TIMEOUT=137


# Should return almost immediately, so use timeout to limit it from going beserk
status=$(timeout 2s claws-mail --status )
exit_code=$?

# If it times out, then won't have any output
if [[ (( "$exit_code" -eq "$TERM_TIMEOUT" )) || (( "$exit_code" -eq "$KILL_TIMEOUT" )) ]]
then
    echo -n 'Timeout'
    exit 1
fi

# the below comparison is what claws-mail prints out when its not
# running. Using this rather than grepping ps output b/c that'll fail
# if it catches a previous grep. Ie: this script was previously
# interfering with itself when the `claws-mail --status` process was
# found. Plus this does one less subprocess call.
if [ "$status" == '0 Claws Mail not running.' ]
then
    echo -n 'N/A'
else
    echo -n "$status"
fi

exit 0

