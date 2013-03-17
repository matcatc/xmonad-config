#!/usr/bin/python3
'''
Output the volume level for use in the status bar.

Originally for wmii's status bar, this version has been modified to work with
xmobar.

@note
assumes pulse audio w/ alsa backend. B/c pacmd doesn't work w/ pulseaudio
--system, we have to hack some stuff together. This uses amixer to get the data
(hence the assumption of the alsa backend.

@par
assumes only one output (sink) line

@license
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

@note
TODO: add logging?

@date Jun 14, 2011
@author Matthew Todd
'''
import subprocess
import re

NUM_SEGMENTS = 12

# characters for output
# on is part of the bar that is full, off is the part that is empty (think
# progress bar)
#
# output looks and works best when the font is fixed size
ON_CHAR = '-'
OFF_CHAR = ' '

#
## Exception defs
#
class CustException (Exception):
    '''
    Custom exception

    @date Jun 17, 2011
    @author Matthew Todd
    '''
    def __init__(self, description, exception):
        '''
        @param description String describing what went wrong.
        @param exception Exception the exception that was thrown and caught.
        @date Jun 17, 2011
        @author Matthew Todd
        '''
        self.description = description
        self.exception = exception

    def __repr__(self):
        '''
        '''
        return "%s(description=%r, exception=%r)" % (self.__class__, self.description, self.exception)

    def __str__(self):
        '''
        '''
        return "Exception: %s\n%s" % (self.description, self.exception)

#
## Function defs
#

def get_data():
    try:
        ret = subprocess.check_output(['amixer'])
    except (subprocess.CalledProcessError, OSError) as e:
        raise CustException('amixer failed', e)

    for line in ret.decode().split('\n'):
        if re.search(r'Playback', line):
            for col in line.split():
                if re.search(r'%', col):
                    volume = int(col.strip('[]%'))
                elif re.search(r"off", col):
                    mute = True
                elif re.search(r"on", col):
                    mute = False

#    print("DEBUG: volume = %s, mute = %s" % (volume, mute))

    return (volume, mute)

def generate_output(vol, is_mute):
    '''
    If user has provided a custom SEGMENT_WIDTH, then it is used. Else it is
    computed from other predefined information.

    @param Number vol volume percentage w/ no decimal place (possbile to be > 100%)
    @param Bool is_mute True if output muted
    @return String the output to be printed
    @date Jun 14, 2011
    @author Matthew Todd
    '''
    if is_mute:
        return "Vol: muted"
    else:
        vol_perc = min(vol, 100) / 100
        num_on = min(int(vol_perc * NUM_SEGMENTS), NUM_SEGMENTS)

#        print("DEBUG: num_on = %s" % num_on)

        return ON_CHAR*num_on + OFF_CHAR*(NUM_SEGMENTS-num_on)

def print_output(output):
    '''
    B/c wmiir create uses stdin, we need a way to either send EOF or just send
    the data. So we use Popen() with shell=True

    @param String output the output
    @date Jun 14, 2011
    @author Matthew Todd
    '''
    print(output)


def main():
    '''
    @date Jun 14, 2011
    @author Matthew Todd
    '''
    vol, is_mute = get_data()

    output = generate_output(vol, is_mute)

    print_output(output)

if __name__ == '__main__':
    main()


