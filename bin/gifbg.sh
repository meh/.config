#!/bin/sh
# Uses xwinwrap to display given animated .gif in the center of the screen

if [ $# -ne 1 ]; then
echo 1>&2 Usage: $0 image.gif
exit 1
fi

killall -9 xwinwrap &> /dev/null
xwinwrap -fs -ov -ni -s -nf -- gifview -w WID $1 -a &> /dev/null &
