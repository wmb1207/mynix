#!/bin/bash
# Kill existing bars
killall -q polybar

# Wait until polybar processes have shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# Launch the bar
polybar top &
