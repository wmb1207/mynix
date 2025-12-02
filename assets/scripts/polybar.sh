#!/usr/bin/env bash

pkill -9 polybar

INPUT=$1

case "$INPUT" in
    laptop)
	echo "here"
	polybar -c ~/.config/polybar/config.ini top &
	;;
    external)
	echo "here2"
	polybar -c ~/.config/polybar/config.ini top-external &
	;;
    *)
	echo "here3"
	polybar -c ~/.config/polybar/config.ini top-external &
	polybar -c ~/.config/polybar/config.ini top &
	;;
esac

disown
