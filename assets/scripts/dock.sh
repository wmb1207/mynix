#!/usr/bin/env bash

POSITION="$1"
KEYBOARD="$2"

CONNECTED_EXTERNAL_CMD=$(xrandr | grep " connected" | grep -v eDP  | awk '{print $1}')
CONNECTED_EXTERNAL_DISPLAY_COUNT=$(echo "$CONNECTED_EXTERNAL_CMD" | wc -l)
xrandr --output eDP-1 --mode 2560x1600 --below "$CONNECTED_EXTERNAL_CMD" --rotate normal --output "$CONNECTED_EXTERNAL_CMD" --rotate normal --mode "$CONNECTED_EXTERNAL_RESOLUTION"


if [[ "$CONNECTED_EXTERNAL_DISPLAY_COUNT" -ne "1" ]]; then
    echo "Expected exactly 1 (ONE) external display, found $CONNECTED_EXTERNAL_DISPLAY_COUNT"
    exit 2
fi

# for m in $(bspc query -M); do
#     if ! xrandr | grep -q "^$m connected"; then
#         bspc monitor "$m" -r  # remove it
#     fi
# done

CONNECTED_EXTERNAL_RESOLUTION=$(xrandr | grep "$CONNECTED_EXTERNAL_CMD" -A1 | tail -1 | awk '{print $1}') # | awk '{print $1}')
echo $CONNECTED_EXTERNAL_RESOLUTION

echo "SETTING $CONNECTED_EXTERNAL_CMD"
case "$POSITION" in
    right)
	 xrandr --output eDP-1 --mode 2560x1600 --left-of "$CONNECTED_EXTERNAL_CMD" --rotate normal --output "$CONNECTED_EXTERNAL_CMD" --rotate normal --mode "$CONNECTED_EXTERNAL_RESOLUTION"
	 bspc monitor "$CONNECTED_EXTERNAL_CMD" -d Hollow Thorne Grimoire
	 bspc monitor eDP-1 -d Abyss Nocturne
	 polybar -c ~/.config/polybar/config.ini top-external &
	 ;;
     left)
	 xrandr --output eDP-1 --mode 2560x1600 --right-of "$CONNECTED_EXTERNAL_CMD" --rotate normal --output "$CONNECTED_EXTERNAL_CMD" --rotate normal --mode "$CONNECTED_EXTERNAL_RESOLUTION"
	 bspc monitor "$CONNECTED_EXTERNAL_CMD" -d Hollow Thorne Grimoire
	 bspc monitor eDP-1 -d Abyss Nocturne
	 polybar -c ~/.config/polybar/config.ini top-external &
	 ;;
     bottom)
	 xrandr --output eDP-1 --mode 2560x1600 --below "$CONNECTED_EXTERNAL_CMD" --rotate normal --output "$CONNECTED_EXTERNAL_CMD" --rotate normal --mode "$CONNECTED_EXTERNAL_RESOLUTION"
	 bspc monitor "$CONNECTED_EXTERNAL_CMD" -d Hollow Thorne Grimoire
	 bspc monitor eDP-1 -d Abyss Nocturne
	 polybar -c ~/.config/polybar/config.ini top-external &
	 ;;
     top)
	 xrandr --output eDP-1 --mode 2560x1600 --above "$CONNECTED_EXTERNAL_CMD" --rotate normal --output "$CONNECTED_EXTERNAL_CMD" --rotate normal --mode "$CONNECTED_EXTERNAL_RESOLUTION"
	 bspc monitor "$CONNECTED_EXTERNAL_CMD" -d Hollow Thorne Grimoire
	 bspc monitor eDP-1 -d Abyss Nocturne
	 polybar -c ~/.config/polybar/config.ini top-external &
	 ;;
     external)
	 xrandr --output eDP-1 --off --output "$CONNECTED_EXTERNAL_CMD"  --mode "$CONNECTED_EXTERNAL_RESOLUTION"
	 bspc monitor eDP-1 -r
	 bspc monitor "$CONNECTED_EXTERNAL_CMD" -d Hollow Thorne Grimoire Abyss Nocturne
	 pkill -9 polybar
	 polybar -c ~/.config/polybar/config.ini top-external &
	 ;;
     laptop)
	 bspc monitor "$CONNECTED_EXTERNAL_CMD" -r
	 xrandr --output eDP-1 --primary --mode 2560x1600 --pos 0x0 --rotate normal --output HDMI-1 --off --output DP-1 --off --output DP-2 --off
	 bspc monitor eDP-1 -d Hollow Thorne Grimoire Abyss Nocturne
	 ;;
     *)
	 exit 1
	 ;;
esac

case "$KEYBOARD" in
    dvorak)
	setxkbmap -layout us -variant "" -option ""
	;;
    qwerty)
	setxkbmap -layout us -variant dvorak -option ctrl:nocaps -option altwin:meta -option ctrl:swap_lalt_lctl
	;;
    *)
	exit 3
	;;
esac

bspc wm -r
