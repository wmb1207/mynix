#!/usr/bin/env bash

notify_message() {
    echo "Battery level at $1 / 100"
}

notify_battery() {
    local value=$1

    if (( value <= 15 )); then
        notify-send -u critical "$(notify_message "$value")"
        return
    fi

    notify-send "$(notify_message "$value")"
}

check() {
    CAPACITY=$(cat /sys/class/power_supply/BAT0/capacity)
    echo "$CURRENT_CAPACITY > $CAPACITY"

    if (($CURRENT_CAPACITY == $CAPACITY)); then
	return
    fi

    DIFF=$(( CURRENT_CAPACITY - CAPACITY ))
    if (( CAPACITY % 5 == 0 || DIFF > 5 )); then
        echo "$CAPACITY"
        notify_battery "$CAPACITY"
    fi
}

CURRENT_CAPACITY=$(cat /sys/class/power_supply/BAT0/capacity)
notify_battery "$CURRENT_CAPACITY"


while :; do
    check
    sleep 30
done
