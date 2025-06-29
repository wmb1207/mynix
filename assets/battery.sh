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
    CURRENT_CAPACITY=$CAPACITY
}

daemon_mode() {
    echo "Starting battery monitor daemon..."
    CURRENT_CAPACITY=$(cat /sys/class/power_supply/BAT0/capacity)
    notify_battery "$CURRENT_CAPACITY"
    
    while :; do
        check
        sleep 30
    done
}

show_current() {
    CURRENT_CAPACITY=$(cat /sys/class/power_supply/BAT0/capacity)
    notify_battery "$CURRENT_CAPACITY"
}

# Parse command line arguments
case "${1:-}" in
    --daemon|-d)
        daemon_mode
        ;;
    --help|-h)
        echo "Usage: $0 [--daemon|-d] [--help|-h]"
        echo "  --daemon, -d    Run as daemon (continuous monitoring)"
        echo "  --help, -h      Show this help message"
        echo "  (no args)       Show current battery notification once"
        ;;
    "")
        show_current
        ;;
    *)
        echo "Unknown option: $1"
        echo "Use --help for usage information"
        exit 1
        ;;
esac
