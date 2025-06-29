#!/usr/bin/env bash

# date-time
eww update date="$(date '+%a %b %d')"
eww update time="$(date '+%H:%M')"

# memory
mem_info=$(free -h | awk '/^Mem/ {print $3 " " $2}')
eww update mem_used=$(echo $mem_info | cut -d' ' -f1)
eww update mem_total=$(echo $mem_info | cut -d' ' -f2)

# cpu
cpu=$(top -bn1 | grep "Cpu(s)" | awk '{ print 100 - $8 }')
eww update cpu="${cpu}%"

# disk
df_root=$(df -h / | awk 'NR==2 {print $3 " " $2}')
eww update root_used=$(echo $df_root | cut -d' ' -f1)
eww update root_total=$(echo $df_root | cut -d' ' -f2)

df_home=$(df -h /home | awk 'NR==2 {print $3 " " $2}')
eww update home_used=$(echo $df_home | cut -d' ' -f1)
eww update home_total=$(echo $df_home | cut -d' ' -f2)

# volume
vol=$(pactl get-sink-volume @DEFAULT_SINK@ | awk '{print $5}' | head -n1)
eww update volume=$vol

# battery
battery=$(acpi -b | awk -F', ' '{print $2}')
eww update battery="$battery"

# wifi
ssid=$(nmcli -t -f active,ssid dev wifi | grep '^yes' | cut -d: -f2)
eww update ssid="${ssid:-Disconnected}"
