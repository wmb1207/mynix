#!/usr/bin/env sh
dir="$HOME/Pictures/screenshots"
mkdir -p "$dir"
file="$dir/$(date +%Y%m%d_%H%M%S).png"
maim -s "$file" && xclip -selection clipboard -t image/png < "$file"
