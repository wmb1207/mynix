#!/bin/sh
opts=$(cat "$HOME/.config/dmenu.opts" 2>/dev/null)
eval exec dmenu_run $opts
