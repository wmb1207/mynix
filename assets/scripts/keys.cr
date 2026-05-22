#!/usr/bin/env crystal

BOLD   = "\e[1m"
DIM    = "\e[2m"
CYAN   = "\e[36m"
YELLOW = "\e[33m"
RESET  = "\e[0m"

def hdr(value : String)
  puts "\n#{BOLD}#{CYAN}#{value}#{RESET}"
end

def row(key : String, desc : String)
  printf "  #{BOLD}%-32s#{RESET}#{DIM}%s#{RESET}\n", key, desc
end

def principle(title : String, body : String)
  printf "  #{BOLD}#{YELLOW}%-22s#{RESET}%s\n", title, body
end

hdr "── Applications ────────────────────"
row "Super+Return", "terminal"
row "Super+Space", "dmenu run"
row "Super+e", "emacs client"
row "Super+w", "all windows (all desks)"
row "Super+k", "toggle dvorak / qwerty"
row "Super+Shift+b", "battery status"
row "Super+Shift+l", "lock screen"
row "BrightnessDown/Up", "backlight -/+"

hdr "── Windows ─────────────────────────"
row "Super+q", "close"
row "Super+Shift+Space", "maximize fullscreen"
row "Super+Shift+f", "toggle maximize"
row "Super+Shift+r", "raise"
row "Super+Shift+m", "lower"
row "Super+Tab / Alt+Tab", "window list"
row "Super+o", "cycle windows"
row "Super+\\", "toggle left panel"

hdr "── Window Move (Super+Alt) ─────────"
row "Super+Alt+f/b", "move right/left 25%"
row "Super+Alt+n/p", "move down/up 25%"

hdr "── Window Resize (Super+Ctrl+Alt) ──"
row "Super+Ctrl+Alt+f/b", "grow right/left 25%"
row "Super+Ctrl+Alt+n/p", "grow down/up 25%"

hdr "── Window Shuffle (Ctrl+Meta) ──────"
row "Ctrl+Meta+Arrow", "shuffle window direction"
row "Ctrl+Shift+Meta+Arrow", "grow window direction"

hdr "── Desktops (Super+N) ─────────────"
row "Super+1..5", "switch to desktop"
row "Super+Shift+1..5", "move window to desktop"

hdr "── Pages (Super+Ctrl+N) ────────────"
row "Super+Ctrl+1", "page top-left"
row "Super+Ctrl+2", "page top-right"
row "Super+Ctrl+3", "page bottom-left"
row "Super+Ctrl+4", "page bottom-right"
row "Super+Shift+Ctrl+N", "move window to page"
row "Super+n / Super+p", "cycle pages forward/back"

hdr "── Mouse ───────────────────────────"
row "Super+LMB", "move window"
row "Super+RMB", "resize window"
row "LMB titlebar", "move / double-click maximize"
row "Scroll titlebar", "shade / unshade"
row "RMB titlebar", "window ops menu"
row "RMB desktop", "window ops"
row "MMB desktop", "window list"
row "3-finger swipe up", "all windows"

hdr "── Menus ───────────────────────────"
row "F1 / Menu key", "root menu"

hdr "── studiowmb principles ────────────"
principle "open by default", "source, infra, decisions - shared freely, shaped by community"
principle "minimal by design", "one thing well, easy to reason about, no unnecessary footprint"
principle "systems thinking", "every layer - TTY to web services to NixOS infrastructure"
principle "self-hosted first", "own the stack - git, infra, services on hardware we control"

puts "\n#{DIM}press any key to close#{RESET}"
STDIN.read_byte
