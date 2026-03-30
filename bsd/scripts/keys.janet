#!/usr/bin/env janet

(def bold  "\e[1m")
(def dim   "\e[2m")
(def cyan  "\e[36m")
(def reset "\e[0m")

(defn hdr [s]
  (print "\n" bold cyan s reset))

(defn row [key desc]
  (printf "  %s%-30s%s%s%s%s\n" bold key reset dim desc reset))

(hdr "── Applications ──────────────────")
(row "Super+Return"          "terminal")
(row "Super+Space"           "dmenu run")
(row "Super+e"               "emacs client")
(row "Super+Shift+b"         "battery status")
(row "Super+Shift+l"         "lock screen")
(row "BrightnessDown"        "decrease backlight")
(row "BrightnessUp"          "increase backlight")

(hdr "── Windows ───────────────────────")
(row "Super+q"               "close")
(row "Super+Shift+Space"     "maximize fullscreen")
(row "Super+Shift+f"         "toggle maximize")
(row "Super+Shift+r"         "raise")
(row "Super+Shift+m"         "lower")
(row "Super+Tab / Alt+Tab"   "window list")
(row "Super+o"               "cycle windows")

(hdr "── Window Move (Super+Alt) ───────")
(row "Super+Alt+f/b"         "move right/left 25%")
(row "Super+Alt+n/p"         "move down/up 25%")

(hdr "── Window Resize (Super+Ctrl+Alt)")
(row "Super+Ctrl+Alt+f/b"    "grow right/left 25%")
(row "Super+Ctrl+Alt+n/p"    "grow down/up 25%")

(hdr "── Desktops (Super+N) ────────────")
(row "Super+1..5"            "switch to desktop 1-5")
(row "Super+Shift+1..5"      "move window to desktop")

(hdr "── Pages (Super+Ctrl+N) ──────────")
(row "Super+Ctrl+1"          "page top-left")
(row "Super+Ctrl+2"          "page top-right")
(row "Super+Ctrl+3"          "page bottom-left")
(row "Super+Ctrl+4"          "page bottom-right")
(row "Super+Shift+Ctrl+N"    "move window to page")
(row "Super+n / Super+p"     "cycle pages forward/back")

(hdr "── Mouse ─────────────────────────")
(row "Super+LMB"             "move window")
(row "Super+RMB"             "resize window")
(row "LMB titlebar"          "move / double-click maximize")
(row "Scroll titlebar"       "shade / unshade")
(row "RMB titlebar"          "window ops menu")
(row "RMB desktop"           "window ops")
(row "MMB desktop"           "window list")

(hdr "── Menus ─────────────────────────")
(row "F1 / Menu key"         "root menu")

(print "\n" dim "press any key to close" reset)
(file/read stdin 1)
