#!/usr/bin/env janet

# start.janet — main orchestrator for FreeBSD desktop setup (freedom, Radeon RX 480)
#
# Run after bootstrap.sh has installed git + janet and cloned the repo.
#
# Usage:
#   janet start.janet              interactive menu
#   janet start.janet all          run every phase in order
#   janet start.janet <phase>      run a single phase
#
# Phases (in order):
#   packages   install all pkg packages
#   user       create user wmb, set shell, groups
#   system     rc.conf, sysctl.conf, loader.conf, hostname, hosts, locale
#   power      powerd + sysctl power tuning (replaces TLP)
#   audio      OSS / PipeWire setup
#   bluetooth  hcsecd + rc.conf
#   desktop    .xinitrc, Xresources, fvwm3 config dir
#   dotfiles   copy assets, scripts, emacs config
#   tailscale  install + enable tailscale, inject hosts
#   docker     delegate to setup/docker.janet (preflight → context)
#   fonts      install fonts to /usr/local/share/fonts

# ── helpers ──────────────────────────────────────────────────────────────────

(def bold  (fn [s] (string "\e[1m" s "\e[0m")))
(def green (fn [s] (string "\e[32m" s "\e[0m")))
(def red   (fn [s] (string "\e[31m" s "\e[0m")))
(def cyan  (fn [s] (string "\e[36m" s "\e[0m")))
(def dim   (fn [s] (string "\e[2m"  s "\e[0m")))
(def yel   (fn [s] (string "\e[33m" s "\e[0m")))

(defn step  [s] (print "\n" (bold (cyan (string "── " s " ──")))))
(defn ok    [s] (print (green "  ✓ ") s))
(defn fail  [s] (print (red   "  ✗ ") s) (os/exit 1))
(defn info  [s] (print (dim   "  → ") s))
(defn warn  [s] (print (yel   "  ! ") s))
(defn note  [s] (print "\n" (bold "NOTE: ") s "\n"))

(defn sh! [args]
  (info (string/join args " "))
  (let [exit (os/execute args :p)]
    (when (not= 0 exit)
      (fail (string "command failed (exit " exit "): " (string/join args " "))))))

(defn sh-ok? [args]
  (let [proc (os/spawn args :p {:out :pipe :err :pipe})]
    (= 0 (:wait proc))))

(defn sh-out [args]
  (let [proc (os/spawn args :p {:out :pipe})
        out  (:read (proc :out) :all)
        _    (:wait proc)]
    (string/trimr (or out ""))))

(defn must-be-root []
  (when (not= "0" (sh-out ["id" "-u"]))
    (fail "run this as root: doas janet start.janet")))

(defn file-append! [path line]
  (let [current (if (sh-ok? ["test" "-f" path]) (slurp path) "")]
    (unless (string/find line current)
      (let [f (file/open path :a)]
        (file/write f (string line "\n"))
        (file/close f))
      (info (string path ": " line)))))

(defn mkdir! [path]
  (unless (sh-ok? ["test" "-d" path])
    (sh! ["mkdir" "-p" path])))

(defn copy! [src dst]
  (sh! ["cp" src dst]))

(defn chown! [user path]
  (sh! ["chown" "-R" user path]))

# ── config ────────────────────────────────────────────────────────────────────

(def username  "wmb")
(def hostname  "freedom")
(def tz        "America/Argentina/Buenos_Aires")
(def user-home (string "/home/" username))
(def repo-dir  (os/cwd))   # we're run from bsd/
(def assets    (string repo-dir "/../assets"))
(def templates (string repo-dir "/../templates"))

(def extra-hosts
  ["100.66.15.24\tC1PROW19PRB01"
   "100.66.51.207\tC1DVUA1JB01.adb.sa-santiago-1.oraclecloud.com"
   "127.0.0.1\tlocal.be.warcgroup.com"])

# ── packages ──────────────────────────────────────────────────────────────────

(def cli-packages
  ["oksh"
   "git"
   "curl"
   "wget"
   "dtach"
   "direnv"
   "fzf"
   "fd-find"
   "ripgrep"
   "jq"
   "go-yq"
   "unzip"
   "zip"
   "gmake"
   "xclip"
   "bat"
   "tmux"
   "lf"
   "htop"
   "cmus"
   "mpv"
   "libtool"
   "janet"
   "abcde"
   "flac"
   "postgresql16-client"
   "xdotool"
   "pamixer"
   ])

(def gui-packages
  ["firefox"            # librewolf not in ports; firefox is fine
   "chromium"           # brave alternative
   "flameshot"
   "xterm"
   "rxvt-unicode"
   "pavucontrol"
   "vlc"
   "mupdf"
   "arandr"
   "xfe"
   "plan9port"
   "xclip"
   "xclock"
   "xload"
   "xrdb"
   "xsetroot"
   "ghostty"
   ])

(def wm-packages
  ["fvwm3"
   "dunst"
   "libnotify"
   "picom"
   "feh"
   "dmenu"
   "polybar"
   "xlockmore"              # xsecurelock not in ports
   "xautolock"
   ])

(def system-packages
  ["emacs"
   "tailscale"
   # NetworkManager not in FreeBSD ports; networking via rc.conf
   "dbus"
   "polkit"
   "vm-bhyve"           # for docker VM (setup/docker.janet)
   "bhyve-firmware"
   "grub2-bhyve"
   "docker"             # CLI only
   "docker-compose"
   "xorg-server"
   "xorg-drivers"
   "xauth"
   "xinit"
   "xrandr"
   "mesa-dri"
   "vulkan-loader"
   "drm-kmod"           # GPU / DRM kernel modules
   ])

(def font-packages
  ["dejavu"
   "jetbrains-mono"
   "terminus-font"
   ])

(defn phase-packages []
  (must-be-root)
  (step "installing packages")
  (let [all (array/concat @[]
               cli-packages
               gui-packages
               wm-packages
               system-packages
               font-packages)]
    (sh! (array/concat @["pkg" "install" "-y"] all)))
  (ok "all packages installed")
  (warn "some packages may not exist in ports — check output above for failures")
  (note "packages not in ports: gtop, clipmenu, ameba, postman, slack\ninstall manually or via npm/cargo/gem"))

# ── user ─────────────────────────────────────────────────────────────────────

(defn phase-user []
  (must-be-root)
  (step (string "user: " username))

  (if (sh-ok? ["id" username])
    (do
      (ok (string "user " username " already exists"))
      (sh! ["pw" "usermod" username "-s" "/usr/local/bin/oksh"])
      (ok "shell updated to oksh"))
    (do
      (sh! ["pw" "useradd" username
            "-m"                              # create home dir
            "-s" "/usr/local/bin/oksh"        # shell
            "-G" "wheel,video,audio,operator" # groups
            "-c" username])
      (ok (string "user " username " created"))))

  # ensure groups
  (each g ["wheel" "video" "audio" "operator" "docker"]
    (when (sh-ok? ["pw" "groupshow" g])
      (sh! ["pw" "groupmod" g "-m" username])
      (ok (string "added to group " g))))

  # doas config (equivalent of sudo wheel access)
  (let [doas-conf "/usr/local/etc/doas.conf"]
    (file-append! doas-conf "permit persist :wheel")
    (ok "doas configured"))

  (note (string "set password for " username " with: passwd " username)))

# ── system ────────────────────────────────────────────────────────────────────

(defn phase-system []
  (must-be-root)
  (step "system configuration")

  # hostname
  (spit "/etc/rc.conf.d/hostname" (string "hostname=\"" hostname "\"\n"))
  (sh! ["hostname" hostname])
  (ok (string "hostname: " hostname))

  # timezone
  (sh! ["tzsetup" tz])
  (ok (string "timezone: " tz))

  # locale — write /etc/login.conf.d/wmb
  (mkdir! "/etc/login.conf.d")
  (spit "/etc/login.conf.d/wmb"
        (string username ":\\\n"
                "\t:charset=UTF-8:\\\n"
                "\t:lang=en_US.UTF-8:\\\n"
                "\t:setenv=LC_ALL=en_US.UTF-8,LANG=en_US.UTF-8:\\\n"
                "\t:tc=default:\n"))
  (ok "locale: en_US.UTF-8")

  # /etc/hosts extras
  (step "extra hosts")
  (each h extra-hosts
    (file-append! "/etc/hosts" h)
    (ok h))

  # core rc.conf entries
  (step "rc.conf")
  (each line
    ["dbus_enable=\"YES\""
     "hald_enable=\"YES\""
     "moused_enable=\"YES\""
     "sshd_enable=\"YES\""
     "NetworkManager_enable=\"YES\""
     "cupsd_enable=\"YES\""
     "polkitd_enable=\"YES\""
     (string "defaultroute_delay=\"0\"")]
    (file-append! "/etc/rc.conf" line)
    (ok line))

  # loader.conf
  (step "loader.conf")
  (each line
    ["kern.vty=vt"          # modern vt console
     "hw.vga.textmode=0"
     "vmm_load=\"YES\""     # bhyve
     "if_bridge_load=\"YES\""
     "if_tap_load=\"YES\""]
    (file-append! "/boot/loader.conf" line)
    (ok line))

  (ok "system phase done"))

# ── power ─────────────────────────────────────────────────────────────────────

(defn phase-power []
  (must-be-root)
  (step "power management (replaces TLP)")

  # powerd — adaptive CPU frequency
  (file-append! "/etc/rc.conf" "powerd_enable=\"YES\"")
  (file-append! "/etc/rc.conf" "powerd_flags=\"-a hiadaptive -b adaptive -n adaptive\"")
  (ok "powerd enabled")

  # sysctl.conf tuning
  (let [entries
        ["hw.acpi.lid_switch_state=S3"       # suspend on lid close
         "hw.acpi.sleep_button_state=S3"
         "dev.cpu.0.cx_lowest=C6"            # deep C-states (battery)
         "hw.pci.do_power_nodriver=3"        # power down unused PCI devices
         "net.local.stream.sendspace=65536"
         "kern.hz=100"                       # lower timer freq saves power
         ]]
    (each e entries
      (file-append! "/etc/sysctl.conf" e)
      (ok (string "sysctl: " e))))

  (note "battery charge thresholds (40/80%) have no FreeBSD equivalent.\nMonitor with: acpiconf -i 0"))

# ── audio ─────────────────────────────────────────────────────────────────────

(defn phase-audio []
  (must-be-root)
  (step "audio")

  # OSS is native on FreeBSD; snd_hda for Intel HDA (Latitude)
  (file-append! "/boot/loader.conf" "snd_hda_load=\"YES\"")
  (ok "snd_hda (Intel HDA) enabled")

  # PipeWire (optional, for PulseAudio compat with apps like pavucontrol)
  (when (sh-ok? ["pkg" "info" "-e" "pipewire"])
    (file-append! "/etc/rc.conf" "pipewire_enable=\"YES\"")
    (ok "pipewire enabled"))

  (note "test audio with: cat /dev/zero > /dev/dsp\nOSS mixer: mixerctl"))

# ── bluetooth ────────────────────────────────────────────────────────────────

(defn phase-bluetooth []
  (must-be-root)
  (step "bluetooth")

  (file-append! "/boot/loader.conf" "ng_ubt_load=\"YES\"")
  (file-append! "/etc/rc.conf"      "hcsecd_enable=\"YES\"")
  (file-append! "/etc/rc.conf"      "sdpd_enable=\"YES\"")
  (file-append! "/etc/rc.conf"      "bluetooth_enable=\"YES\"")
  (ok "bluetooth rc.conf entries added")

  (note "pair devices with: hccontrol -n ubt0hci inquiry\nsee: man hccontrol"))

# ── desktop ───────────────────────────────────────────────────────────────────

(defn home [path] (string user-home "/" path))

(defn phase-desktop []
  (must-be-root)
  (step "desktop / X11")

  # run make.janet to render templates into assets
  (info "rendering config templates...")
  (sh! ["janet" "make.janet" "tmpl"])
  (ok "templates rendered")

  # .xinitrc
  (spit (home ".xinitrc")
        (string
          "#!/bin/sh\n"
          "# autostart\n"
          "xrandr --auto\n"
          "xautolock -time 5 -locker xsecurelock &\n"
          "picom -b &\n"
          "dunst &\n"
          "feh --bg-scale ~/.config/wallpapers/wallpaper-1.jpg &\n"
          "exec fvwm3\n"))
  (sh! ["chmod" "+x" (home ".xinitrc")])
  (ok ".xinitrc written")

  # xorg.conf — modesetting DDX over amdgpu KMS (Radeon RX 480)
  (mkdir! "/usr/local/etc/X11/xorg.conf.d")
  (file-append! "/boot/loader.conf" "amdgpu_load=\"YES\"")
  (file-append! "/etc/rc.conf"      "kld_list=\"amdgpu\"")
  (ok "loader.conf + rc.conf: amdgpu")

  # load amdgpu now so /dev/dri/card0 exists for this session
  (info "loading amdgpu kernel module...")
  (if (sh-ok? ["kldload" "amdgpu"])
    (ok "amdgpu loaded")
    (warn "kldload amdgpu failed — may already be loaded"))
  (let [loaded (sh-out ["kldstat"])]
    (if (string/find "amdgpu" loaded)
      (ok "amdgpu confirmed in kldstat")
      (fail "amdgpu NOT in kldstat — reboot and re-run desktop phase")))
  # verify /dev/dri/card0 exists before writing xorg.conf
  (if (sh-ok? ["test" "-e" "/dev/dri/card0"])
    (ok "/dev/dri/card0 present")
    (fail "/dev/dri/card0 missing — amdgpu did not initialise; check: dmesg | grep -i drm"))

  (spit "/usr/local/etc/X11/xorg.conf.d/20-video.conf"
        (string
          "Section \"Device\"\n"
          "  Identifier  \"Card0\"\n"
          "  Driver      \"modesetting\"\n"
          "  Option      \"AccelMethod\" \"glamor\"\n"
          "  Option      \"DRI\" \"3\"\n"
          "EndSection\n"
          "\n"
          "Section \"InputClass\"\n"
          "  Identifier \"touchpad\"\n"
          "  Driver \"libinput\"\n"
          "  MatchIsTouchpad \"on\"\n"
          "  Option \"Tapping\" \"off\"\n"
          "EndSection\n"))
  (ok "xorg.conf written (modesetting + amdgpu KMS)")

  # Xresources (URxvt creamsody palette)
  (spit (home ".Xresources")
        (string
          "URxvt.font:            xft:DejaVu Sans Mono:size=10\n"
          "URxvt.boldFont:        xft:DejaVu Sans Mono:bold:size=10\n"
          "URxvt.italicFont:      xft:DejaVu Sans Mono:italic:size=10\n"
          "URxvt.foreground:      #b5b2a0\n"
          "URxvt.background:      #1c1a18\n"
          "URxvt.color0:          #1c1a18\n"
          "URxvt.color1:          #884545\n"
          "URxvt.color2:          #657050\n"
          "URxvt.color3:          #8a7040\n"
          "URxvt.color4:          #4a6a78\n"
          "URxvt.color5:          #785a5a\n"
          "URxvt.color6:          #4a7070\n"
          "URxvt.color7:          #9a9888\n"
          "URxvt.color8:          #3a3830\n"
          "URxvt.color9:          #9a5035\n"
          "URxvt.color10:         #7a8060\n"
          "URxvt.color11:         #9a8050\n"
          "URxvt.color12:         #5a7888\n"
          "URxvt.color13:         #8a7070\n"
          "URxvt.color14:         #5a8080\n"
          "URxvt.color15:         #b5b2a0\n"
          "URxvt.cursorColor:          #b0ad9a\n"
          "URxvt.highlightColor:       #3a3525\n"
          "URxvt.highlightTextColor:   #b5b2a0\n"
          "URxvt.scrollBar:            false\n"
          "URxvt.scrollTtyOutput:      false\n"
          "URxvt.scrollWithBuffer:     true\n"
          "URxvt.scrollTtyKeypress:    true\n"
          "URxvt.saveLines:            1000\n"
          "URxvt.internalBorder:       2\n"
          "URxvt.borderWidth:          0\n"
          "URxvt.perl-ext-common:      default,clipboard,selection-to-clipboard\n"
          "URxvt.clipboard.autocopy:   true\n"
          "URxvt.clipboard.copycmd:    xclip -i -selection clipboard\n"
          "URxvt.clipboard.pastecmd:   xclip -o -selection clipboard\n"
          "URxvt.meta8:                false\n"
          "URxvt.iso14755:             false\n"
          "URxvt.iso14755_52:          false\n"
          "URxvt.keysym.Control-Shift-w: perl:clipboard:copy\n"
          "URxvt.keysym.Control-Shift-y: perl:clipboard:paste\n"))
  (ok ".Xresources written")

  (chown! username user-home)
  (ok "desktop phase done"))

# ── dotfiles ──────────────────────────────────────────────────────────────────

(defn phase-dotfiles []
  (must-be-root)
  (step "dotfiles + assets")

  (defn install-asset [src rel-dst]
    (let [dst  (home rel-dst)
          parts (string/split "/" dst)
          dir  (string/join (array/slice parts 0 (- (length parts) 1)) "/")]
      (mkdir! dir)
      (copy! (string assets "/" src) dst)
      (ok (string rel-dst))))

  (defn install-script [src rel-dst]
    (let [dst (home rel-dst)]
      (install-asset src rel-dst)
      (sh! ["chmod" "+x" dst])))

  # configs
  (install-asset "fvwm3.conf"      ".fvwm/config")
  (install-asset "dunstrc"         ".config/dunst/dunstrc")
  (install-asset "ghostty"         ".config/ghostty/config")
  (install-asset "polybar.ini"     ".config/polybar/config.ini")
  (install-asset "picom.conf"      ".config/picom/picom.conf")
  # BSD-specific emacs configs (MELPA-based, not Nix)
  (mkdir! (home ".emacs.d/lisp"))
  (copy! (string repo-dir "/init.el")     (home ".emacs.d/init.el"))
  (copy! (string repo-dir "/packages.el") (home ".emacs.d/lisp/packages.el"))
  (ok ".emacs.d/init.el")
  (ok ".emacs.d/lisp/packages.el")

  # scripts (janet versions instead of clj)
  (mkdir! (home ".local/bin"))
  (each script ["backlight.janet" "battery.janet" "dock.janet" "keys.janet"]
    (let [src  (string repo-dir "/scripts/" script)
          dst  (home (string ".local/bin/" script))]
      (copy! src dst)
      (sh! ["chmod" "+x" dst])
      (ok (string ".local/bin/" script))))

  # wallpapers
  (mkdir! (home ".config/wallpapers"))
  (let [wp-src (string assets "/wallpapers")]
    (when (sh-ok? ["test" "-d" wp-src])
      (sh! ["cp" "-r" wp-src (home ".config/")])))

  # .kshrc
  (spit (home ".kshrc")
        (string
          "# Only interactive shells\n"
          "[[ $- != *i* ]] && return\n\n"
          "# === History ===\n"
          "HISTFILE=\"$HOME/.ksh_history\"\n"
          "HISTSIZE=10000\n"
          "SAVEHIST=20000\n\n"
          "PS1='\\033[38;5;67m$(pwd | sed \"s|$HOME|~|\")\\033[0m $ '\n\n"
          "# === Aliases ===\n"
          "alias ll='ls -lh --color=auto'\n"
          "alias la='ls -lah --color=auto'\n"
          "alias gs='git status'\n"
          "alias gc='git commit'\n"
          "alias ga='git add'\n"
          "alias gl='git log --oneline --graph --decorate'\n"
          "alias em='emacsclient -nw'\n"
          "alias e='emacsclient -c -a emacs'\n\n"
          "# === fzf ===\n"
          "if command -v fzf >/dev/null 2>&1; then\n"
          "  export FZF_DEFAULT_COMMAND='rg --files --hidden --follow --glob \"!.git/*\"'\n"
          "  export FZF_CTRL_T_COMMAND=\"$FZF_DEFAULT_COMMAND\"\n"
          "  export FZF_DEFAULT_OPTS='--height 40% --layout=reverse --border'\n"
          "fi\n\n"
          "export EDITOR='emacsclient -c -a emacs'\n"
          "export VISUAL=\"$EDITOR\"\n"
          "export PAGER='less -R'\n"
          "export PATH=\"$HOME/.local/bin:$HOME/go/bin:$HOME/.cargo/bin:$PATH\"\n"
          "export ENV=\"$HOME/.kshrc\"\n\n"
          "# === Direnv ===\n"
          "if command -v direnv >/dev/null 2>&1; then\n"
          "  eval \"$(direnv hook ksh)\"\n"
          "fi\n\n"
          "# === Starship ===\n"
          "if command -v starship >/dev/null 2>&1; then\n"
          "  eval \"$(starship init ksh)\"\n"
          "fi\n"))
  (ok ".kshrc written")

  (chown! username user-home)
  (ok "dotfiles phase done"))

# ── tailscale ─────────────────────────────────────────────────────────────────

(defn phase-tailscale []
  (must-be-root)
  (step "tailscale")

  (file-append! "/etc/rc.conf" "tailscaled_enable=\"YES\"")
  (sh! ["service" "tailscaled" "start"])
  (ok "tailscaled started")

  (note "authenticate with: tailscale up\nthen: tailscale status"))

# ── docker ────────────────────────────────────────────────────────────────────

(defn phase-docker []
  (must-be-root)
  (step "docker (bhyve VM)")
  (info "delegating to setup/docker.janet...")
  (each sub ["preflight" "init" "create"]
    (sh! ["janet" "setup/docker.janet" sub]))
  (note "after Alpine install completes inside the VM:\n  janet setup/docker.janet context"))

# ── fonts ─────────────────────────────────────────────────────────────────────

(defn phase-fonts []
  (must-be-root)
  (step "fonts")
  (sh! ["fc-cache" "-f" "-v"])
  (ok "font cache rebuilt")
  (warn "some fonts (dina, cozette, tamzen) are not in ports")
  (note "install manually:\n  https://www.dcmembers.com/jibsen/download/61/  (dina)\n  https://github.com/slavfox/Cozette (cozette)"))

# ── all ───────────────────────────────────────────────────────────────────────

(def phases
  [["packages"  phase-packages  "install all pkg packages"]
   ["user"      phase-user      "create user, shell, groups, doas"]
   ["system"    phase-system    "rc.conf, loader.conf, hostname, hosts, locale"]
   ["power"     phase-power     "powerd + sysctl power tuning"]
   ["audio"     phase-audio     "snd_hda + PipeWire"]
   ["bluetooth" phase-bluetooth "hcsecd + rc.conf"]
   ["desktop"   phase-desktop   "templates, .xinitrc, Xresources, xorg.conf"]
   ["dotfiles"  phase-dotfiles  "assets, scripts, .kshrc"]
   ["fonts"     phase-fonts     "fc-cache + notes on missing fonts"]
   ["tailscale" phase-tailscale "tailscaled rc.conf + start"]
   ["docker"    phase-docker    "bhyve VM setup (see setup/docker.janet)"]])

(defn run-all []
  (each [name f _] phases
    (print "\n" (bold (string "═══ " (string/ascii-upper name) " ═══")))
    (f))
  (step "all phases complete")
  (print)
  (ok "reboot, then: startx")
  (ok "docker: janet setup/docker.janet context  (after VM install)")
  (ok "tailscale: tailscale up"))

# ── menu ─────────────────────────────────────────────────────────────────────

(defn menu []
  (print "\n" (bold "FreeBSD latitude setup") "\n")
  (each [i [name _ desc]] (pairs phases)
    (printf "  %s%-12s%s %s\n" (bold "") name "" desc))
  (print "\n  " (bold "all") "          run all phases in order")
  (print "\nphase: ")
  (let [choice (string/trim (string (file/read stdin :line)))]
    choice))

(defn main [_ & args]
  (let [cmd (get args 0 (menu))]
    (if (= cmd "all")
      (run-all)
      (if-let [entry (find |(= (get $ 0) cmd) phases)]
        ((get entry 1))
        (do
          (print (red (string "unknown phase: " cmd)))
          (main nil))))))
