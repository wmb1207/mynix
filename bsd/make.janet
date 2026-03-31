#!/usr/bin/env janet

# -- colors: gloomy creamsody --
(def black     "#1c1a18")
(def bg-alt    "#252320")
(def selection "#302d29")
(def dark-gray "#6a6858")
(def white     "#b5b2a0")
(def red       "#884545")
(def olive     "#8a7040")
(def green     "#657050")
(def blue      "#4a6a78")
(def mauve     "#785a5a")
(def cream     "#9a9888")

(def font             "DejaVu Sans Mono")
(def theme            "creamsody-darker")
(def light-theme      "ef-day")
(def ghostty-theme    "Wez")
(def ghostty-theme-light "GruvboxLight")
(def transparency     "100")
(def templates-dir    "../templates")
(def assets-dir       "../assets")

# -- template engine --
(defn apply-tmpl [{:name tname :output output :content content :fields fields}]
  (var result content)
  (each [k v] (pairs fields)
    (set result (string/replace-all k v result)))
  (spit output result)
  (print "  wrote " output " [" tname "]"))

(defn tmpl [name output tmpl-file fields]
  {:name    name
   :output  (string assets-dir "/" output)
   :content (slurp (string templates-dir "/" tmpl-file))
   :fields  fields})

# -- shell --
(defn sh! [args]
  (print "$ " (string/join args " "))
  (let [exit (os/execute args :p)]
    (when (not= 0 exit)
      (eprint "!! command failed with exit " exit)
      (os/exit exit))))

(defn sh-out [args]
  (let [proc (os/spawn args :p {:out :pipe})
        out  (:read (proc :out) :all)
        _    (:wait proc)]
    (string/trimr (or out ""))))

# -- templates --
(def fvwm3
  (tmpl "fvwm3" "fvwm3.conf" "fvwm3.conf.tmpl"
        {"{{background}}"  black
         "{{foreground}}"  white
         "{{bg-alt}}"      bg-alt
         "{{selection}}"   selection
         "{{active}}"      blue
         "{{active-alt}}"  cream
         "{{comments}}"    dark-gray
         "{{green}}"       green
         "{{olive}}"       olive
         "{{theme}}"       theme
         "{{font}}"        font}))

(def ghostty-dark
  (tmpl "ghostty" "ghostty" "ghostty.tmpl"
        {"{{background}}"   black
         "{{transparency}}" transparency
         "{{theme}}"        ghostty-theme
         "{{font}}"         font}))

(def ghostty-light
  (tmpl "ghostty" "ghostty" "ghostty.tmpl"
        {"{{background}}"   white
         "{{transparency}}" transparency
         "{{theme}}"        ghostty-theme-light
         "{{font}}"         font}))

(def dunstrc
  (tmpl "dunstrc" "dunstrc" "dunstrc.tmpl"
        {"{{black}}"        bg-alt
         "{{frame}}"        blue
         "{{green}}"        green
         "{{red}}"          red
         "{{white}}"        white
         "{{transparency}}" transparency
         "{{font}}"         font}))

(def emacs-dark
  (tmpl "emacs" "init.el" "init.el.tmpl"
        {"{{transparency}}" transparency
         "{{theme}}"        theme
         "{{font}}"         font
         "{{background}}"   black}))

(def emacs-light
  (tmpl "emacs" "init.el" "init.el.tmpl"
        {"{{transparency}}" transparency
         "{{theme}}"        light-theme
         "{{font}}"         font
         "{{background}}"   white}))

# -- pkg --
(def packages
  ["fvwm3" "dunst" "emacs" "ghostty" "dmenu"
   "xrandr" "xinit" "xorg-server" "drm-kmod"])

(defn pkg-install []
  (print "installing packages via pkg...")
  (sh! (array/concat ["pkg" "install" "-y"] packages)))

# -- rc.conf helpers --
(def rc-conf "/etc/rc.conf")

(defn rc-set! [key value]
  (let [current (slurp rc-conf)
        line     (string key "=\"" value "\"")
        pat      (string key "=")]
    (if (string/find pat current)
      (spit rc-conf (string/replace-all (string pat ``.*``) line current))
      (spit rc-conf (string current "\n" line "\n")))
    (print "  rc.conf: " line)))

(defn setup-rc []
  (print "configuring rc.conf...")
  (rc-set! "dbus_enable"     "YES")
  (rc-set! "hald_enable"     "YES")
  (rc-set! "moused_enable"   "YES")
  (rc-set! "allscreens_flags" "-nolisten tcp"))

# -- xinitrc --
(defn write-xinitrc []
  (let [path (string (os/getenv "HOME") "/.xinitrc")]
    (spit path "exec fvwm3\n")
    (print "  wrote " path)))

# -- fvwm3 config --
(defn install-fvwm-config []
  (let [home    (os/getenv "HOME")
        dst-dir (string home "/.fvwm")
        dst     (string dst-dir "/config")
        src     (string assets-dir "/fvwm3.conf")]
    (os/mkdir dst-dir)
    (sh! ["cp" src dst])
    (print "  wrote " dst)))

# -- main --
(defn usage []
  (print "usage: make.janet <command>")
  (print "  tmpl [--light]   apply templates")
  (print "  install          install packages via pkg")
  (print "  apply [--light]  tmpl + install + xinitrc + rc.conf"))

(defn light? [args]
  (find |(= $ "--light") args))

(defn run-tmpls [args]
  (print "applying templates...")
  (let [ghostty (if (light? args) ghostty-light ghostty-dark)
        emacs   (if (light? args) emacs-light   emacs-dark)]
    (each t [fvwm3 dunstrc ghostty emacs]
      (apply-tmpl t))))

(defn main [_ & args]
  (let [args (array ;args)
        cmd  (get args 0)]
    (case cmd
      "tmpl"    (run-tmpls args)
      "install" (pkg-install)
      "apply"   (do (run-tmpls args)
                    (pkg-install)
                    (setup-rc)
                    (write-xinitrc)
                    (install-fvwm-config))
      (usage))))
