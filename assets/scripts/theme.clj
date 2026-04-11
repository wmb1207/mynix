#!/usr/bin/env bb

(require '[clojure.string :as str]
         '[clojure.edn    :as edn]
         '[babashka.process :refer [shell process]])

(def home (System/getenv "HOME"))

(defn read-theme [name]
  (edn/read-string (slurp (str home "/.config/themes/" name ".edn"))))

;; ── fvwm colorsets ────────────────────────────────────────────────────────────

(defn fvwm-colorsets [{:keys [foreground background active bg-alt active-alt
                               olive comments selection]}]
  [(str "Colorset 0  fg " foreground ", bg " background ", hi " bg-alt     ", sh " bg-alt     ", Plain, NoShape")
   (str "Colorset 1  fg " foreground ", bg " background ", hi " bg-alt     ", sh " bg-alt     ", Plain, NoShape")
   (str "Colorset 2  fg " background ", bg " active     ", hi " active-alt ", sh " active-alt ", Plain, NoShape")
   (str "Colorset 3  fg " active-alt  ", bg " active-alt ", hi " active-alt ", sh " active-alt ", Plain, NoShape")
   (str "Colorset 4  fg " active      ", bg " active     ", hi " active     ", sh " active     ", Plain, NoShape")
   (str "Colorset 5  fg " foreground ", bg " background ", hi " bg-alt     ", sh " bg-alt     ", Plain, NoShape")
   (str "Colorset 6  fg " background ", bg " active     ", hi " active-alt ", sh " active-alt ", Plain, NoShape")
   (str "Colorset 7  fg " comments   ", bg " background ", hi " bg-alt     ", sh " bg-alt     ", Plain, NoShape")
   (str "Colorset 8  fg " background ", bg " active-alt ", hi " active     ", sh " active     ", Plain, NoShape")
   (str "Colorset 10 fg " foreground ", bg " background ", hi " bg-alt     ", sh " bg-alt     ", Plain, NoShape")
   (str "Colorset 11 fg " background ", bg " active     ", hi " active-alt ", sh " active-alt ", Plain, NoShape")
   (str "Colorset 12 fg " background ", bg " olive      ", hi " olive      ", sh " olive      ", Plain, NoShape")
   (str "Colorset 13 fg " background ", bg " active-alt ", hi " active     ", sh " active     ", Plain, NoShape")
   (str "Colorset 14 fg " comments   ", bg " bg-alt     ", hi " selection  ", sh " selection  ", Plain, NoShape")])

(def fvwm-cmd
  (first (filter #(.exists (java.io.File. %))
                 ["/run/current-system/sw/bin/FvwmCommand"
                  "/usr/bin/FvwmCommand"])))

(defn apply-fvwm [t]
  (if-not fvwm-cmd
    (println "⚠ FvwmCommand not found, skipping fvwm update")
    (try
      (doseq [cs (fvwm-colorsets t)]
        (shell fvwm-cmd cs))
      (shell fvwm-cmd "Refresh")
      (catch Exception e
        (println "⚠ fvwm:" (ex-message e))))))

;; ── xresources ────────────────────────────────────────────────────────────────

(defn xresources-str [{:keys [background foreground colors cursor highlight
                               highlight-text]}]
  (str/join "\n"
    (concat
      [(str "URxvt.background:         " background)
       (str "URxvt.foreground:         " foreground)
       (str "URxvt.cursorColor:        " cursor)
       (str "URxvt.highlightColor:     " highlight)
       (str "URxvt.highlightTextColor: " highlight-text)]
      (map-indexed (fn [i c] (str "URxvt.color" i ": " c)) colors)
      [""])))

(defn apply-xresources [t]
  (let [tmp "/tmp/theme.Xresources"]
    (spit tmp (xresources-str t))
    (shell "xrdb" "-merge" tmp)))

;; ── urxvt ─────────────────────────────────────────────────────────────────────

(defn urxvt-osc [{:keys [background foreground cursor colors highlight highlight-text]}]
  (str/join
    (concat
      [(str "\033]10;" foreground "\007")
       (str "\033]11;" background "\007")
       (str "\033]12;" cursor "\007")
       (str "\033]17;" highlight "\007")
       (str "\033]19;" highlight-text "\007")]
      (map-indexed (fn [i c] (str "\033]4;" i ";" c "\007")) colors))))

(defn urxvt-ptys []
  (try
    (let [raw (try (:out (shell {:out :string :err :string :continue true}
                               "pgrep" "urxvt"))
                   (catch Exception _ ""))
          pids (remove str/blank? (str/split-lines raw))]
      (->> pids
           (mapcat
             (fn [pid]
               (try
                 (->> (.listFiles (java.io.File. (str "/proc/" pid "/fd")))
                      (keep (fn [f]
                              (try
                                (let [p (str (.toRealPath (.toPath f)
                                                          (into-array java.nio.file.LinkOption [])))]
                                  (when (and (str/starts-with? p "/dev/pts/")
                                             (not= p "/dev/pts/ptmx"))
                                    p))
                                (catch Exception _ nil)))))
                 (catch Exception _ []))))
           distinct))
    (catch Exception _ [])))

(defn apply-urxvt [t]
  (let [osc (urxvt-osc t)]
    (doseq [pty (urxvt-ptys)]
      (try
        (spit pty osc)
        (catch Exception e
          (println "⚠ urxvt" pty ":" (ex-message e)))))))

;; ── tmux ──────────────────────────────────────────────────────────────────────

(defn apply-tmux [{:keys [background foreground active]}]
  (try
    (shell "tmux" "set-option" "-g" "status-style"
           (str "fg=" foreground ",bg=" background))
    (shell "tmux" "set-option" "-g" "window-status-current-style"
           (str "fg=" background ",bg=" active))
    (shell "tmux" "refresh-client" "-S")
    (catch Exception _ nil)))

;; ── emacs ─────────────────────────────────────────────────────────────────────

(defn apply-emacs [{:keys [emacs-theme background foreground]}]
  (try
    (shell "emacsclient" "--eval"
           (str "(progn"
                " (mapc #'disable-theme custom-enabled-themes)"
                " (load-theme '" emacs-theme " t)"
                " (modify-all-frames-parameters"
                "  '((background-color . \"" background "\")"
                "    (foreground-color . \"" foreground "\")))"
                " (when (fboundp 'my-reset-whitespace-faces) (my-reset-whitespace-faces))"
                " (when (fboundp 'my-reset-font)"
                "   (my-reset-font)))"))
    (catch Exception _ nil)))

;; ;; ── dunst ─────────────────────────────────────────────────────────────────────

;; (defn dunstrc [{:keys [font background foreground active bg-alt comments color1]}]
;;   (str "[global]\n"
;;        "    font = \"" font " 10\"\n"
;;        "    background = \"" bg-alt "\"\n"
;;        "    foreground = \"" comments "\"\n"
;;        "    frame_color = \"" active "\"\n"
;;        "    separator_color = \"" bg-alt "\"\n"
;;        "    width = 150\n"
;;        "    offset=0x0\n"
;;        "    horizontal_padding = 0\n"
;;        "    padding = 0\n"
;;        "    frame_width = 2\n"
;;        "    gap_size = 10\n"
;;        "    origin = center-left\n"
;;        "    alignment = center\n"
;;        "    show_indicators = false\n\n"
;;        "[urgency_low]\n"
;;        "    background = \"" bg-alt "\"\n"
;;        "    foreground = \"" comments "\"\n"
;;        "    frame_color = \"" active "\"\n"
;;        "    timeout = 10\n\n"
;;        "[urgency_normal]\n"
;;        "    background = \"" bg-alt "\"\n"
;;        "    foreground = \"" foreground "\"\n"
;;        "    frame_color = \"" comments "\"\n"
;;        "    timeout = 10\n\n"
;;        "[urgency_critical]\n"
;;        "    background = \"" bg-alt "\"\n"
;;        "    foreground = \"" color1 "\"\n"
;;        "    frame_color = \"" color1 "\"\n"
;;        "    timeout = 0\n\n"
;;        "[spotify]\n"
;;        "    appname = \"Spotify\"\n"
;;        "    background = \"" bg-alt "\"\n"
;;        "    foreground = \"" comments "\"\n"
;;        "    frame_color = \"" comments "\"\n"
;;        "    timeout = 5\n"
;;        "    format = \"<b>%s</b>\\n%b\"\n"
;;        "    alignment = center\n"
;;        "    word_wrap = yes\n\n"
;;        "[spotify_alt]\n"
;;        "    desktop_entry = \"spotify\"\n"
;;        "    background = \"" bg-alt "\"\n"
;;        "    foreground = \"" comments "\"\n"
;;        "    frame_color = \"" comments "\"\n"
;;        "    timeout = 5\n"))

;; (defn apply-dunst [t]
;;   (let [rc-path (str home "/.config/dunst/dunstrc")]
;;     (spit rc-path (dunstrc t))
;;     (try (shell "pkill" "dunst") (catch Exception _ nil))
;;     (Thread/sleep 200)
;;     (process {:out :inherit :err :inherit}
;;              "dunst" "--config" rc-path)))

;; ── main ──────────────────────────────────────────────────────────────────────

(let [theme-name (first *command-line-args*)]
  (when-not (contains? #{"dark" "light"} theme-name)
    (println "Usage: theme.clj dark|light")
    (System/exit 1))
  (let [t (read-theme theme-name)]
    (apply-xresources t)
    (apply-urxvt t)
    (apply-fvwm t)
    (apply-tmux t)
    (apply-emacs t)
    ;; (apply-dunst t)                     ;
    (println "→" theme-name)))
