#!/usr/bin/env janet

# Display/dock setup via xrandr (works on FreeBSD with xorg)
# Replaces bspc (bspwm) commands with fvwm3-appropriate ones

(defn sh-out [args]
  (let [proc (os/spawn args :p {:out :pipe})
        out  (:read (proc :out) :all)
        _    (:wait proc)]
    (string/trimr (or out ""))))

(defn sh! [args]
  (print "$ " (string/join args " "))
  (os/execute args :p))

# -- xrandr parsing --
(defn parse-xrandr [output]
  (var current nil)
  (var displays @[])
  (each line (string/split "\n" output)
    (cond
      (string/find " connected" line)
      (do
        (when current (array/push displays current))
        (set current {:name (first (string/split " " line)) :resolutions @[]}))

      (and current (peg/match '(* :s+ :d+ "x" :d+) line))
      (let [m (peg/match '(* :s+ (capture (* :d+ "x" :d+))) line)]
        (when m (array/push (current :resolutions) (first m))))))
  (when current (array/push displays current))
  displays)

(defn external-display [displays]
  (find |(not= "eDP-1" ($ :name)) displays))

(defn xrandr-args [position display]
  (let [base ["xrandr" "--output" "eDP-1"]]
    (case position
      "left"     (array/concat base ["--mode" "2560x1600" "--left-of"  (display :name)
                                     "--output" (display :name) "--mode" (get (display :resolutions) 0)])
      "right"    (array/concat base ["--mode" "2560x1600" "--right-of" (display :name)
                                     "--output" (display :name) "--mode" (get (display :resolutions) 0)])
      "bottom"   (array/concat base ["--mode" "2560x1600" "--below"    (display :name)
                                     "--output" (display :name) "--mode" (get (display :resolutions) 0)])
      "top"      (array/concat base ["--mode" "2560x1600" "--above"    (display :name)
                                     "--output" (display :name) "--mode" (get (display :resolutions) 0)])
      "external" (array/concat base ["--off" "--output" (display :name) "--mode" (get (display :resolutions) 0)])
      "laptop"   (array/concat base ["--mode" "2560x1600"])
      (error (string "unknown position: " position)))))

(defn usage []
  (print "usage: dock.janet <position>")
  (print "  positions: left right top bottom external laptop"))

(defn main [_ & args]
  (let [position (get args 0)]
    (if (nil? position)
      (usage)
      (let [xrandr-out (sh-out ["xrandr"])
            displays   (parse-xrandr xrandr-out)
            ext        (external-display displays)]
        (if (and (not= position "laptop") (nil? ext))
          (print "no external display detected")
          (sh! (xrandr-args position ext)))))))
