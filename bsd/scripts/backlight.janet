#!/usr/bin/env janet

# FreeBSD backlight control via backlight(8) (FreeBSD 13+)
# Falls back to sysctl hw.acpi.video.lcd0.brightness

(defn sysctl-get [key]
  (let [proc (os/spawn ["sysctl" "-n" key] :p {:out :pipe})
        out  (:read (proc :out) :all)
        _    (:wait proc)]
    (string/trim (or out ""))))

(defn backlight-cmd? []
  (= 0 (os/execute ["which" "backlight"] :p {:out :pipe :err :pipe})))

(defn get-current []
  (if (backlight-cmd?)
    (let [proc (os/spawn ["backlight"] :p {:out :pipe})
          out  (:read (proc :out) :all)
          _    (:wait proc)
          # output: "brightness: 80"
          m    (peg/match '(* "brightness: " (number :d+)) (string/trim (or out "")))]
      (if m (first m) 50))
    (let [v (sysctl-get "hw.acpi.video.lcd0.brightness")]
      (if (= "" v) 50 (scan-number v)))))

(defn set-backlight [pct]
  (let [clamped (max 0 (min 100 pct))]
    (print "setting brightness to " clamped "%")
    (if (backlight-cmd?)
      (os/execute ["backlight" (string clamped)] :p)
      (os/execute ["sysctl" (string "hw.acpi.video.lcd0.brightness=" clamped)] :p))))

(defn usage []
  (print "usage: backlight.janet <command>")
  (print "  up          increase by 10%")
  (print "  down        decrease by 10%")
  (print "  set <N>     set to N (0-100)"))

(defn main [_ & args]
  (let [cmd (get args 0)]
    (case cmd
      "up"   (set-backlight (+ (get-current) 10))
      "down" (set-backlight (- (get-current) 10))
      "set"  (if (get args 1)
               (set-backlight (scan-number (get args 1)))
               (usage))
      (usage))))
