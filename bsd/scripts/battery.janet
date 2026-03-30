#!/usr/bin/env janet

# FreeBSD battery monitoring via sysctl hw.acpi.battery

(defn battery-life []
  (let [proc (os/spawn ["sysctl" "-n" "hw.acpi.battery.life"] :p {:out :pipe})
        out  (:read (proc :out) :all)
        _    (:wait proc)
        v    (string/trim (or out ""))]
    (if (= "" v) nil (scan-number v))))

(defn notify [msg]
  (print "notify: " msg)
  (os/execute ["notify-send" "Battery" msg] :p))

(defn check-and-notify [pct last-pct]
  (when pct
    (cond
      (< pct 10)           (notify (string pct "% — critically low!"))
      (< pct 20)           (notify (string pct "% — low battery"))
      (= 0 (% pct 20))     (notify (string pct "%")))
    (when (and last-pct (< pct 5) (>= last-pct 5))
      (notify "5% — plug in now!"))))

(defn daemon []
  (print "starting battery daemon...")
  (var last nil)
  (forever
    (let [pct (battery-life)]
      (check-and-notify pct last)
      (set last pct))
    (os/sleep 30)))

(defn status []
  (let [pct (battery-life)]
    (if pct
      (print "battery: " pct "%")
      (print "battery: not found (hw.acpi.battery.life)"))))

(defn usage []
  (print "usage: battery.janet <command>")
  (print "  status     print current level")
  (print "  daemon     run notification daemon"))

(defn main [_ & args]
  (case (get args 0)
    "status" (status)
    "daemon" (daemon)
    (usage)))
