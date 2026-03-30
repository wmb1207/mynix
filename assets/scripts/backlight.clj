#!/usr/bin/env bb
(ns backlight
  (:require [babashka.cli :as cli]
            [clojure.string :as str]
            [babashka.fs :as fs]))

(def cli-opts
  {:spec {:increase {:desc "increase backlight by 10%"
                     :alias :p}
          :decrease {:desc "decrease backlight by 10%"
                     :alias :d}
          :set {:desc "set backlight to N% (0-100)"
                :alias :s
                :coerce :long}}})

(def backlight-dir "/sys/class/backlight/intel_backlight")
(def brightness-path (str backlight-dir "/brightness"))
(def max-path (str backlight-dir "/max_brightness"))

(def max-brightness
  (parse-long (str/trim (slurp max-path))))

(def current
  (parse-long (str/trim (slurp brightness-path))))

(defn pct->abs [pct]
  (long (* max-brightness (/ pct 100.0))))

(defn clamp [v]
  (max 0 (min max-brightness v)))

(defn set-brightness [amount]
  (let [pct (int (Math/round (* 100.0 (/ amount max-brightness))))]
    (println "Setting brightness to" amount (str "(" pct "%)")))
  (spit brightness-path (str amount "\n")))

(defn -main [& args]
  (let [opts (cli/parse-opts args cli-opts)]
    (if (fs/exists? brightness-path)
      (cond
        (:increase opts) (set-brightness (clamp (+ current (pct->abs 10))))
        (:decrease opts) (set-brightness (clamp (- current (pct->abs 10))))
        (:set opts)      (set-brightness (clamp (pct->abs (:set opts))))
        :else (println "Usage: backlight.clj [--increase|-p] [--decrease|-d] [--set|-s N]"))
      (println "Backlight not found:" brightness-path))))

(apply -main *command-line-args*)
