#!/usr/bin/env bb
(ns backlight
  (:require [babashka.cli :as cli]
            [clojure.string :as str]
            [babashka.process :refer [shell check process]]
            [babashka.fs :as fs]))

(def cli-opts
  {:spec {:increase {:desc "increase backlight"
                     :alias :p}
          :decrease {:desc "decrease backlight"
                     :alias :d}
          :set {:desc "set backlight"
                     :alias :s}}})

(def backlight-path "/sys/class/backlight/intel_backlight/brightness")

(defn ensure-sudo!
  "Prompt for sudo upfront and exit on failure."
  []
  (try
    (check (shell ["sudo" "-v"]))
    (println "Sudo authenticated.")
    (catch Exception _
      (println "!! Failed to authenticate sudo.")
      (System/exit 1))))

(defn set-brightness
  [amount]
  (println "Setting the brightness to" amount)
  (check
    (process
      ["sudo" "tee" backlight-path]
      {:in (str amount "\n")})))

(def current
  (let [content (slurp backlight-path)]
    (parse-long (str/trim content))))

(defn increase
  [amount]
    (let [result (+ current amount)]
    (cond
      (>= result 192000) 192000 ;; let's hope that this makes sense
      :else result)))

(defn decrease
  [amount ]
  (let [result (- current amount)]
    (cond
      (< result 0) 0
      :else result)))1

(defn -main
  [& args]
  (ensure-sudo!)
  (let [opts (cli/parse-opts args cli-opts)]
    (if (fs/exists? backlight-path)
      (cond
        (:increase opts) (set-brightness (increase 25000))
        (:decrease opts) (set-brightness (decrease 25000))
        (:set opts) (set-brightness (parse-long (second args)))
        :else (println "Usage: battery.clj [--daemon|-d] [--notify|-n]")))))
    
(apply -main *command-line-args*)
