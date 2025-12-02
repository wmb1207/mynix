#!/usr/bin/env bb

;; LISP
;; LISt Processor

(require '[babashka.cli :as cli]
         '[babashka.process :refer [shell check]]
         '[babashka.fs :as fs])

(def cli-opts
  {:spec {:daemon {:desc "Run the process as a daemon"
                  :alias :d}
          :notify {:desc "trigger the notification"
                   :alias :n}}})
  
(def battery-path "/sys/class/power_supply/BAT0/capacity")

(defn notify-message
  [percentage]
  (str "Battery level at " percentage "/100"))

(defn notify
  [message]
  (let [cmd ["notify-send" message]
        result (try (shell cmd)
                    (catch Exception e
                      (println "!! Exception catch during shell invocation" (.getMessage e))
                      {:exit 1 :out "" :err (.getMessage e)}))]))
  
(defn notify-battery
  [percentage state]
  (let [p (Integer. percentage)]
    (println percentage)
    (if-not (= percentage state)
      (cond
        (< p 15) (notify (notify-message percentage))
        (= 0 (mod p 5)) (notify (notify-message percentage))))))

(defn daemon []
  (println "Starting the battery daemon")
  (loop [state nil]
    (let [percentage (Integer/parseInt (clojure.string/trim (slurp battery-path)))]
      (notify-battery percentage state)
      (Thread/sleep 10000)
      (recur percentage))))

(defn -main
  [& args]
  (let [opts (cli/parse-opts args cli-opts)]
    (if (fs/exists? battery-path)
      (cond
        (:daemon opts) (daemon)
        (:notify opts) (let [percentage (Integer/parseInt (clojure.string/trim (slurp battery-path)))]
                         (notify (notify-message percentage)))
        :else (println "Usage: battery.clj [--daemon|-d] [--notify|-n]")))))
    
(apply -main *command-line-args*)
