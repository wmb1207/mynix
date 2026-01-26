#!/usr/bin/env bb

(require '[babashka.cli :as cli]
         '[babashka.process :refer [shell check process]]
         '[clojure.string :as str])

(defrecord XrandrDisplay [^String key
                          ^clojure.lang.IPersistentCollection resolutions])

(def cli-opts
  {:spec {:position {:desc "The position of the connected screen"
                     :alias :p}
          :keyboard {:desc"The keyboard physical distribution"
                     :alias :k}}})

(def xrandr-output
  (:out (shell {:out :string} "xrandr")))

(defn parse-xrandr
  [input]
  (let [lines (str/split-lines input)]
    (loop [xs lines
           current nil
           acc []]
      (if-let [l (first xs)]
        (cond
          (re-find #" connected" l)
          (let [display (first (str/split l #" "))]
            (recur (rest xs)
                   {:key display :resolutions []}
                   (if current (conj acc current) acc)))

          ;; resolution line under current
          (and current (re-find #"^\s+[0-9]+x[0-9]+" l))
          (let [res (second (re-find #"^\s+([0-9]+x[0-9]+)" l))]
            (recur (rest xs)
                   (update current :resolutions conj res)
                   acc))

          :else
          (recur (rest xs) current acc))

        ;; EOF
        (cond-> acc
          current (conj current))))))

(defn ->xrandr-display [parsed]
  (map #(map->XrandrDisplay %) parsed))

(defn xrandr-commands
  [position display]
  (let [base ["xrandr" "--output" "eDP-1"]]  ; Fixed: removed extra dash
    (print display)
    [{:cmd
      (into base
            (case position
              "left" ["--mode" "2560x1600" "--left-of" (:key display) "--rotate" "normal"
                      "--output" (:key display) "--rotate" "normal"
                      "--mode" (first (:resolutions display))]
              "right" ["--mode" "2560x1600" "--right-of" (:key display) "--rotate" "normal"
                       "--output" (:key display) "--rotate" "normal"
                       "--mode" (first (:resolutions display))]
              "bottom" ["--mode" "2560x1600" "--below" (:key display) "--rotate" "normal"
                        "--output" (:key display) "--rotate" "normal"
                        "--mode" (first (:resolutions display))]
              "top" ["--mode" "2560x1600" "--above" (:key display) "--rotate" "normal"
                     "--output" (:key display) "--rotate" "normal"
                     "--mode" (first (:resolutions display))]
              "external" ["--off" "--output" (:key display) "--rotate" "normal"
                          "--mode" (first (:resolutions display))]
              "laptop" ["--mode" "2560x1600"]))
      :bg false}]))

(defn bspc-commands
  [position display]
  (case position
    "external" [{:cmd ["bspc" "monitor" "eDP-1" "-r"] :bg false}
                {:cmd ["bspc" "monitor" (:key display) "-d" "(first)" "(second)" "(third)" "(fourth)" "(fifth)"] :bg false}]
    "laptop"   [{:cmd ["bspc" "monitor" (:key display) "-r"] :bg false}
                {:cmd ["bspc" "monitor" "eDP-1" "-d" "(first)" "(second)" "(third)" "(fourth)" "(fifth)"] :bg false}]
    [{:cmd ["bspc" "monitor" (:key display) "-d" "(first)" "(second)" "(third)"] :bg false}
     {:cmd ["bspc" "monitor" "eDP-1" "-d" "(fourth)" "(fifth)"] :bg false}]))

(defn polybar-commands
  [position]
  (case position
    "external" [{:cmd ["bash" "~/.local/bin/polybar.sh" "external"] :bg true}]
    "laptop"   [{:cmd ["bash" "~/.local/bin/polybar.sh" "laptop"] :bg true}]
    [{:cmd ["bash" "~/.local/bin/polybar.sh" "any"] :bg true}]))

(defn keyboard-setup-commands
  [keyboard]
  (case keyboard
    "qwerty" [{:cmd ["setxkbmap" "-layout" "us" "-variant" "dvorak"
                     "-option" "ctrl:nocaps" "-option" "altwin:meta"
                     "-option" "ctrl:swap_lalt_lctl"] :bg false}]
    "dvorak" [{:cmd ["setxkbmap" "-layout" "us"] :bg false}]
    []))

(defn filter-internal-display
  [displays]
  (filter #(not= "eDP-1" (:key %)) displays))  ; Fixed: compare :key

(defn config-displays
  [position displays]
  (let [external (first (filter-internal-display displays))]
    (if (= position "laptop")
      (xrandr-commands position nil)
      (xrandr-commands position external))))

(defn config-bspc
  [position displays]
  (let [external (first (filter-internal-display displays))]
    (bspc-commands position external)))

(defn shell!
  [cmds]
  (doseq [{:keys [cmd bg]} (remove nil? cmds)]
    (println "Executing:" cmd)
    (let [result (try 
                   (process (str/join " " cmd)  {:bg bg :detach bg :out :string})
                   (catch Exception e
                     (println "!!Exception during shell invocation:" (.getMessage e))))]
      (println result))))

(defn handler
  [opts displays]
  (let [cmds (concat 
              (config-displays (:position opts) displays)
              (config-bspc (:position opts) displays)
              (keyboard-setup-commands (:keyboard opts))
              [{:cmd ["bspc" "wm" "-r"] :bg false}] 
              (polybar-commands (:position opts)))]
    (shell! cmds)))  ; Pass all commands at once
  
(defn -main
  [& args]
  (let [displays (-> xrandr-output
                     parse-xrandr
                     ->xrandr-display)
        opts (cli/parse-opts args cli-opts)]
    (println (handler opts displays))))

(apply -main *command-line-args*)

(set-mouse-color "olive")
