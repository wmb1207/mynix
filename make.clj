#!/usr/bin/env bb

;;(ns my-nix
(require '[babashka.process :refer [shell check]])
(require '[babashka.cli :as cli])
(require '[babashka.fs :as fs])

(def cli-opts
  {:spec {:clear {:desc "clear all the previous instances"
                  :alias :c}
          :apply {:desc "apply the template"
                  :alias :a}}})

;; Plan 9 / acme inspired colors
(def black "#000000")     ;; absolute black (rio background)
(def white "#e6e6e6")     ;; chalky white (text)
(def green "#5f875f")     ;; muted sage green
(def blue  "#5f87af")     ;; dusty blue (links / selection)
(def red   "#875f5f")     ;; brick / error red
(def dark-gray "#444444") ;; window dividers / inactive
(def cream "#ffffe0")    ;; acme selection background


(def font "DejaVu Sans Mono")
(def templates-folder "templates")
(def assets-folder "assets")
(def transparency "100")

;;(def theme "acme")
(def theme "doric-earth")
(def light-theme "ef-day")

(def ghostty-theme "Wez")
(def ghostty-theme-light "GruvboxLight")

(defrecord TemplateField [^String key
                          ^String value])

(defrecord Template [^String name
                     ^String output
                     ^String content
                     ^clojure.lang.IPersistentCollection fields])

(defn ensure-sudo!
  "Prompt for sudo upfront and exit on failure."
  []
  (try
    (check (shell ["sudo" "-v"]))
    (println "Sudo authenticated.")
    (catch Exception _
      (println "!! Failed to authenticate sudo.")
      (System/exit 1))))

(defn apply-flake
  "Run `nixos-rebuild switch --flake .#host --impure` on the given host via sudo,
   using `shell` so we always get a proper {:exit :out :err} map."
  [host]
  (let [cmd ["sudo" "nixos-rebuild" "switch" "--flake" (str ".#" host) "--upgrade" "--impure"]
        result (try
                 (println "Executing " cmd)
                 (shell cmd)
                 (catch Exception e
                   (println "!! Exception during shell invocation:" (.getMessage e))
                   {:exit 1 :out "" :err (.getMessage e)}))]
    (let [{:keys [exit out err]} result]
      (if (zero? exit)
        (println "Flake applied on" host "\n" out)
        (do
          (println "!! nixos-rebuild failed (exit" exit "):\n" err)
          (System/exit exit))))))

(defn remove-file
  [file]
  (if (fs/exists? file)
    (do
      (println "Removing the file" (str file))
      (let [{:keys [exit err]} (shell ["sudo" "rm" "-f" (str file)])]
        (if (zero? exit)
          (println "Removed" file)
          (println "!!Could not remove" file ":" err))))
    (println "Emacs " file " not found, skipping.")))

(defn remove-init-el
  "Delete the user’s init.el, using sudo if necessary."
  []
  (let [paths [(fs/path "/home/wmb/.emacs.d/init.el")
               (fs/path "/home/wmb/.emacs.d/lisp/packages.el")]]
    (doseq [file paths]
      (remove-file file))))

(defn apply-tmpl
  [tmpl]
  (spit (:output tmpl)
        (reduce (fn [acc field]
                  (str/replace acc (:key field) (:value field)))
                (:content tmpl)
                (:fields tmpl))))

(defn apply-tmpls!
  [tmpls]
  (try
    (doseq [tmpl tmpls]
      (apply-tmpl tmpl))
    (catch Exception e
      (println e)
      (System/exit 3))))

(def polybar
  (->Template "polybar"
              (str assets-folder "/polybar.ini")
              (slurp (str "./" templates-folder "/polybar.ini.tmpl"))
              [(->TemplateField "{{background}}" black)
               (->TemplateField "{{foreground}}" white)
               (->TemplateField "{{focused-background}}" white)
               (->TemplateField "{{focused-foreground}}" black)
               (->TemplateField "{{font}}" font)]))

(def bspwmrc
  (->Template "bspwmrc"
              (str assets-folder "/bspwmrc")
              (slurp (str "./" templates-folder "/bspwmrc.tmpl"))
              [(->TemplateField "{{background}}" (str "\\" white))
               (->TemplateField "{{normal-background}}" (str "\\" black))]))

(def sxhkdrc
  (->Template "sxhkdrc"
              (str assets-folder "/sxhkdrc")
              (slurp (str "./" templates-folder "/sxhkdrc.tmpl"))
              [(->TemplateField "{{background}}" black)
               (->TemplateField "{{font}}" font)
               (->TemplateField "{{selected-foreground}}" black)
               (->TemplateField "{{foreground}}" white)]))

(def ghostty-dark
  (->Template "ghostty"
              (str assets-folder "/ghostty")
              (slurp (str "./" templates-folder "/ghostty.tmpl"))
              [(->TemplateField "{{background}}" black)
               (->TemplateField "{{transparency}}" transparency)
               (->TemplateField "{{theme}}" ghostty-theme)
               (->TemplateField "{{font}}" font)]))

(def ghostty-light
  (->Template "ghostty"
              (str assets-folder "/ghostty")
              (slurp (str "./" templates-folder "/ghostty.tmpl"))
              [(->TemplateField "{{background}}" white)
               (->TemplateField "{{transparency}}" transparency)
               (->TemplateField "{{theme}}" ghostty-theme-light)
               (->TemplateField "{{font}}" font)]))

(def dunstrc
  (->Template "dunstrc"
              (str assets-folder "/dunstrc")
              (slurp (str "./" templates-folder "/dunstrc.tmpl"))
              [(->TemplateField "{{black}}" "#f3efe6")
               (->TemplateField "{{frame}}" "#2a8f8a")
               (->TemplateField "{{green}}" "#f3efe6")
               (->TemplateField "{{red}}" red)
               (->TemplateField "{{white}}" black)
               (->TemplateField "{{transparency}}" transparency)
               (->TemplateField "{{font}}" font)]))

(def emacs-light
  (->Template "emacs"
              (str assets-folder "/init.el")
              (slurp (str "./" templates-folder "/init.el.tmpl"))
              [(->TemplateField "{{transparency}}" transparency)
               (->TemplateField "{{theme}}" light-theme)
               (->TemplateField "{{font}}" font)
               (->TemplateField "{{background}}" white)]))

(def emacs-dark
  (->Template "emacs"
              (str assets-folder "/init.el")
              (slurp (str "./" templates-folder "/init.el.tmpl"))
              [(->TemplateField "{{transparency}}" transparency)
               (->TemplateField "{{theme}}" theme)
               (->TemplateField "{{font}}" font)
               (->TemplateField "{{background}}" black)]))

(defn emacs
  [args]
  (if (some #(= "--light" %) args)
    emacs-light
    emacs-dark))

(defn ghostty
  [args]
  (if (some #(= "--light" %) args)
    ghostty-light
    ghostty-dark))

(defn clear
  []
  (let [cmd ["sudo" "nix-collect-garbage" "-d"]]
    (try
      (println "Executing" cmd)
      (shell cmd)
      (catch Exception e
        (println "!! Exception during shell invocation:" (.getMessage e))
        {:exit 1 :out "" :err (.getMessage e)}))))

(defn run
  [args]
  (println args)
  (apply-tmpls! [polybar bspwmrc sxhkdrc (emacs (second args)) dunstrc])
  (ensure-sudo!)
  (remove-init-el)
  (apply-flake (second args)))

(defn main
  [& args]
  (let [opts (cli/parse-opts args cli-opts)]
    (cond
      (:clear opts) (clear)
      (:apply opts) (run args))))

(apply main *command-line-args*)
