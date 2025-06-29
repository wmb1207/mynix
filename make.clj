#!/usr/bin/env bb

;;(ns my-nix
(require '[babashka.process :refer [shell check]])
(require '[babashka.fs :as fs])

(def black "#000000")
(def green "#a6e3a1")
(def red "#C68689")
(def dark-gray "#101010")
(def white "#ffffff")
(def font "Iosevka Term")
(def templates-folder "templates")
(def assets-folder "assets")
(def transparency "88")


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
  "Run `nixos-rebuild switch --flake .#host` on the given host via sudo,
   using `shell` so we always get a proper {:exit :out :err} map."
  [host]
  (let [cmd ["sudo" "nixos-rebuild" "switch" "--flake" (str ".#" host)]
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
              [(->TemplateField "{{background}}" green)
               (->TemplateField "{{foreground}}" dark-gray)
               (->TemplateField "{{focused-background}}" red)
               (->TemplateField "{{font}}" font)]))

(def bspwmrc
  (->Template "bspwmrc"
              (str assets-folder "/bspwmrc")
              (slurp (str "./" templates-folder "/bspwmrc.tmpl"))
              [(->TemplateField "{{background}}" (str "\\" green))
               (->TemplateField "{{normal-background}}" (str "\\" red))]))

(def sxhkdrc
  (->Template "sxhkdrc"
              (str assets-folder "/sxhkdrc")
              (slurp (str "./" templates-folder "/sxhkdrc.tmpl"))
              [(->TemplateField "{{background}}" green)
               (->TemplateField "{{font}}" font)
               (->TemplateField "{{selected-foreground}}" white)
               (->TemplateField "{{foreground}}" dark-gray)]))

(def ghostty
  (->Template "ghostty"
              (str assets-folder "/ghostty")
              (slurp (str "./" templates-folder "/ghostty.tmpl"))
              [(->TemplateField "{{background}}" black)
               (->TemplateField "{{transparency}}" transparency)
               (->TemplateField "{{font}}" font)]))

(def dunstrc
  (->Template "dunstrc"
              (str assets-folder "/dunstrc")
              (slurp (str "./" templates-folder "/dunstrc.tmpl"))
              [(->TemplateField "{{background}}" black)
               (->TemplateField "{{green}}" green)
               (->TemplateField "{{red}}" red)
               (->TemplateField "{{white}}" white)
               (->TemplateField "{{transparency}}" transparency)
               (->TemplateField "{{font}}" font)]))

(def emacs
  (->Template "emacs"
              (str assets-folder "/init.el")
              (slurp (str "./" templates-folder "/init.el.tmpl"))
              [(->TemplateField "{{transparency}}" transparency)]))

;; (def eww
;;   (->Template "eww"
;;               (str assets-folder "/eww.scss")
;;               (slurp (str "./" template-folder "/eww.scss"))
;;               []))

(defn main
  [& args]
  (apply-tmpls! [polybar bspwmrc sxhkdrc ghostty emacs dunstrc])
  (ensure-sudo!)
  (remove-init-el)
  (apply-flake (first args)))

(apply main *command-line-args*)
