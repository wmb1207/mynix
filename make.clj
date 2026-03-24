#!/usr/bin/env bb

;;(ns my-nix
(require '[babashka.process :refer [shell check]])
(require '[babashka.cli :as cli])
(require '[babashka.fs :as fs])

(def cli-opts
  {:spec {:clear {:desc "clear all the previous instances"
                  :alias :c}
          :apply {:desc "apply the template"
                  :alias :a}
          :tmpl {:desc "build the templates img"
                  :alias :t}
          :pi {:desc "build the pi img"
                  :alias :p}
          :iso {:desc "build the iso"
                  :alias :i}}})


;; gloomy creamsody colors
(def black     "#1c1a18")  ;; default background
(def bg-alt    "#252320")  ;; lighter background / modeline / linum
(def selection "#302d29")  ;; selection / region / subtle border
(def dark-gray "#6a6858")  ;; comments / inactive
(def white     "#b5b2a0")  ;; default foreground (muted cream)
(def red       "#884545")  ;; errors / deleted (deep rose)
(def olive     "#8a7040")  ;; strings / keywords / desk highlight (dark amber)
(def green     "#657050")  ;; added / success (muted olive)
(def blue      "#4a6a78")  ;; functions / headings / focus (steel blue)
(def mauve     "#785a5a")  ;; selection alias (dusty mauve)
(def cream     "#9a9888")  ;; types / subtle foreground (warm grey)


(def font "DejaVu Sans Mono")
(def templates-folder "templates")
(def assets-folder "assets")
(def transparency "100")

;;(def theme "acme")
;;(def theme "gruber-darker")
;;(def theme "base16-ashes")
;;(def theme "mbo70s")
(def theme "creamsody-darker")
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

(defn build-iso
  "Run the nixos build to generate an iso"
  [host]
  (let [cmd ["nix" "build" (str ".#nixosConfigurations." host ".config.system.build.isoImage") "--impure"]
        result (try
                 (println "Executing " cmd)
                 (shell cmd)
                 (catch Exception e
                   (println "!! Exception during shell invocation:" (.getMessage e))
                   {:exit 1 :out "" :err (.getMessage e)}))]
    (let [{:keys [exit out err]} result]
      (if (zero? exit)
        (println "buliding done for host: " host "\n" out)
        (do
          (println "!! nixos build failed (exit" exit "):\n" err)
          (System/exit exit))))))

(defn build-pi
  "Run the nixos build to generate an img ready to be used with my pi3b+"
  [host]
  (let [cmd ["nix" "build" (str ".#nixosConfigurations." host ".config.system.build.sdImage")]
        result (try
                 (println "Executing " cmd)
                 (shell cmd)
                 (catch Exception e
                   (println "!! Exception during shell invocation:" (.getMessage e))
                   {:exit 1 :out "" :err (.getMessage e)}))]
    (let [{:keys [exit out err]} result]
      (if (zero? exit)
        (println "buliding done for host: " host "\n" out)
        (do
          (println "!! nixos build failed (exit" exit "):\n" err)
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
  (println "Applying templates")
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
               (->TemplateField "{{focused-background}}" blue)
               (->TemplateField "{{focused-foreground}}" black)
               (->TemplateField "{{font}}" font)]))

(def bspwmrc
  (->Template "bspwmrc"
              (str assets-folder "/bspwmrc")
              (slurp (str "./" templates-folder "/bspwmrc.tmpl"))
              [(->TemplateField "{{active}}" (str "\\" blue))
               (->TemplateField "{{normal-border}}" (str "\\" selection))]))

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

(def fvwm3
  (->Template "fvwm3"
              (str assets-folder "/fvwm3.conf")
              (slurp (str "./" templates-folder "/fvwm3.conf.tmpl"))
              [(->TemplateField "{{background}}"  black)
               (->TemplateField "{{foreground}}"  white)
               (->TemplateField "{{bg-alt}}"      bg-alt)
               (->TemplateField "{{selection}}"   selection)
               (->TemplateField "{{active}}"      blue)
               (->TemplateField "{{active-alt}}"  cream)
               (->TemplateField "{{comments}}"    dark-gray)
               (->TemplateField "{{green}}"       green)
               (->TemplateField "{{olive}}"       olive)
               (->TemplateField "{{theme}}"       theme)
               (->TemplateField "{{font}}"        font)]))

(def dunstrc
  (->Template "dunstrc"
              (str assets-folder "/dunstrc")
              (slurp (str "./" templates-folder "/dunstrc.tmpl"))
              [(->TemplateField "{{black}}" bg-alt)
               (->TemplateField "{{frame}}" blue)
               (->TemplateField "{{green}}" green)
               (->TemplateField "{{red}}" red)
               (->TemplateField "{{white}}" white)
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

(defn stage-assets
  "Stage generated asset files so nix flake picks up the changes."
  []
  (let [cmd ["git" "add"
             "assets/init.el"
             "assets/bspwmrc"
             "assets/sxhkdrc"
             "assets/dunstrc"
             "assets/polybar.ini"
             "assets/fvwm3.conf"]]
    (println "Staging assets:" cmd)
    (shell cmd)))

(defn activate-home-manager
  "Re-run the home-manager activation script for the current user."
  []
  (let [activate (str (System/getenv "HOME") "/.local/state/home-manager/gcroots/current-home/activate")
        result (try
                 (println "Running home-manager activation:" activate)
                 (shell [activate])
                 (catch Exception e
                   (println "!! Exception during home-manager activation:" (.getMessage e))
                   {:exit 1 :out "" :err (.getMessage e)}))]
    (let [{:keys [exit out err]} result]
      (if (zero? exit)
        (println "Home-manager activation done.\n" out)
        (do
          (println "!! home-manager activation failed (exit" exit "):\n" err)
          (System/exit exit))))))

(defn run
  [args]
  (println args)
  (apply-tmpls! [polybar bspwmrc sxhkdrc (emacs (second args)) dunstrc fvwm3])
  (stage-assets)
  (ensure-sudo!)
  (remove-init-el)
  (apply-flake (second args))
  (activate-home-manager))

(defn main
  [& args]
  (let [opts (cli/parse-opts args cli-opts)]
    (cond
      (:iso opts) (build-iso (second args))
      (:pi opts) (build-pi (second args))
      (:tmpl opts) (apply-tmpls! [polybar bspwmrc sxhkdrc (emacs (second args)) dunstrc fvwm3])
      (:clear opts) (clear)
      (:apply opts) (run args))))

(apply main *command-line-args*)
