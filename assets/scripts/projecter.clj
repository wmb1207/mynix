#!/usr/bin/env bb

(ns projecter
  (:require [babashka.cli :as cli]
            [babashka.fs :as fs]
            [clojure.string :as str]))

(def allowed-templates #{"php" "python" "nodejs" "raw"})

(def cli-opts
  {:spec
   {:name {:desc "project name"
           :alias :n}

    :template {:desc "projects base template"
               :alias :t
               :validate allowed-templates
               :validate-msg (str "Template must be one of: "
                                  (str/join ", " allowed-templates))}

    :path {:desc "projects root"
           :alias :p}}})

(defrecord Template [^String name
                     ^String template
                     ^clojure.lang.IPersistentCollection tags])

(def template-path "/home/wmb/.local/templates/")
(def scripts-path "/home/wmb/.local/bin/")
(def scripts ["tunnel" ""])


(def templates
  {"php" (->Template "php"
                     "php.nix"
                     ["php" "laravel" "nodejs"])
   "python" (->Template "python"
                        "python.nix"
                        ["python" "uv" "pip"])
   "nodejs" (->Template "nodejs"
                        "nodejs.nix"
                        ["nodejs" "js" "ts"])
   "raw"   (->Template "raw"
                       "raw.nix"
                       ["raw" "agnostic"])})

(defn create-project
  [template path]
  (let [dir (fs/path (fs/cwd) path)]
    (when-not (fs/exists? dir)
      (fs/create-dirs dir))
    (fs/copy (fs/path template-path (:template template))
             (fs/path dir (:template template)))
    (doseq [i scripts] (fs/copy (fs/path (str scripts-path i "clj"))
                                (fs/path dir (:template template))))
    (println "New dev shell ready to be used")
    (println "Run: cd" dir)))


(defn create!
  [{:keys [template path]} templates]
  (let [tpl (get templates template)]
    (when-not tpl
      (throw (ex-info "Unknown template" {:template template})))
    (create-project tpl path)))

(defn main
  [& args]
  (let [opts (cli/parse-opts args cli-opts)]
    (create! opts templates)))

(apply main *command-line-args*)
