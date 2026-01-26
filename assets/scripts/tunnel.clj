#!/usr/bin/env bb

(ns tunnel
  (:require
   [babashka.process :refer [process]]
   [babashka.cli :as cli]))

(def envs
  {:dev  {:local-port 0
          :db-host ""
          :db-port 0
          :bastion ""
          :user ""}})


(defn start-tunnel! [env]
  (let [{:keys [local-port db-host db-port bastion user]} (envs env)
        proc (process
              ["ssh"
               "-o" "ExitOnForwardFailure=yes"
               "-N"
               "-L" (str local-port ":" db-host ":" db-port)
               (str user "@" bastion)]
              {:inherit true})]
    (println "Tunnel started for" (name env))
    (println "PID:" (:pid proc))
    @proc)) ;; block so SSH keeps running


(defn -main [& args]
  (let [{:keys [env]} (cli/parse-opts args)
        env (keyword env)]
    (when-not (contains? envs env)
      (throw (ex-info "Unknown env"
                      {:env env
                       :available (keys envs)})))
    (start-tunnel! env)))

(apply -main *command-line-args*)
