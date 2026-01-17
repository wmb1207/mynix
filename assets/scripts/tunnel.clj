#!/usr/bin/env bb

(ns tunnel
  (:require
   [babashka.process :refer [process]]
   [babashka.cli :as cli]))

(def envs
  {:dev  {:local-port 54321
          :db-host "warc-development-rds-postgres.cfko0ca8atxw.eu-north-1.rds.amazonaws.com"
          :db-port 5432
          :bastion "BastionWarcDEV"
          :user "lao"}

   :prod {:local-port 54322
          :db-host "warc-production-rds-postgres.cfko0ca8atxw.eu-north-1.rds.amazonaws.com"
          :db-port 5432
          :bastion "BastionWarc"
          :user "lao"}})


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
