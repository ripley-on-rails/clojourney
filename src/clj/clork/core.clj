(ns clork.core
  (:require
   [ollama-whisperer.core :as ow]
   [clork.server :refer [new-web-server]]
   [clork.handler :refer [app]]
   [com.stuartsierra.component :as component])
  (:gen-class))

(def host ow/default-host)

(defonce system (atom nil))

(defn init-system []
  (component/system-map
   :server (new-web-server app 3000)))

(defn start-system []
  (reset! system (component/start (init-system))))

(defn stop-system []
  (when @system
    (swap! system component/stop)))

(defn reset-system []
  (stop-system)
  (start-system))

(defn -main [& args]
  (start-system))


