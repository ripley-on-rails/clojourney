(ns clojourney.core
  (:require
   [ollama-whisperer.core :as ow]
   [clojourney.server :refer [new-web-server]]
   [clojourney.handler :refer [app]]
   [com.stuartsierra.component :as component]
   [taoensso.timbre :as timbre
    ;; Optional, just refer what you like:
    :refer [log  trace  debug  info  warn  error  fatal  report
            logf tracef debugf infof warnf errorf fatalf reportf
            spy]]
   [taoensso.timbre.appenders.core :as appenders])
  (:gen-class))

(timbre/set-ns-min-level! #"clojourney.*" :info #_[#{"*"} :debug])

(timbre/merge-config!
 {:appenders {:spit (appenders/spit-appender {:fname "log.txt"})
              :println {:enabled? false}}
  :min-level [["clojourney.*" :info] ["#{\"clojourney.*\"}" :info] ["*" :error]]})

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


