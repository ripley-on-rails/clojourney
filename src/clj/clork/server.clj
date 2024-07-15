(ns clork.server
  (:require [com.stuartsierra.component :as component]
            [ring.adapter.jetty :refer [run-jetty]]))

(defrecord HttpServer [handler port]
  component/Lifecycle
  (start [this]
    (let [server (run-jetty handler {:port port :join? false})]
      (assoc this :server server)))
  (stop [this]
    (when-let [server (:server this)]
      (.stop server))
    (assoc this :server nil)))

(defn new-web-server [handler port]
  (map->HttpServer {:handler handler :port port}))
