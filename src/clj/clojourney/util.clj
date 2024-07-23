(ns clojourney.util)

(defn player-location [game]
  (let [location-id (get-in game [:player :location])
        location (get-in game [:world :locations location-id])
        exits (->> (get-in game [:world :connections])
                   (filter #(= (first %) (:id location)))
                   (map (fn [connection]
                          {:name (second connection)
                           :via (nth connection 3)})))]
    {:location (assoc location :exits exits)
     :player (:player game)}))
