(ns clojourney.actions)

(defn new-location-id [game direction]
  (let [current-location-id (get-in game [:player :location])]
    (some (fn [[start dir end via]]
            (and (= start current-location-id)
                 (= direction dir)
                 {:location-id end
                  :via via}))
          (get-in game [:world :connections]))))

;; move

(defn move [game {:keys [verb target with]}]
  (let [direction target]
    (if ((get-in game [:world :directions]) direction)
      (if-let [{:keys [location-id via]} (new-location-id game direction)]
        (do
          (if (or (= via "ladder")
                  (not ((get-in game [:player :state]) "hungry")))
            {:describe? true
             :game (assoc-in game [:player :location] location-id)}
            {:failure true
             :message "Player is too hungry to go there. "}))
        {:failure true
         :message "Cannot go this way"})
      {:failure true
       :message "Unknown direction"})))

;; take / pickup

(defn- item-by-name-in-location [game name location-id]
  (->> (get-in game [:world :locations location-id :items])
       (filter #(= name (:name %)))
       first))

(defn- item-by-name-in-inventory [game name]
  (->> (get-in game [:player :inventory])
       (filter #(= name (:name %)))
       first))

(defn- remove-item [game item location-id]
  (let [path (if (= location-id :inventory)
               [:player :inventory]
               [:world :locations location-id :items])]
    (update-in game
               path
               (fn [items]
                 (remove #(= item
                             %)
                         items)))))

(defn- add-item-to-inventory [game item]
  (update-in game
             [:player :inventory]
             (fn [inventory]
               (conj inventory item))))

(defn- transfer-item-from-location-to-inventory [game item location-id]
  (-> game
      (remove-item item location-id)
      (add-item-to-inventory item)))

(defn pickup [game {:keys [target]}]
  (let [location-id (get-in game [:player :location])
        item-in-location (item-by-name-in-location game target location-id)
        item-in-inventory (item-by-name-in-inventory game target)]
    (if item-in-location
      (if (:takeable item-in-location)
        {:game (transfer-item-from-location-to-inventory game item-in-location location-id)
         :message (format "Player takes %s and puts it into their inventory. %s"
                          target
                          (:on-take-desc item-in-location))}
        {:failure true
         :message (:on-take-desc item-in-location)})
      (if item-in-inventory
        {:failure true
         :message (format "Player already has %s in inventory. " target)}
        {:failure true
         :message (format "There is no such item %s here. " target)})))
  )

;; eat

(defn decrease-hunger [game]
  (let [state (get-in game [:player :state])]
    (if (state "hungry")
      {:game (update-in game [:player :state] #(disj % "hungry"))
       :additional-desc "Player stops feeling hungry. "}
      {:game game})))

(defn eat [game {:keys [target]}]
  (let [location-id (get-in game [:player :location])
        item-in-location (item-by-name-in-location game target location-id)
        item-in-inventory (item-by-name-in-inventory game target)
        found-item (or item-in-location
                       item-in-inventory)
        found-in (and found-item
                      (or (and item-in-location location-id)
                          :inventory))]
    (if found-item
      (if (:edible found-item)
        (let [{:keys [game additional-desc]}
              (-> game
                  (remove-item item-in-inventory :inventory)
                  decrease-hunger)]
          {:game game
           :message (str (format "Player eats %s. " target)
                         additional-desc)})
        {:failure true
         :message (format "%s is not edible" target)})
      {:failure true
       :message (format "There is no such item %s here. " target)})))

;; Unmatched

(defn unmatched [game {:keys [verb target with]}]
  {:failure true
   :game game})
