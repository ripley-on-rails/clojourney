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

(defn- find-item [game name]
  (let [player-location-id (get-in game [:player :location])
        item-in-location (item-by-name-in-location game name player-location-id)
        item-in-inventory (item-by-name-in-inventory game name)]
    {:item (or item-in-location item-in-inventory)
     :location-id (if item-in-location
                    player-location-id
                    (if item-in-inventory
                      :inventory))}))

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

(defn idp [x] (prn x) x)

(defn- transfer-item-from-location-to-inventory [game item location-id]
  (-> game
      (remove-item item location-id)
      (add-item-to-inventory item)))


(defn pickup [game {:keys [target]}]
  (let [{:keys [location-id item]} (find-item game target)]
    (cond
      (not item)
      {:failure true
       :message (format "There is no such item %s here. " target)}
      
      (= location-id :inventory)
      {:failure true
       :message (format "Player already has %s in inventory. " target)}

      (:takeable item)
      {:game (transfer-item-from-location-to-inventory game item location-id)
       :message (format "Player takes %s and puts it into their inventory. %s"
                        target
                        (:on-take-desc item))}

      :else
      {:failure true
       :message (:on-take-desc item)})))

;; eat

(defn decrease-hunger [game]
  (let [state (get-in game [:player :state])]
    (if (state "hungry")
      {:game (update-in game [:player :state] #(disj % "hungry"))
       :additional-desc "Player stops feeling hungry. "}
      {:game game})))

(defn eat [game {:keys [target]}]
  (let [{:keys [location-id item]} (find-item game target)]
    (cond
      (not item)
      {:failure true
       :message (format "There is no such item %s here. " target)}

      (:edible item)
      (let [{:keys [game additional-desc]}
            (-> game
                (remove-item item :inventory)
                decrease-hunger)]
        {:game game
         :message (str (format "Player eats %s. " target)
                       additional-desc)})

      :else
      {:failure true
       :message (format "%s is not edible" target)})))


;; talk

(defn talk [game {:keys [target]}]
  (let [{:keys [location-id item]} (find-item game target)]
    (cond
      (not item)
      {:failure true
       :message (format "There is no %s to talk to. " target)}

      (:on-talk item)
      {:message (:on-talk item)}

      :else
      {:failure true
       :message (format "The player does not know how to talk to %s or what to talk about with it. " target)})))

;; attack
(defn attack [game {:keys [target with]}]
  (let [{target-location-id :location-id
         target-item :item} (find-item game target)
        {with-location-id :location-id
         with-item :item} (find-item game with)]
    (prn "attack")
    (prn [target-location-id target-item with-location-id with-item])
    (cond
      (not target-item)
      {:failure true
       :message (format "There is no %s to attack. " target)}

      (not with-item)
      {:failure true
       :message (format "You don't have %s to attack %s with. " with target)}

      (not= :inventory with-location-id)
      {:message (format "You don't have %s on you. But there's one near by. " with)}


      
      :else
      {:failure true}
      )))

;; kiss
(defn kiss [game {:keys [target]}]
  (let [{:keys [location-id item]} (find-item game target)]
    (cond
      (not item)
      {:failure true
       :message (format "There is no %s to kiss. " target)}

      (= "dragon" target)
      {:message "Say literally: 'You kiss the dragon and you liked it.'"}

      :else
      {:message (format "The player kisses %s." target)})))

;; redirect
(defn redirect [sentence]
  (fn [_ _]
    {:redirect true
     :sentence sentence}))

;; Unmatched

(defn unmatched [game {:keys [verb target with]}]
  {:failure true
   :game game})
