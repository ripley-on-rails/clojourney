(ns clork.game
  (:require [clork.ai :as ai]
            [clojure.edn :as edn]
            [schema.core :as s]))

(defn- wrap-schema-elemnt [elem name]
  (s/one
   (cond
     (= elem :any) s/Any
     (string? elem) (s/eq elem)
     :else (throw (str "Unknown value: " elem)))
   name))

(defn instructions->schemata [instructions]
  (into (array-map)
        (map (fn [[pattern outcome]]
               (let [new-pattern (mapv wrap-schema-elemnt pattern
                                       ["verb" "target" "with"])]
                 [new-pattern outcome]))
             instructions)))

#_(defn instruction->schema [instruction]
    (let [new-pattern (mapv wrap-schema-elemnt pattern
                            ["verb" "target" "with"])]
      new-pattern))

(def initial-game {:player {:location :orchard
                            :inventory []
                            :state ["hungry"]}
                   :world {:locations [{:id :orchard
                                        :name "Castle's orchard"
                                        :items [{:name "ripe apple tree"}
                                                {:name "ladder"
                                                 :state "is on ground"}]}
                                       {:id :courtyard
                                        :name "Castle's courtyard"}]
                           :connections [[:orchard :north :courtyard]
                                         [:courtyard :south :orchard]]}
                   :history []
                   :instructions (array-map
                                  ["take" "ladder" :any] :foo
                                  ["climb" "ripe apple tree" "ladder"] :bar)})

(defonce game-state (atom initial-game))

(defn reset-game! [] (reset! game-state initial-game))

(def directions [:north, :south, :west, :east, :up, :down])

(defn player-location [game]
  (let [locations (get-in game [:world :locations])
        location (first (filter (fn [loc]
                                  (= (loc :id)
                                     (get-in game [:player :location])))
                                locations))
        exits (->> (get-in game [:world :connections])
                   (filter #(= (first %) (:id location)))
                   (map second))]
    {:location (assoc location :exits exits)
     :player (:player game)}))

(defn describe []
  (let [description (ai/describe (player-location @game-state))]
    (swap! game-state
           (fn [s]
             (update s :history #(conj % description))))
    description))

(defn available-items [game]
  (let [{:keys [player location]} (player-location game)]
    (concat (:items player) (:items location))))

(defn available-verbs [game]
  (->> game
       :instructions
       keys
       (map first)))

(defn- sanitize-map [m]
  (into {} (map (fn [[k v]] [k (if (= v "nil") nil v)]) m)))

(defn- parse-edn-or-nil [s]
  (try
    (edn/read-string s)
    (catch Exception e
      (prn e)
      nil)))

(defn- sanitize-instructions [i]
  (let [edn (parse-edn-or-nil i)]
    (cond
      (map? edn)
      (sanitize-map edn)

      ;; ollama sometimes returns a string of string of edn
      (string? edn)
      (parse-edn-or-nil edn)
      
      :else
      (do (prn "unparsable map: " edn)
          nil))))

(defn- find-instruction [{:keys [verb target with]} available-instructions]
  (let [pattern [verb target with]]
    (second (first (filter (fn [[k v]]
                             (prn [k v])
                             (s/check k pattern))
                           (instructions->schemata available-instructions))))))

(defn process-user-instructions [command]
  (let [game @game-state
        items (available-items game)
        verbs (available-verbs game)
        #_ ["take" "open" "eat" "kiss" "poke" "go"]
        reply (-> (ai/process-user-instructions command
                                                items
                                                verbs)
                  sanitize-instructions
                  (ai/normalize-sentence items verbs))
        instruction (find-instruction reply (:instructions game))]
    (find-instruction reply (:instructions game))
    reply))
