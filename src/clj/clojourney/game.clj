(ns clojourney.game
  (:require [clojourney.ai :as ai]
            [clojure.edn :as edn]
            [clojourney.actions :as actions]
            [clojourney.util :as util]
            [taoensso.timbre :as timbre :refer [info]]
            [schema.core :as s]))

(defn- wrap-schema-elemnt [elem name]
  (s/one
   (cond
     (= elem :any) s/Any
     (or (string? elem)
         (nil? elem)) (s/eq elem)
     :else (throw (new Exception (str "Unknown value: " elem " of type " (type elem)))))
   name))

(defn- verb-target-with->schema [pattern]
  (mapv wrap-schema-elemnt
        pattern
        ["verb" "target" "with"]))

(defn combine-schemata
  [& schemata]
  (let [predicates (mapv #(fn [x] (nil? (s/check % x))) schemata)
        schema-pairs (interleave predicates schemata)]
    (apply s/conditional schema-pairs)))

(defn instructions->schemata [instructions]
  (apply array-map
         (mapcat (fn [[pattern outcome]]
                   (if (vector? (first pattern))
                     [(apply combine-schemata
                             (map verb-target-with->schema pattern))
                      outcome]
                     (let [new-pattern (verb-target-with->schema pattern)]
                       [new-pattern outcome])))
                 instructions)))

(def initial-game {:player {:location :orchard
                            :inventory []
                            :state #{"hungry"}}
                   :world {:locations {:orchard
                                       {:id :orchard
                                        :name "Castle's orchard"
                                        :items [{:name "tree"
                                                 :description "ripe apple tree"
                                                 :on-take-desc "It is too big and heavy, silly!"}
                                                {:name "ladder"
                                                 :on-take-desc "Too bulky. "
                                                 :state "leans against tree"}]}
                                       
                                       :orchard-up
                                       {:id :orchard-up
                                        :name "Up on the ladder next to the tree in Castle's orchard"
                                        :items [{:name "tree"
                                                 :description "ripe apple tree"
                                                 :on-take-desc "Even if it was not too big and heavy. How would you want to pick it up when standing on a ladder leaning against it, fool?"}
                                                {:name "apple"
                                                 :edible "true"
                                                 :takeable true
                                                 :state "hangs from the tree"}
                                                {:name "ladder"
                                                 :on-take-desc "Too bulky"
                                                 ;;:state "leaning against tree"
                                                 }]}

                                       :treasure-chamber
                                       {:id :treasure-chamber
                                        :name "Castle's treasure chamber"
                                        :items [{:name "dragon"
                                                 :description "A frolicking dragon that tells the player to kiss them for they are an enchanted dragon."
                                                 :on-take-desc "Player loving tries to lift dragon but realizes it is too heavy. The dragon looks at the player somewhat confused. "}
                                                {:name "sword"}]}}
                           :connections [[:orchard "north" :treasure-chamber "path"]
                                         [:orchard "up" :orchard-up "ladder"]
                                         [:orchard-up "down" :orchard "ladder"]
                                         [:treasure-chamber "south" :orchard "path"]]
                           :directions #{"north" "south" "west" "east" "up" "down"}}
                   :history []
                   :instructions (array-map
                                  ;;["put"]
                                  [["climb" "ladder" nil] 
                                   ["climb" "up" "ladder"]
                                   ["climb" "up" nil] 
                                   ["climb" "ripe apple tree" "ladder"]]
                                  (fn [game _]
                                    (actions/move game {:verb "go"
                                                        :target "up"
                                                        :with "ladder"}))
                                  ;; change state to be on ladder

                                  [["climb" "down" "ladder"]
                                   ["climb" "down" nil]]
                                  (fn [game _]
                                    (actions/move game {:verb "go"
                                                        :target "down"
                                                        :with "ladder"}))

                                  ["eat" "apple" :any] actions/eat

                                  ["sleep" :any :any] actions/unmatched
                                  
                                  ["rest" :any :any] actions/unmatched
                                  
                                  ["go" :any :any]
                                  (fn [game sentence]
                                    (actions/move game sentence))
                                  
                                  ["put" :any :any] actions/unmatched
                                  ["take" :any :any] actions/pickup
                                  
                                  [:any :any :any] actions/unmatched)})

(defonce game-state (atom initial-game))

(defn reset-game! [] (reset! game-state initial-game))

;;(def directions [:north, :south, :west, :east, :up, :down])

(defn describe []
  (let [description @(ai/describe @game-state )]
    (swap! game-state
           (fn [s]
             (update s :history #(conj % description))))
    description))

(defn available-items [game]
  (let [{:keys [player location]} (util/player-location game)]
    (concat (:items player) (:items location))))

(defn available-verbs [game]
  (->> game
       :instructions
       keys
       (mapcat (fn [pattern_s]
                 (if (vector? (first pattern_s))
                   (mapv first pattern_s)
                   [(first pattern_s)])))
       set
       (#(disj % :any))))

(defn available-directions [game]
  (-> game
      :world
      :directions))

(defn- sanitize-map [m]
  (into {} (map (fn [[k v]] [k (if (= v "nil") nil v)]) m)))

(defn- parse-edn-or-nil [s]
  (try
    (edn/read-string s)
    (catch Exception e
      (println "error parsing edn: " s)
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
                             (not (s/check k pattern)))
                           (instructions->schemata available-instructions))))))

(defn idp [x] (prn x) x)

(defn exec-instruction [game sentence instruction]
  (merge {:game game
          :failure false
          :describe? false
          :sentence sentence}
         (instruction game sentence)))

(defn process-user-instructions [command]
  (let [game @game-state
        items (available-items game)
        verbs (available-verbs game)
        directions (available-directions game) ;; todo check nil
        sentence (-> (ai/process-user-instructions command
                                                   items
                                                   verbs
                                                   directions)
                     (#(do
                         (info "parsed user instructions" {:data %})
                         %))
                     sanitize-instructions
                     (#(do
                         (info "saitized instructions" {:data %})
                         %))
                     (ai/normalize-sentence items verbs directions))
        instruction (find-instruction sentence (:instructions game))
        consequence (exec-instruction game sentence instruction)
        ;;consequence (dissoc consequence :game)
        ]
    (reset! game-state (:game consequence))
    ;;(update-in (ai/explain-consequence consequence) [:game] dissoc :instructions)
    (dissoc (ai/explain-consequence consequence) :game)))
