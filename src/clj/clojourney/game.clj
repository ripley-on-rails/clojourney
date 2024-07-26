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

(def initial-game {:player {:location #_:orchard :treasure-chamber
                            :inventory [{:name "bare hands"}]
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
                                                 :description "A frolicking dragon. They urges you to talk to them/converse with them."
                                                 :on-take-desc "Player loving tries to lift dragon but realizes it is too heavy. The dragon looks at the player somewhat confused. "
                                                 :on-talk #_"The dragon explains that they (the dragon) are an enchanted dragon. It wants the player to kiss them. They look at the player with anticipation. Make the dragon use direct speech here for the explanation. "
                                                 "The dragon says, \"I am an enchanted dragon. Kiss me to set me free!\". They look at the player with anticipation."
                                                 #_ "Make the dragon use direct speech here for the explanation. "
                                                 #_"The dragon says, \"I'm an enchanted dragon. Please kiss me!\". They look at the player with anticipation."}
                                                {:name "sword"
                                                 :takeable true}]}}
                           :connections [[:orchard "north" :treasure-chamber "path"]
                                         [:orchard "up" :orchard-up "ladder"]
                                         [:orchard-up "down" :orchard "ladder"]
                                         [:treasure-chamber "south" :orchard "path"]]
                           :directions #{"north" "south" "west" "east" "up" "down"}}})

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

(def raw-instructions
  (array-map
   ;;["put"]
   [["climb" "ladder" nil] 
    ;;["climb" "up" "ladder"]
    ["climb" "up" nil]
    ["climb" "ripe apple tree" "ladder"]
    ["go" "up" :any]]
   (actions/redirect {:verb "climb"
                      :target "up"
                      :with "ladder"})

   ["climb" "up" "ladder"] actions/move


   [["go" "down" :any]
    ["climb" "down" nil]]
   (actions/redirect {:verb "climb"
                      :target "down"
                      :with "ladder"})

   ["climb" "down" "ladder"] actions/move
   
   ;;["eat" "apple" :any] actions/eat
   ["eat" :any :any] actions/eat

   ["talk" :any :any] actions/talk
   
   ["attack" "dragon" "sword"] actions/attack
   ["attack" "dragon" "bare hands"] actions/attack
   
   ["attack" "dragon" :any]
   (actions/redirect {:verb "attack"
                      :target "dragon"
                      :with "bare hands"})

   ["attack" :any nil]
   (fn [_ {:keys [verb target]}]
     {:redirect true
      :sentence {:verb verb, :target target, :with "bare hands"}})
   
   ["attack" :any :any] actions/attack
   ["kiss" :any :any] actions/kiss

   ["climb" :any :any] actions/move
   ["go" :any :any] actions/move
   
   ["put" :any :any] actions/unmatched
   ["take" :any :any] actions/pickup
   
   [:any :any :any] actions/unmatched))

(def instructions (instructions->schemata raw-instructions))

(defn describe [game-state]
  (let [description @(ai/describe game-state)]
    description))

(defn available-items [game]
  (let [{:keys [player location]} (util/player-location game)]
    (idp
     (concat (:items player) (:items location)))))

(defn available-verbs [game]
  (->> raw-instructions
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
                           available-instructions)))))

(defn exec-instruction [game sentence]
  (let [instruction (find-instruction sentence instructions)
        result (merge {:game game
                       :failure false
                       :redirect false
                       :describe? false
                       :sentence sentence}
                      (instruction game sentence))]
    (if (:redirect result)
      (exec-instruction (:game result) (:sentence result))
      result)))

(defn process-user-instructions [game command]
  (let [items (available-items game)
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
                         (info "sanitized instructions" {:data %})
                         %))
                     (ai/normalize-sentence items verbs directions))
        ;;instruction (find-instruction sentence instructions)
        consequence (exec-instruction game sentence)
        game (:game consequence)]
    ;;(update-in (ai/explain-consequence consequence) [:game] dissoc :instructions)
    {:message (dissoc (ai/explain-consequence consequence) :game)
     :game game}))
