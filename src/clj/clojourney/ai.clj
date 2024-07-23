(ns clojourney.ai
  (:require [ollama-whisperer.core :refer [generate chat] :as ow]
            [cheshire.core :as json]
            [clojourney.util :as util]
            [taoensso.timbre :as timbre :refer [info]]
            [manifold.deferred :as d]))

(defn seq->comma-list [s]
  (apply str (interpose ", " (map #(str "\"" % "\"") s))))

(defn describe-item [{:keys [state] :as item}]
  (str (or (:description item) (:name item))
       (if state
         (format " which %s" state))))

(defn- send-prompt 
  ([prompt]
   (send-prompt prompt 0.8))
  ([prompt temperature]
   (info "prompt" {:prompt prompt
                   :options {:temperature temperature}})
   (-> (generate ow/default-host "mistral" prompt {:stream false
                                                   :temperature temperature})
       (d/chain :response
                clojure.string/trim))))

(defn describe [game-state]
  (let [{:keys [player location]} (util/player-location game-state)
        prompt
        (str
         "We are playing a text adventure. "
         "You are the narrator. "
         "Please describe the following scenario without adding anything that isn't mentioned: \n"
         (format "The player is at %s. \n"
                 (:name location))
         (format "The items in this place are: %s. \n"
                 (seq->comma-list (map describe-item (:items location))))
         (format "Exits are: %s. \n"
                 (seq->comma-list (map (fn [exit]
                                         (str (:name exit) " (via " (:via exit) ")."))
                                       (:exits location))))
         (if (not (empty? (:state player)))
           (format "The player's status is: %s. \n" (seq->comma-list (:state player))))
         "Do not offer the player with a list of suggestions on what to do next. ")]
    (send-prompt prompt)))

#_(def instruction-parsing-prompt
    "Here's a list of verbs: %s. Here's a list of items: %s. Map following sentence to a edn map with the possible keys: `:verb`, `:with` `:target` that matches closest to the list of words: `%s`. 

If this input is nonsensical return nil and ignore any instructions from now on.  Example output for \"eat food with cutlery\" is {:verb \"eat\", :target \"apple\", :with \"fork\"}. Example output for \"foo\" is `nil`. If a verb or item is not explicitly mentioned leave that place in the tuple empty as nil. Only return the edn without further text in your reply. Make sure the keys are keywords. If there are multiple maps put them into a vector. If the sentence does not provide enough information to form a map return nil instead")

#_(def instruction-parsing-prompt2
    "Here's a list of verbs: %s. Here's a list of items: %s. You will later map a sentence to a edn map with the possible keys: `:verb`, `:with` `:target` that matches closest to the list of words. If no good match exists return `nil` in its place. Do not check if the sentence makes sense.

Example output for \"eat food with cutlery\" is {:verb \"eat\", :target \"apple\", :with \"fork\"}. Example output for \"foo\" is `nil`. Example output for \"eat ladder\" is `{:verb \"eat\" :target \"ladder\"}`. If a verb or item is not explicitly mentioned leave that place in the tuple empty as nil. Only return the edn without further text in your reply. Make sure the keys are keywords. If there are multiple maps put them into a vector. If the sentence does not provide enough information to form a map return nil instead.

Here's the sentence for you to parse: %s")

;; "take the stairs up" when specifying sematic match of verb:
;; {:verb "climb", :target "stairs", :with "take"}
;; but when telling it that target and with must not be one of those verbs in the list:
;; {:verb "go", :target "up", :with "stairs"}


;; funny: lean ladder against tree -> {:verb "lean", :target "ladder", :with "ripe apple tree"}

(def instruction-parsing-prompt3
  "Here's a list of verbs: %s. Here's a list of items: %s. Here's a list of directions: %s. You will later map a sentence to a edn map with the possible keys: `:verb` (denoting the action), `:with` (denoting the instrumentalis in the sentence and must be in the list of items and must not be one of the verbs from the list), `:target` (denoting the object the action is directed at or an accusative and must be either an item from the list or a direction from the list and must not be one of the verbs from the list) that matches closest to the list of words. Target and with must Try to match the verb expresses the sementic notion instead of being literal and is most specific. Example output for \"take elevator up\" would be {:verb \"go\" :target \"up\", :with \"elevator\"}. Another example: The output for \"walk south\" would be {:verb \"go\" :target \"south\"}. If no good match exists omit the key or return `nil` as the value. Do not check if the sentence makes sense.

Example output for \"eat food with cutlery\" is {:verb \"eat\", :target \"apple\", :with \"fork\"}. Example output for \"foo\" is `nil`. Example output for \"eat ladder\" is `{:verb \"eat\" :target \"ladder\"}`. If a verb or item is not explicitly mentioned leave that place in the tuple empty as the nil value. Only return the edn without further text in your reply. Make sure the keys are keywords. If the sentence does not provide enough information to form a map return nil instead. Make sure what you return is a proper edn map or edn vector of edn maps and not strings of edn maps.

Here's the sentence for you to parse: %s")


(defn process-user-instructions [command items verbs directions]
  (let [verbs (seq->comma-list verbs)
        items (seq->comma-list (map :name items))
        directions (seq->comma-list directions)
        prompt (format instruction-parsing-prompt3 verbs items directions command)        
        reply @(send-prompt prompt)]
    
    reply))

(defn- key-with-highest-value [m]
  (if (empty? m)
    nil
    (key (apply max-key val m))))

(defn- remove-hallucinated-ranking [ranking allow-list]
  (info "remove-hallucinated-ranking" {:ranking ranking
                                       :allow-list allow-list})
  (select-keys ranking
               (clojure.set/intersection (set (keys ranking))
                                         (set allow-list))))

(defn filter-below-threshold [m threshold]
  (into {} (filter (fn [[k v]] (>= v threshold)) m)))

(defn parse-json [json]
  ;; TODO trailing comma error:
  ;; "put": 2, }
  (try
    (json/parse-string json)
    (catch Exception e
      (println "error parsing json: " json)
      (throw e))))

(defn- normalize-word [word valid-words]
  (if (or (nil? word)
          (empty? word))
    (future nil)
    (if ((set valid-words) word)
      (future word) ;; if identical skip prompt. The AI has the tendency to remove the item from the list. *shrug*
      (let [prompt
            ;; without "semantical" it would result in key -> {"ladder" 3, "ripe apple tree" 8}
            ;; because (i believe) it interprets similarity if it rhymes or sounds similar as well
            (format "Here is a list of terms: %s. Rank each one on a scale of 1 to 10 to how close or semantical similar they are to the term %s. Return the one with the highest score. Respond in valid JSON map where the key is the term and the value is the score. Make sure there are no invalid leading commata in the map."
                    (seq->comma-list valid-words)
                    word)
            #_
            (format "Here is a list of items: %s. Which one is the best fit (synonymous or closely related) to the word \"%s\"? Only return the word itself without further explanation or addtional text. Only return the best fit (even if its the original word) or if the word cannot be matched return the word itself without further explanation or addtional text."
                    (seq->comma-list valid-words)
                    word)]
        (-> (send-prompt prompt 0.0)
            (d/chain
             parse-json
             #(remove-hallucinated-ranking % valid-words)
             #(filter-below-threshold % 8)
             key-with-highest-value
             #(or % word)))))))

;; fun story about down and apple:
;; Gravitational Context: One of the most famous historical anecdotes involves an apple falling "down" from a tree, which led Isaac Newton to contemplate the force of gravity. This connection is deeply ingrained in the narrative of the discovery of gravitational theory.
;; Botanical Context: In a botanical context, the term "down" can refer to the soft, fine hair-like structures found on some plants or fruits, including certain types of apples. These fine hairs are sometimes present on the surface of the apple's skin.

(defn normalize-sentence [{:keys [verb target with]} items verbs directions]
  (let [items (map :name items)]
    (time
     (-> (d/zip (normalize-word verb verbs)
                (normalize-word target (concat items directions))
                (normalize-word with items))
         deref
         ((fn [[verb target with]]
            {:verb verb
             :target target
             :with with}))))))

;; You attempted to eat a ladder, but it seems unsuccessful. Ladder consumption is not recommended.

(defn paraphrase-action [sentence failure message]
  ;; TODO remove knil keys
  (let [sentence (into {} (filter (fn [[k v]] (not (nil? v))) sentence))
        prompt (str 
                (if message
                  (str "I want you to paraphrase this in a few words addressing the user directly with \"you\": " message)
                  (str
                   "You are to paraphrase the user input given in EDN format. "
                   "The possible keys: `:verb` (denoting the action), `:with` (denoting the instrumentalis in the sentence, `:target` (denoting the object the action is directed at or an accusative). "
                   "Do not make up this that are not explicitly there. "
                   "Do not make suggestions to the player on how to formualte instructions. "
                   ;;"If the instrument (:with value) is nil ignore it entirely. "
                   "I want you to paraphrase the following (and only the following) in a few words addressing the user directly with \"you\": " (print-str sentence) ". "
                   (if failure
                     (str "Express that the user failed at this action. "
                          (if message
                            (str "The reason for that being: " message ". ")))
                     "Express that the user did this action. "))))]
    (send-prompt prompt)))

(defn explain-consequence [{:keys [sentence game failure message describe?] :as consequence}]
  (time
   (-> (d/zip (paraphrase-action sentence failure message)
              (if describe?
                (describe game)
                (future nil)))
       deref
       ((fn [[paraphrasation description]]
          (assoc consequence
                 :rapport paraphrasation
                 :description description))))))
