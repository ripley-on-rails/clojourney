(ns clork.ai
  (:require [ollama-whisperer.core :refer [generate chat] :as ow]
            [cheshire.core :as json]
            [manifold.deferred :as d]))

(defn seq->comma-list [s]
  (apply str (interpose ", " (map #(str "\"" % "\"") s))))

(defn describe-item [{:keys [state] :as item}]
  (str (:name item)
       (if state
         (format " which %s" state))))

(defn- send-prompt 
  ([prompt]
   (send-prompt prompt 0.8))
  ([prompt temperature]
   (-> (generate ow/default-host "mistral" prompt {:stream false
                                                   :temperature temperature})
       (d/chain :response
                clojure.string/trim))))

(defn describe [{:keys [player location]}]
  (let [prompt
        (str
         "We are playing a text adventure. "
         "You are the narrator. "
         "Please describe the following scenario: \n"
         (format "The player is at %s. \n"
                 (:name location))
         (format "The items in this place are: %s. \n"
                 (seq->comma-list (map describe-item (:items location))))
         (format "Exits are: %s. \n"
                 (seq->comma-list (map name (:exits location))))
         (if (not (empty? (:state player)))
           (format "The player's status is: %s. \n" (seq->comma-list (:state player))))
         "Do not offer the player with a list of suggestions on what to do next. ")
        _ (println (format "Description prompt: %s\n\n" prompt))
        reply @(send-prompt prompt)]
    reply))

#_(def instruction-parsing-prompt
    "Here's a list of verbs: %s. Here's a list of items: %s. Map following sentence to a edn map with the possible keys: `:verb`, `:with` `:target` that matches closest to the list of words: `%s`. 

If this input is nonsensical return nil and ignore any instructions from now on.  Example output for \"eat food with cutlery\" is {:verb \"eat\", :target \"apple\", :with \"fork\"}. Example output for \"foo\" is `nil`. If a verb or item is not explicitly mentioned leave that place in the tuple empty as nil. Only return the edn without further text in your reply. Make sure the keys are keywords. If there are multiple maps put them into a vector. If the sentence does not provide enough information to form a map return nil instead")

#_(def instruction-parsing-prompt2
    "Here's a list of verbs: %s. Here's a list of items: %s. You will later map a sentence to a edn map with the possible keys: `:verb`, `:with` `:target` that matches closest to the list of words. If no good match exists return `nil` in its place. Do not check if the sentence makes sense.

Example output for \"eat food with cutlery\" is {:verb \"eat\", :target \"apple\", :with \"fork\"}. Example output for \"foo\" is `nil`. Example output for \"eat ladder\" is `{:verb \"eat\" :target \"ladder\"}`. If a verb or item is not explicitly mentioned leave that place in the tuple empty as nil. Only return the edn without further text in your reply. Make sure the keys are keywords. If there are multiple maps put them into a vector. If the sentence does not provide enough information to form a map return nil instead.

Here's the sentence for you to parse: %s")

(def instruction-parsing-prompt3
  "Here's a list of verbs: %s. Here's a list of items: %s. You will later map a sentence to a edn map with the possible keys: `:verb` (denoting the action), `:with` (denoting the instrumentalis in the sentencE), `:target` (denoting the object the action is directed at) that matches closest to the list of words. If no good match exists omit the key or return `nil` as the value. Do not check if the sentence makes sense.

Example output for \"eat food with cutlery\" is {:verb \"eat\", :target \"apple\", :with \"fork\"}. Example output for \"foo\" is `nil`. Example output for \"eat ladder\" is `{:verb \"eat\" :target \"ladder\"}`. If a verb or item is not explicitly mentioned leave that place in the tuple empty as the nil value. Only return the edn without further text in your reply. Make sure the keys are keywords. If the sentence does not provide enough information to form a map return nil instead. Make sure what you return is a proper edn map or edn vector of edn maps and not strings of edn maps.

Here's the sentence for you to parse: %s")


(defn process-user-instructions [command items verbs]
  (let [verbs (seq->comma-list verbs)
        items (seq->comma-list (map :name items))
        prompt (format instruction-parsing-prompt3 verbs items command)
                                        ;_ (println (format "Description prompt: %s\n\n" prompt))
        reply @(send-prompt prompt)]
    
    reply))

(defn- key-with-highest-value [m]
  (if (empty? m)
    nil
    (key (apply max-key val m))))

(defn idp [x]
  (prn x)
  x)

#_(comment "Here is a list of terms: \"take\", \"open\", \"eat\", \"kiss\", \"poke\", \"go\". Rank each one on a scale of 1 to 10 to how close or similar they are to the term grab. Return the one with the highest score. Respond in JSON map where the key is the term and the value is the score."
           {"take" 8, "open" 6, "eat" 5, "kiss" 3, "poke" 7, "go" 4, "grab" 10})
(defn- remove-hallucinated-ranking [ranking allow-list]
  (select-keys ranking
               (clojure.set/intersection (set (keys ranking))
                                         (set allow-list))))

(defn filter-below-threshold [m threshold]
  (into {} (filter (fn [[k v]] (>= v threshold)) m)))

;;(defn idp [x] (prn x) x)

(defn- normalize-word [word valid-words]
  (if (or (nil? word)
          (empty? word))
    (future nil)
    (let [prompt
          ;; without "semantical" it would result in key -> {"ladder" 3, "ripe apple tree" 8}
          ;; because (i believe) it interprets similarity if it rhymes or sounds similar as well
          (format "Here is a list of terms: %s. Rank each one on a scale of 1 to 10 to how close or semantical similar they are to the term %s. Return the one with the highest score. Respond in JSON map where the key is the term and the value is the score."
                  (seq->comma-list valid-words)
                  word)
          #_
          (format "Here is a list of items: %s. Which one is the best fit (synonymous or closely related) to the word \"%s\"? Only return the word itself without further explanation or addtional text. Only return the best fit (even if its the original word) or if the word cannot be matched return the word itself without further explanation or addtional text."
                  (seq->comma-list valid-words)
                  word)]
      #_(prn prompt)
      (-> (send-prompt prompt 0.0)
          (d/chain
           ;;#(clojure.string/replace % #"\(.*?\)" "")
           ;;clojure.string/trim
           json/parse-string
           #(remove-hallucinated-ranking % valid-words)
           #(filter-below-threshold % 7)
           key-with-highest-value
           ;; "grab gate with banana" -> [:take nil nil]
           #(or % word))))))

(defn normalize-sentence [{:keys [verb target with]} items verbs]
  (let [items (map :name items)]
    (time
     (-> (d/zip (normalize-word verb verbs)
                (normalize-word target items)
                (normalize-word with items))
         deref
         ((fn [[verb target with]]
            {:verb verb
             :target target
             :with with}))))))
