(ns ^:figwheel-hooks clork.view
  (:require
   [goog.dom :as gdom]
   [reagent.core :as reagent :refer [atom]]
   [cljs.core.async :refer [<! go]]
   [cljs-http.client :as http]
   [clojure.string :as str]
   [cljs.reader :as reader]
   [reagent.dom :as rdom]))

(defonce app-state (atom {:history []
                          :text ""}))

(defn get-app-element [] 
  (gdom/getElement "app"))

(defn append-to-history! [msg]
  (prn "!")
  (swap! app-state
         (fn [s]
           (update s :history #(conj % msg)))))

(defn send-input [input]
  (go (let [response (<! (http/post "http://localhost:3000/action"
                                    {:with-credentials? false
                                     :headers {"Accept" "application/edn"}
                                     :edn-params {:input input}}))]
        (append-to-history! {:type :game
                             :text (get-in response [:body :message])})
        (prn (:body response)))))

(defn reset-game []
  (go (let [response (<! (http/post "http://localhost:3000/reset-game"
                                    {:with-credentials? false
                                     :headers {"Accept" "application/edn"}
                                     :edn-params {:input "foo"}}))]
        (swap! app-state
               (fn [s]
                 (assoc s :history [{:type :game
                                     :text (get-in response [:body :message])}]))))))


(defn chat-input [] 
  (let [written-text (atom "")]
    (fn []
      [:p [:input {:type "text"
                   :value @written-text
                   :on-change #(reset! written-text (.. % -target -value))
                   :on-key-press (fn [e]
                                   (when (= 13 (.-charCode e))
                                     (.preventDefault e)
                                     (let [text @written-text]
                                       (append-to-history! {:type :user
                                                            :text text})
                                       (reset! written-text "")
                                       (send-input text))))}]])))

(defn render-paragraphs [text]
  (let [lines (str/split-lines text)]
    (map-indexed (fn [idx line]
                   [:p {:key idx} line]) lines))) 


(defn game []
  [:div {:class "content"}
   [:h1 "Clork"]
   (map-indexed (fn [idx item]
                  (into [:div {:key idx
                               :class (str "reply " (name (:type item)))}
                         (render-paragraphs (:text item))]))
                (:history @app-state))
   (if (not (empty? (:history @app-state)))
     [:h2 "What do you do?"
      [chat-input]])
   [:input {:type "button"
            :value "Start new game"
            :on-click (fn [] (reset-game)) :href "#"}]])

(defn mount [el]
  (rdom/render [game] el))

(defn mount-app-element []
  (when-let [el (get-app-element)]
    (mount el)))

;; conditionally start your application based on the presence of an "app" element
;; this is particularly helpful for testing this ns without launching the app
(mount-app-element)

;; specify reload hook with ^:after-load metadata
(defn ^:after-load on-reload []
  (mount-app-element)
  ;; optionally touch your app-state to force rerendering depending ond
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )
