(ns ^:figwheel-hooks clojourney.view
  (:require
   [goog.dom :as gdom]
   [reagent.core :as reagent :refer [atom]]
   [cljs.core.async :refer [<! go]]
   [cljs-http.client :as http]
   [clojure.string :as str]
   [cljs.reader :as reader]
   [reagent.dom :as rdom]))

(defonce app-state (atom {:history []
                          :text ""
                          :spinner false}))

(defn get-app-element [] 
  (gdom/getElement "app"))

(defn append-to-history! [msgs]
  (swap! app-state
         (fn [s]
           (update s :history #(concat % msgs)))))

(defn idp [x] (prn x) x)

(defn process-server-response [response]
  (cond
    (string? response)
    [{:type :game
      :text response}]

    (map? response)
    (remove (comp nil? :text)
            [{:type :debug
              :text (dissoc response :rapport :description)}
             {:type :game
              :text (:rapport response)} 
             {:type :game
              :text (:description response)}])

    :else
    [{:type :error
      :text "Error!"}]))

(defn show-spinner! []
  (swap! app-state #(assoc % :spinner true)))

(defn hide-spinner! []
  (swap! app-state #(assoc % :spinner false)))

(defn send-input [input]
  (go
    (show-spinner!)
    (let [response (<! (http/post "http://localhost:3000/action"
                                  {:with-credentials? false
                                   :headers {"Accept" "application/edn"}
                                   :edn-params {:input input}}))]
      (append-to-history! (process-server-response (get-in response [:body :message])))
      (hide-spinner!))))

(defn reset-game []
  (go
    (show-spinner!)
    (let [response (<! (http/post "http://localhost:3000/reset-game"
                                  {:with-credentials? false
                                   :headers {"Accept" "application/edn"}
                                   ;;:edn-params {:input "foo"}
                                   }))]
      (swap! app-state #(assoc % :history []))
      (append-to-history! (process-server-response
                           (get-in response [:body :message])))
      (hide-spinner!))))

(defn chat-input [] 
  (let [written-text (atom "")]
    (fn []
      [:input {:type "text"
               :value @written-text
               :on-change #(reset! written-text (.. % -target -value))
               :on-key-press (fn [e]
                               (when (= 13 (.-charCode e))
                                 (.preventDefault e)
                                 (let [text @written-text]
                                   (append-to-history! [{:type :user
                                                         :text text}])
                                   (reset! written-text "")
                                   (send-input text))))}])))

(defn render-paragraphs [text]
  (let [lines (str/split-lines text)]
    (map-indexed (fn [idx line]
                   [:p {:key idx} line]) lines))) 


(defn- history-entry->items [entry]
  (prn entry)
  [entry])

(defn game []
  [:div {:class "content"}
   [:h1 "Clojourney"]
   (map-indexed (fn [idx item]
                  (into [:div {:key idx
                               :class (str "reply " (name (:type item)))}
                         (render-paragraphs (:text item))]))
                (:history @app-state))
   (if (not (:spinner @app-state))
     [:<>
      (let [history? (not (empty? (:history @app-state)))]
        (if (not (empty? (:history @app-state)))
          [:<>
           [:h2 "What do you do?"]
           [:div {:class "input-container"}
            [chat-input]
            [:input {:type "button"
                     :value "Send"
                     :on-click (fn [] (reset-game)) :href "#"}]]]))
      [:input {:type "button"
               :value "Start new game"
               :class "reset-button"
               :on-click (fn [] (reset-game)) :href "#"}]]
     [:span {:class "loader"}])])

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
