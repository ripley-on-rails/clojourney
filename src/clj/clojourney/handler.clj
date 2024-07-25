(ns clojourney.handler
  (:require [compojure.core :refer [defroutes GET POST]]
            [compojure.route :as route]
            [ring.util.response :refer [response content-type]]
            [clojure.edn :as edn]
            [clojourney.game :as game]
            [ring.middleware.format-response :refer [wrap-format-response]]
            [ring.middleware.cors :refer [wrap-cors]]
            [ring.middleware.anti-forgery :refer :all]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]))

(defn idp [x] (prn x) x)

(defroutes app-routes
  (POST "/action" {:keys [body]}
    (let [reply (game/process-user-instructions (:game body) (:input body))]
      (-> (response reply)
          (content-type "application/edn"))))
  (POST"/reset-game" _
    (do
      (let [game game/initial-game]
        (-> (response {:message (game/describe game)
                       :game game})
            (content-type "application/edn")))))
  (route/not-found "Not Found"))


(defn wrap-edn-request
  [handler]
  (fn [request]
    (let [body (slurp (:body request))]
      (handler (assoc request :body (edn/read-string body))))))

(defn wrap-edn-response
  [handler]
  (fn [request]
    (let [response (handler request)]
      (if (map? (:body response))
        (-> response
            (assoc :body (pr-str (:body response)))
            (content-type "application/edn"))
        response))))

(defn error-logger [handler]
  (fn [request]
    (try
      (let [response (handler request)]
        response)
      (catch Exception e
        (prn e)
        (throw e)))))

(def app
  (-> #'app-routes
      error-logger
      wrap-edn-response
      wrap-edn-request
      (wrap-cors :access-control-allow-origin [#".*"]
                 ;;:access-control-allow-origin [#"http://localhost:9500"]
                 ;;:access-control-allow-origin [#"^(http(s)?://)?localhost:(\d){4}$"]
                 :access-control-allow-headers ["Content-Type" "Authorization"]
                 :access-control-allow-methods [:get :put :post :delete])
      (wrap-defaults (assoc-in site-defaults [:security :anti-forgery] false))
      
      
      ;; wrap-anti-forgery
      #_(fr/wrap-clojure-response )
      #_wrap-format-response
      #_ (fr/wrap-clojure-response)))
