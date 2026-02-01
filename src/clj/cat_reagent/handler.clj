(ns cat-reagent.handler
  (:require
    [cat-reagent.game :as g]
    [reitit.ring :as reitit-ring]
    [cat-reagent.middleware :refer [middleware]]
    [hiccup.page :refer [include-js include-css html5]]
    [clojure.data.json :as json]
    [config.core :refer [env]]))

(def mount-target
  [:div#app
   [:h2 "Welcome to cat-reagent"]
   [:p "please wait while Figwheel is waking up ..."]
   [:p "(Check the js console for hints if nothing exciting happens.)"]])

(defn head []
  [:head
   [:meta {:charset "utf-8"}]
   [:meta {:name "viewport"
           :content "width=device-width, initial-scale=1"}]
   (include-css (if (env :dev) "/css/site.css" "/css/site.min.css"))])

(defn loading-page []
  (html5
   (head)
   [:body {:class "body-container"}
    mount-target
    (include-js "/js/app.js")]))
(defn get-parameter 
  [req key-coll] 
  (let [params (:params req)] 
    (if (vector? key-coll)
      (select-keys params key-coll)
      (get params key-coll)
      )))

(defn keystring->keyword
  [keystring]
  (keyword (subs keystring 1))
  )

(defn pass-state
  [s player]
  (let [char-data (get-in s [:characters player])
        loc (:loc char-data)
        opponent (if (= player :cat) :caretaker :cat)
        opp-data (get-in s [:characters opponent])
        opp-loc (:loc opp-data)
        in-combat (= loc opp-loc)]
    {:game-over (:game-over s)
     :turn (:turn s)
     :board (g/map-pos-to-char s player)
     :message (get-in s [:characters player :message])
     :status-message (get-in s [:characters player :status-message])
     :scores (:scores s)
     :patrol-tasks (:patrol-tasks s)
     :turn-number (:turn-number s)
     :cat-treasure (get-in s [:characters :cat :treasure] 0)
     :treasure-total 50
     :caretaker-aware (get-in s [:characters :caretaker :aware] false)
     ;; Noise counter (neighbors get suspicious at 4+)
     :neighbors (:neighbors s)
     ;; Character status
     :arms-bound (:arms char-data)
     :legs-bound (:legs char-data)
     :in-mouth-gag (:in-mouth-mat char-data)
     :over-mouth-gag (:over-mouth-mat char-data)
     :gagged (or (:in-mouth-mat char-data) (:over-mouth-mat char-data))
     :in-combat in-combat
     :has-cursor (some? (:cursor char-data))
     ;; Opponent status (only visible when in combat)
     :opp-arms-bound (when in-combat (:arms opp-data))
     :opp-legs-bound (when in-combat (:legs opp-data))
     :opp-in-mouth-gag (when in-combat (:in-mouth-mat opp-data))
     :opp-over-mouth-gag (when in-combat (:over-mouth-mat opp-data))
     ;; Location info
     :at-phone (contains? (:phone-map s) loc)
     :at-treasure (contains? (:treasure-map s) loc)
     :at-door (contains? (:door-list s) loc)
     :at-sabotaged (contains? (:sabotaged s) loc)}))
(defn pass-play
  [m]
  (let [[command player] (map #(keystring->keyword (val %)) m)] 
  (pass-state (g/play command player) player))
  )
(defn get-state-handler
  [_request]
  {:status 200
   :headers {"Content-Type" "text/json"}
   :body (-> (let [p (partial get-parameter _request)]
               (str (json/write-str (pass-state @g/s (keystring->keyword (p :player)))))
               ))})
(defn restart-handler
  [_request]
  {:status 200
   :headers {"Content-Type" "text/json"}
   :body (-> (let [p (partial get-parameter _request)]
               (str (json/write-str (pass-state (g/restart) (keystring->keyword (p :player)))))
               ))  
   }
  )
(defn play-handler
  [_request]
  {:status 200
   :headers {"Content-Type" "text/json"}
   :body (-> (let [p (partial get-parameter _request)]
               (str (json/write-str (pass-play (p [:command :player]))))
               ))})
(defn index-handler
  [_request]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body (loading-page)})

(def app
  (reitit-ring/ring-handler
   (reitit-ring/router
    [["/" {:get {:handler index-handler}}]
     ["/play"
      ["" {:get {:handler play-handler}}]
      ]
     ["/get-state"
      ["" {:get {:handler get-state-handler}}]
      ]
     ["/restart"
      ["" {:get {:handler restart-handler}}]
      ]
     ["/items"
      ["" {:get {:handler index-handler}}]
      ["/:item-id" {:get {:handler index-handler
                          :parameters {:path {:item-id int?}}}}]]
     ["/about" {:get {:handler index-handler}}]
     ["/cat-page" {:get {:handler index-handler}}]
     ["/care-page" {:get {:handler index-handler}}]])
   (reitit-ring/routes
    (reitit-ring/create-resource-handler {:path "/" :root "/public"})
    (reitit-ring/create-default-handler))
   {:middleware middleware}))
