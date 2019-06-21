(ns cat-reagent.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
    [reagent.core :as reagent :refer [atom]]
    [reagent.session :as session]
    [reitit.frontend :as reitit]
    [clerk.core :as clerk]
    [accountant.core :as accountant]
    [quil.core :as q :include-macros true]
    [quil.middleware :as m]
    [cljs-http.client :as http]
    [cljs.core.async :refer [<!]]
    [cognitect.transit :as t]
    )
  )
;; Atoms
(defonce user (reagent/atom nil))
(defonce character (reagent/atom nil))
(def s (reagent/atom nil))


;; -------------------------
;; Quil
(def size [800 600])
(def row-length (* (/ (first size) 100) 4))
(def num-rows (* (/ ( second size) 100) 3))
(def keystring-char {"The Cat" :cat "The Caretaker" :caretaker})
(def char-keystring {:cat "The Cat" :caretaker "The Caretaker"})
(defn keynum
  [num]
  (keyword (str num))
)
 
(defn setup 
  []
  (q/smooth)
  (q/frame-rate 30)
  (q/background 000)
  s
  )
(defn update-state
  [s]
  (do (go (let [url (str "http://localhost:3449/get-state?player=" @character)
                response (<! (http/get url {:with-credentials? false}))
                r (t/reader :json)
                ]
            (reset! s (clojure.walk/keywordize-keys (t/read r (:body response))))
            ))
      s
      )
  )
(defn on-key
  [s e]
  (do (go (let [url "http://localhost:3449/play"
                response (<! (http/get (str url "?command=" (:key e) "&player=" @character) {:with-credentials? false}))
                r (t/reader :json)
                ]
            (reset! s (clojure.walk/keywordize-keys (t/read r (:body response))))
            ))
      s
      )
  )

(defn draw-string
  [s x]
  (cond
    (contains? (:board s) (keynum x)) ((keynum x) (:board s))
    :else "."
    )
  )
(defn draw-rows [s x-step y-step]
  (doseq [y (map inc (range (* y-step 4)))]
    (doseq [x (map inc (range (* x-step 4)))]
      (let [x-coord (* x-step (* 4 y))
            y-coord (* y-step (* 4 x))
            ] 
        (do
          (q/text (draw-string s (+ x (* (* x-step 4) (- y 1)))) y-coord x-coord)
          ;(q/text "c" 96 32)
          ;(println (/  (* x-coord y-coord) 4))
          ))
      )
  ))
(defn draw
  [s]
  (let [fsize-tp (/ (first size) 100)
        ssize-tp (/ (second size) 100)
        fsize-np (- (first size) fsize-tp)
        ssize-np (- (second size) ssize-tp)
        ]
    (q/background 000)
    (q/stroke 255 255 255)
    (q/line [fsize-tp ssize-tp] [fsize-np ssize-tp])
    (q/line [fsize-tp ssize-tp] [fsize-tp ssize-np])
    (q/line [fsize-np ssize-tp] [fsize-np ssize-np])
    (q/line [fsize-np ssize-np] [fsize-tp ssize-np])
    (q/text-size 24)
    (do (draw-rows @s fsize-tp ssize-tp))
    ;(q/text "." (* ssize-tp 4) (* fsize-tp 4))
    )
  )

;; World updaters
;(defn make-obstacle
;  "Make an obstacle in row r of length 1 and width w offset from the left by o."
;  [r l w o]
;   (set (map keynum (flatten (map (fn [m] (map (fn [c] (+ c (* dim m))) (map #(+ (+ (* dim (- r 1)) (+ o 1)) %) (range l)))) (range w)))))
;  )
;(def v-walls (clojure.set/union 
;               (make-vwall 8 9 0 6) 
;               (make-vwall 8 4 13 2) 
;               (make-vwall 7 2 18) 
;               (make-vwall 14 2 18) 
;               (make-vwall 2 5 4 1)))
;(def h-walls (clojure.set/union 
;               (make-hwall 9 7 0 3) 
;               (make-hwall 12 7 0 5) 
;               (make-hwall 4 2 0) 
;               (make-hwall 17 6 7 3)))
;(def right-corners (set (map keynum [(+ 8 (* 9 20))])))
;(def closets (set [:161 :373 :393 :394 :395 :396 :397 :398 :399 :400]))
;(def obstacles (clojure.set/union (make-obstacle 2 5 1 1) (make-obstacle 5 1 4 3)))
;(def window-left-rows (set '(2 3)))
;(def window-right-rows (set '(17 18)))
;; -------------------------
;; Routes

(def router
  (reitit/router
   [["/" :index]
    ["/about" :about]
    ["/cat-page" :cat-page]
    ["/care-page" :care-page]
    ]))

(defn path-for [route & [params]]
  (if params
    (:path (reitit/match-by-name router route params))
    (:path (reitit/match-by-name router route))))

(path-for :about)
;; -------------------------
;; Page components

(defn input-element
  "An input element which updates its value on change."
  [id name type value]
  [:input {:id id
           :name name
           :class "form-control"
           :type type
           :required ""
           :value @value
           :on-change #(reset! value (-> % .-target .-value))
           }]
  )
(defn text-input
  [text-type text-atom]
  (input-element text-type text-type text-type text-atom)
  )
(defn home-page
  []
  (fn []
      [:div {:class "login-wrapper"} 
       [:h3 "Welcome to The Cat!"]
       [:div 
        [:div "Please enter your name and the character you would like to play:" 
         [:form  
          [text-input "username" user] 
          ;[:p [text-input "character" character]] 
          [:select.form-control {:field :list :id :many-options :on-click #(reset! character (get keystring-char (-> % .-target .-value)))}
           [:option {:key :cat-page} "The Cat"]
           [:option {:key :caretaker-page} "The Caretaker"]
           ]
          [:p (when (and (not (nil? @user)) (not (nil? @character))) 
                (do (go (let [url (str "http://localhost:3449/get-state?player=" @character)
                              response (<! (http/get url {:with-credentials? false}))
                              r (t/reader :json)
                              ]
                          (reset! s (clojure.walk/keywordize-keys (t/read r (:body response))))
                          )) [:div "Your username is " @user " and you wish to play as " (@character char-keystring) ". Click " [:a {:href (path-for (if (= @character :cat) :cat-page :care-page))} "here"] " to play the game!"]))]
          ]
         ] 
        ] 
       ]   
      )
  )
(defn game-board []
  (reagent/create-class
    {:component-did-mount 
     (fn [component]
       (let [node (reagent/dom-node component)
             width (.-width node)
             height (.-height node)
             ]
         (q/sketch 
           :title "The Cat"
           :host node
           :size size
           ; setup function called only once, during sketch initialization.
           :setup setup
           ; update-state is called on each iteration before draw-state.
           :update update-state
           :draw draw 
           :key-pressed on-key
           ; This sketch uses functional-mode middleware.
           ; Check quil wiki for more info about middlewares and particularly
           ; fun-mode.
           :middleware [m/fun-mode]))) 
     :render (fn [] [:canvas {:width (/ (.-innerWidth js/window) 2) 
                              :height (/ (.-innerHeight js/window) 2)}])
     }
    ) 
  )
(defn cat-page
  []
  (fn []
    [:span.main [:div (str "Welcome to the game! Your name is " @user " and you are playing as the Cat.")]
     [game-board] 
     [:div (:message @s)]
    ]
    )
  )
(defn care-page
  []
  (fn []
    [:span.main [:div (str "Welcome to the game! Your name is " @user " and you are playing as the Caretaker.")]
     [game-board] 
     [:div (:message @s)]
    ]
    )
  )
(defn about-page []
  (fn [] [:span.main
          [:h3 "About The Cat"]
          [:div "The Cat is an interactive text adventure!"]
          ]))


;; -------------------------
;; Translate routes -> page components

(defn page-for [route]
  (case route
    :index #'home-page
    :about #'about-page
    :cat-page #'cat-page
    :care-page #'care-page
    ))


;; -------------------------
;; Page mounting component

(defn current-page []
  (fn []
    (let [page (:current-page (session/get :route))]
      [:div
       [:header
        [:p [:a {:href (path-for :index)} "Home"] " | "
         [:a {:href (path-for :about)} "About cat-reagent"]]]
       [page]
       [:footer
        [:p "cat-reagent was generated by the "
         [:a {:href "https://github.com/reagent-project/reagent-template"} "Reagent Template"] "."]]])))

;; -------------------------
;; Initialize app

(defn mount-root []
  (reagent/render [current-page] (.getElementById js/document "app")))

(defn init! []
  (clerk/initialize!)
  (accountant/configure-navigation!
   {:nav-handler
    (fn [path]
      (let [match (reitit/match-by-path router path)
            current-page (:name (:data  match))
            route-params (:path-params match)]
        (reagent/after-render clerk/after-render!)
        (session/put! :route {:current-page (page-for current-page)
                              :route-params route-params})
        (clerk/navigate-page! path)
        ))
    :path-exists?
    (fn [path]
      (boolean (reitit/match-by-path router path)))})
  (accountant/dispatch-current!)
  (mount-root))
