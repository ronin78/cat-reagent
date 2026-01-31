(ns cat-reagent.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
    [reagent.core :as reagent :refer [atom]]
    [reagent.session :as session]
    [reitit.frontend :as reitit]
    [clerk.core :as clerk]
    [accountant.core :as accountant]
    [cljs-http.client :as http]
    [cljs.core.async :refer [<!]]
    [cognitect.transit :as t]
    [clojure.walk :as walk]))
;; Atoms
(defonce user (reagent/atom nil))
(defonce character (reagent/atom nil))
(def s (reagent/atom nil))


;; -------------------------
;; Game rendering constants and helpers
(def grid-cols 32)
(def grid-rows 18)

(def keystring-char {"The Cat" :cat "The Caretaker" :caretaker})
(def char-keystring {:cat "The Cat" :caretaker "The Caretaker"})

(defn keynum
  [num]
  (keyword (str num)))

;; CSS color map (hex strings instead of RGB arrays)
(def css-colors
  {:green "#32cd32"
   :blue "#0000ff"
   :white "#ffffff"
   :orange "#ffa500"
   :grey "#808080"
   :black "#000000"
   :yellow "#ffff00"
   :cyan "#00ffff"
   :red "#ff0000"})

;; State refresh function
(defn refresh-state []
  (when @character
    (go (let [url (str "http://localhost:3449/get-state?player=" @character)
              response (<! (http/get url {:with-credentials? false}))
              r (t/reader :json)]
          (reset! s (walk/keywordize-keys (t/read r (:body response))))))))

;; Map browser key names to game command format
(def key-map
  {"ArrowUp"    ":up"
   "ArrowDown"  ":down"
   "ArrowLeft"  ":left"
   "ArrowRight" ":right"})

(defn translate-key [k]
  (or (get key-map k)
      (str ":" k)))

;; Keyboard handler
(defn handle-keypress [e]
  (when-not (.-repeat e)  ; Ignore key repeat events
    (let [key (translate-key (.-key e))]
      (when @character
        (go (let [url (str "http://localhost:3449/play?command=" key "&player=" @character)
                  response (<! (http/get url {:with-credentials? false}))
                  r (t/reader :json)]
              (reset! s (walk/keywordize-keys (t/read r (:body response))))))))))

;; Store interval ID for cleanup
(defonce refresh-interval (atom nil))

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
          [:select.form-control {:field :list :id :many-options :on-change #(reset! character (get keystring-char (-> % .-target .-value)))}
           [:option {:key :select :value ""} "-- Select Character --"]
           [:option {:key :cat-page} "The Cat"]
           [:option {:key :caretaker-page} "The Caretaker"]]
          [:p (when (and (not (nil? @user)) (not (nil? @character)))
                (do (go (let [url (str "http://localhost:3449/get-state?player=" @character)
                              response (<! (http/get url {:with-credentials? false}))
                              r (t/reader :json)
                              ]
                          (reset! s (walk/keywordize-keys (t/read r (:body response))))
                          )) [:div "Your username is " @user " and you wish to play as " (@character char-keystring) ". Click " [:a {:href (path-for (if (= @character :cat) :cat-page :care-page))} "here"] " to play the game!"]))]
          ]
         ]
        ]
       ]
      )
  )
(defn game-board []
  (let [board (:board @s)
        default-color (if (= :cat @character) "#000000" "#808080")]
    [:div.game-grid
     (for [row (range grid-rows)]
       ^{:key row}
       [:div.game-row
        (for [col (range grid-cols)]
          (let [pos (+ 1 col (* row grid-cols))
                [char color] (get board (keyword (str pos)))
                display-char (or char ".")
                ;; Make the cat bright lime green so it stands out
                display-color (cond
                                (and (= char "t") (= @character :cat)) "#32cd32"
                                (= char "r") "#ff6b6b"  ; Caretaker in coral red
                                char (get css-colors (keyword color) "#808080")
                                :else default-color)]
            ^{:key col}
            [:span.game-cell {:style {:color display-color}} display-char]))])]))

;; Symbol legend - colors chosen for visibility in the key
(def symbol-legend
  [["t" "#32cd32" "Cat"]
   ["r" "#ff6b6b" "Caretaker"]
   ["$" "#32cd32" "Treasure"]
   ["\u00b6" "#87ceeb" "Phone"]
   ["\u250f" "#daa520" "Door"]
   ["|" "#b0b0b0" "Wall"]
   ["_" "#b0b0b0" "Wall"]
   ["," "#a0a0a0" "Footprint"]
   ["?" "#ffff00" "Disturbed"]
   ["!" "#00ffff" "Patrol target"]
   ["x" "#ff0000" "Sabotaged"]
   ["X" "#ff69b4" "Combat!"]
   ["\u2588" "#da70d6" "Cursor"]])

(defn symbol-key []
  [:div {:style {:padding "10px" :background "#1a1a1a" :border-radius "5px"}}
   [:div {:style {:font-weight "bold" :margin-bottom "5px" :color "#fff"}} "Symbols"]
   [:div {:style {:font-size "11px" :font-family "monospace"}}
    (for [[sym color desc] symbol-legend]
      ^{:key sym} [:div {:style {:display "flex" :align-items "center" :margin "2px 0"}}
                   [:span {:style {:color color :margin-right "8px" :width "12px" :text-align "center"}} sym]
                   [:span {:style {:color "#aaa"}} desc]])]])

(defn character-status []
  (let [arms (:arms-bound @s)
        legs (:legs-bound @s)
        gagged (:gagged @s)
        in-combat (:in-combat @s)]
    [:div {:style {:margin-bottom "10px" :padding "10px" :background "#1a1a1a" :border-radius "5px"}}
     [:div {:style {:font-weight "bold" :margin-bottom "5px" :color "#fff"}} "Status"]
     [:div {:style {:font-size "13px"}}
      (if (or arms legs gagged in-combat)
        [:div
         (when in-combat [:div {:style {:color "#ff69b4"}} "IN COMBAT!"])
         (when arms [:div {:style {:color "#ff6b6b"}} (str "Arms bound (" arms ")")])
         (when legs [:div {:style {:color "#ff6b6b"}} (str "Legs bound (" legs ")")])
         (when gagged [:div {:style {:color "#ff6b6b"}} (str "Gagged: " gagged)])]
        [:div {:style {:color "#32cd32"}} "Free"])]]))

(defn location-info []
  (let [at-phone (:at-phone @s)
        at-treasure (:at-treasure @s)
        at-door (:at-door @s)
        at-sabotaged (:at-sabotaged @s)]
    [:div {:style {:margin-bottom "10px" :padding "10px" :background "#1a1a1a" :border-radius "5px"}}
     [:div {:style {:font-weight "bold" :margin-bottom "5px" :color "#fff"}} "Location"]
     [:div {:style {:font-size "13px"}}
      (if (or at-phone at-treasure at-door at-sabotaged)
        [:div
         (when at-phone [:div {:style {:color "#87ceeb"}} "Phone here"])
         (when at-treasure [:div {:style {:color "#32cd32"}} "Treasure here"])
         (when at-door [:div {:style {:color "#daa520"}} "Door here"])
         (when at-sabotaged [:div {:style {:color "#ff0000"}} "Sabotaged!"])]
        [:div {:style {:color "#666"}} "Empty space"])]]))

(defn available-actions [player]
  (let [my-turn (= (keyword (:turn @s)) player)
        in-combat (:in-combat @s)
        has-cursor (:has-cursor @s)
        at-phone (:at-phone @s)
        at-treasure (:at-treasure @s)
        at-door (:at-door @s)
        at-sabotaged (:at-sabotaged @s)
        arms (:arms-bound @s)
        legs (:legs-bound @s)
        gagged (:gagged @s)
        aware (:caretaker-aware @s)
        cat-treasure (or (:cat-treasure @s) 0)
        treasure-total (or (:treasure-total @s) 50)
        can-move (or (nil? legs) (< legs 9))  ;; Can move unless legs are heavily bound
        ;; Build available actions based on context
        base-actions (cond-> []
                       ;; Movement
                       (and my-turn can-move (not has-cursor))
                       (conj {:key "m" :desc "Start moving" :color "#32cd32"})

                       (and my-turn has-cursor)
                       (conj {:key "arrows" :desc "Select destination" :color "#da70d6"}
                             {:key "m" :desc "Confirm move" :color "#32cd32"})

                       (and my-turn legs)
                       (conj {:key "l" :desc "Struggle (legs)" :color "#ffff00"})

                       (and my-turn arms)
                       (conj {:key "a" :desc "Struggle (arms)" :color "#ffff00"})

                       (and my-turn gagged)
                       (conj {:key "g" :desc "Remove gag" :color "#ffff00"}))

        ;; Cat-specific actions
        cat-actions (when (= player :cat)
                      (cond-> []
                        (and my-turn at-treasure (not in-combat))
                        (conj {:key "t" :desc "Take treasure" :color "#32cd32"})

                        (and my-turn (or at-phone at-door) (not at-sabotaged) (not in-combat))
                        (conj {:key "s" :desc "Sabotage" :color "#ff6b6b"})

                        (and my-turn at-door (>= cat-treasure treasure-total))
                        (conj {:key "escape!" :desc "WIN THE GAME!" :color "#ffd700"})))

        ;; Caretaker-specific actions
        caretaker-actions (when (= player :caretaker)
                           (cond-> []
                             (and my-turn at-phone aware (not at-sabotaged))
                             (conj {:key "c" :desc "Call 911" :color "#ff0000"})

                             (and my-turn at-sabotaged)
                             (conj {:key "f" :desc "Fix sabotage" :color "#87ceeb"})

                             (and my-turn aware)
                             (conj {:key "y" :desc (if gagged "Yell (muffled)" "Yell for help") :color "#ffff00"})))

        ;; Combat actions
        combat-actions (when (and my-turn in-combat)
                        [{:key "A" :desc "Bind arms" :color "#ff69b4"}
                         {:key "L" :desc "Bind legs" :color "#ff69b4"}
                         {:key "G" :desc "Gag" :color "#ff69b4"}
                         {:key "u" :desc "Muffle" :color "#ff69b4"}])

        all-actions (concat base-actions cat-actions caretaker-actions combat-actions)]

    [:div {:style {:margin-bottom "10px" :padding "10px" :background "#1a1a1a" :border-radius "5px"}}
     [:div {:style {:font-weight "bold" :margin-bottom "5px" :color (if my-turn "#32cd32" "#ff6b6b")}}
      (if my-turn "Your Turn - Actions:" "Opponent's Turn")]
     [:div {:style {:font-size "13px"}}
      (if (and my-turn (seq all-actions))
        (for [{:keys [key desc color]} all-actions]
          ^{:key (str key desc)}
          [:div {:style {:margin "3px 0"}}
           [:span {:style {:color color :font-family "monospace" :font-weight "bold" :margin-right "8px"}} key]
           [:span {:style {:color "#ccc"}} desc]])
        (when-not my-turn
          [:div {:style {:color "#666"}} "Waiting..."]))]]))

(defn side-panel [player]
  [:div {:style {:min-width "250px" :margin-left "15px"}}
   [character-status]
   [location-info]
   [available-actions player]
   [symbol-key]])

(defn cat-page
  []
  (reagent/create-class
    {:component-did-mount
     (fn [_]
       (.addEventListener js/document "keydown" handle-keypress)
       (reset! refresh-interval (js/setInterval refresh-state 500)))
     :component-will-unmount
     (fn [_]
       (.removeEventListener js/document "keydown" handle-keypress)
       (when @refresh-interval
         (js/clearInterval @refresh-interval)
         (reset! refresh-interval nil)))
     :reagent-render
     (fn []
       (let [scores (:scores @s)
             cat-treasure (or (:cat-treasure @s) 0)
             treasure-total (or (:treasure-total @s) 50)
             caretaker-aware? (:caretaker-aware @s)]
         [:div.main
          ;; Game over banner
          (when (:game-over @s)
            [:div {:style {:background "#333" :padding "15px" :margin-bottom "10px" :border-radius "5px" :text-align "center"}}
             [:div {:style {:font-size "18px" :color (if (= (:game-over @s) :cat) "#32cd32" "#ff6b6b")}}
              (str "Game Over! The " (if (= (:game-over @s) :cat) "Cat" "Caretaker") " won!")]
             [:input {:type "button" :value "New Game"
                      :style {:margin-top "10px" :padding "8px 16px" :cursor "pointer"}
                      :on-click #(go (let [url (str "http://localhost:3449/restart?player=" @character)
                                           response (<! (http/get url {:with-credentials? false}))
                                           r (t/reader :json)]
                                       (reset! s (walk/keywordize-keys (t/read r (:body response))))))}]])

          ;; Main layout: game board + side panel
          [:div {:style {:display "flex" :align-items "flex-start"}}
           ;; Left: Game board and messages
           [:div
            [game-board]
            [:div {:style {:margin-top "10px" :max-width "640px" :padding "10px" :background "#1a1a1a" :border-radius "5px"}}
             [:div {:style {:font-weight "bold" :margin-bottom "5px" :color "#87ceeb"}} "Last Action:"]
             [:div {:style {:color "#fff" :font-size "14px"}} (or (:message @s) "—")]
             (when (:status-message @s)
               [:div {:style {:color "#ffff00" :margin-top "5px" :font-style "italic"}} (:status-message @s)])]]

           ;; Right: Side panel
           [side-panel :cat]]

          ;; Bottom stats bar
          [:div {:style {:display "flex" :gap "15px" :margin-top "10px" :padding "8px" :background "#1a1a1a" :border-radius "5px" :flex-wrap "wrap" :font-size "13px"}}
           [:div {:style {:color (if (= cat-treasure treasure-total) "#32cd32" "#ffd700")}}
            (str "Treasure: " cat-treasure "/" treasure-total)
            (when (= cat-treasure treasure-total) " - ESCAPE!")]
           [:div {:style {:color (if caretaker-aware? "#ffa500" "#32cd32")}}
            (if caretaker-aware? "AWARE!" "Unaware")]
           [:div {:style {:color "#aaa"}}
            (str "Score: Cat " (or (:cat scores) 0) " / Caretaker " (or (:caretaker scores) 0))]]]))}))

(defn care-page
  []
  (reagent/create-class
    {:component-did-mount
     (fn [_]
       (.addEventListener js/document "keydown" handle-keypress)
       (reset! refresh-interval (js/setInterval refresh-state 500)))
     :component-will-unmount
     (fn [_]
       (.removeEventListener js/document "keydown" handle-keypress)
       (when @refresh-interval
         (js/clearInterval @refresh-interval)
         (reset! refresh-interval nil)))
     :reagent-render
     (fn []
       (let [patrol-tasks (:patrol-tasks @s)
             pending-count (count patrol-tasks)
             current-task (first patrol-tasks)
             scores (:scores @s)
             aware? (:caretaker-aware @s)]
         [:div.main
          ;; Game over banner
          (when (:game-over @s)
            [:div {:style {:background "#333" :padding "15px" :margin-bottom "10px" :border-radius "5px" :text-align "center"}}
             [:div {:style {:font-size "18px" :color (if (= (:game-over @s) :cat) "#ff6b6b" "#32cd32")}}
              (str "Game Over! The " (if (= (:game-over @s) :cat) "Cat" "Caretaker") " won!")]
             [:input {:type "button" :value "New Game"
                      :style {:margin-top "10px" :padding "8px 16px" :cursor "pointer"}
                      :on-click #(go (let [url (str "http://localhost:3449/restart?player=" @character)
                                           response (<! (http/get url {:with-credentials? false}))
                                           r (t/reader :json)]
                                       (reset! s (walk/keywordize-keys (t/read r (:body response))))))}]])

          ;; Main layout: game board + side panel
          [:div {:style {:display "flex" :align-items "flex-start"}}
           ;; Left: Game board and messages
           [:div
            [game-board]
            [:div {:style {:margin-top "10px" :max-width "640px" :padding "10px" :background "#1a1a1a" :border-radius "5px"}}
             [:div {:style {:font-weight "bold" :margin-bottom "5px" :color "#87ceeb"}} "Last Action:"]
             [:div {:style {:color "#fff" :font-size "14px"}} (or (:message @s) "—")]
             (when (:status-message @s)
               [:div {:style {:color "#ffff00" :margin-top "5px" :font-style "italic"}} (:status-message @s)])]]

           ;; Right: Side panel
           [side-panel :caretaker]]

          ;; Bottom stats bar
          [:div {:style {:display "flex" :gap "15px" :margin-top "10px" :padding "8px" :background "#1a1a1a" :border-radius "5px" :flex-wrap "wrap" :font-size "13px"}}
           [:div {:style {:color (if aware? "#ff0000" "#666")}}
            (if aware? "AWARE - Intruder detected!" "Unaware")]
           [:div {:style {:color "#00ffff"}}
            (str "Tasks: " pending-count)
            (when current-task (str " - " (:name current-task)))]
           [:div {:style {:color "#aaa"}}
            (str "Score: Cat " (or (:cat scores) 0) " / Caretaker " (or (:caretaker scores) 0))]]]))}))

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
