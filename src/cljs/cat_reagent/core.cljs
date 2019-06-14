(ns cat-reagent.core
  (:require
    [reagent.core :as reagent :refer [atom]]
    [reagent.session :as session]
    [reitit.frontend :as reitit]
    [clerk.core :as clerk]
    [accountant.core :as accountant]
    [quil.core :as q :include-macros true]
    [quil.middleware :as m]
    [loom.graph :as graph]
    [loom.alg :as alg]
    )
  )

;;Setup constants
(def dim 20)
(def size [800 600])
(def row-length (* (/ (first size) 100) 4))
(def num-rows (* (/ ( second size) 100) 3))
(def keystring-char {"The Cat" :cat "The Caretaker" :caretaker})
(def char-keystring {:cat "The Cat" :caretaker "The Caretaker"})

;; Setup functions
(defn keynum
  [num]
  (keyword (str num))
)
(defn drop-nth [n coll] 
  (keep-indexed #(if (not= %1 n) %2) coll)
 )  
   
;; Board creation
(defn make-edges
  "Make edges from grid."
  ([size]
  (let [sv (vec (map #(+ 1 %) (range (* (/ (first size) 100) (/ (second size) 100) 12))))
        space-set (set sv)
        adj-vec [-1 +1 -32 +32]
        dir-vec [:left :right :up :down]
        adj-fn (fn [s]
                 (map #(+ % s) adj-vec)
                 )
        make-link (fn [s]
                    ;(map #(cond (contains? space-set %1) [(keynum s) (keynum %1) {:direction %2}]) (adj-fn s) dir-vec)
                    (map #(cond (contains? space-set %1) [(keynum s) (keynum %1)]) (adj-fn s) dir-vec)
                    )
        ]
    (vec (filter #(not (nil? %)) (mapcat make-link sv)))
    ))
  ([s w]
   (let [sv (vec (map #(+ 1 %) (range (* (/ (first s) 100) (/ (second s) 100) 12))))
         space-set (set sv)
        adj-vec [-1 +1 -32 +32]
        dir-vec [:left :right :up :down]
        adj-fn (fn [s]
                 (map #(+ % s) adj-vec)
                 )
        make-link (fn [s]
                    (map #(cond (contains? space-set %1) [(keynum s) (keynum %1) {:direction %2 :weight w}]) (adj-fn s) dir-vec)
                    )
        ]
    (vec (filter #(not (nil? %)) (mapcat make-link sv)))
    )
   )
  )
(defn cut-edge
  [g e]
  (graph/remove-edges g e (reverse e))
  )
(defn cut-edges
  [g l]
  (if (empty? l) 
    g
    (recur (cut-edge g (first l)) (next l))
    )
  )
(defn cut-borders
  [g]
  (let [row-ends (mapv #(* row-length %) (range 1 (+ 1 num-rows)))
        cut (mapv #(vector (keynum %) (keynum (+ 1 %))) row-ends)
        ]
    (cut-edges g cut)
    )
  )
(defn make-vwall 
  "Make a vertical wall in column c of length 1 offset from the top by o, with a hole in d."
  ([c l o]
   (set (map #(keynum (+ (* row-length %) c)) (range o (+ o l)))))
  ([c l o d]
   (set (map #(keynum (+ (* row-length %) c)) (drop-nth (- d 1) (range o (+ o l))))) 
   )
  )
(defn make-hwall
  "Make a horizontal wall in row r of length 1 offset from the left by o, with a hole in d."
  ([r l o]
   (set (map #(keynum (+ (+ (* row-length (- r 1)) (+ o 1)) %)) (range l)))
   )
  ([r l o d]
   (set (drop-nth (- d 1) (map #(keynum (+ (+ (* row-length r) (+ o 1)) %)) (range l))))
   )
  )


;; -------------------------
;; Constants
(def g (cut-borders (apply graph/digraph (make-edges size))))
(def vwalls [(make-vwall 3 4 4)
             (make-vwall 5 2 10)
             ])
(def hwalls [(make-hwall 3 4 4)
             (make-hwall 5 2 10)
             ])
(def position-map {:check :576})

;; Atoms
(def user (reagent/atom nil))
(def s (reagent/atom {:board position-map :turn :cat :move-edges g :sound-edges g :treasure-map #{:5 :400} :vwalls vwalls :hwalls hwalls :door-list {:front-door (keynum (- (* dim dim) (/ dim 2))) :side-door (keynum (+ 1 (* 4 dim)))}
                  :characters {:cat {:loc :4 :message "You are the Cat."} :caretaker {:loc :198 :message "You are the Caretaker."}}
                  }))

;; Util
(defn other-character
  []
  (if (= (:turn @s) :cat)
    :caretaker
    :cat
    )
  )
(defn other-message
  []
  (if (= (:turn @s) :cat)
    (get-in @s [:characters :cat :message]) 
    (get-in @s [:characters :caretaker :message]) 
    )
  )
(defn change-character
  [s]
  (assoc s :turn (other-character))
  )
(defn dice-roll
  ([]
   (+ 1 (rand-int 6))
   )
  ([i]
   (- (dice-roll) i)
   )
  )
(defn rand-string
  []
  (apply str (take 10 (repeatedly #(char (+ (rand 26) 65)))))
  )
(defn keyword-to-int
  [k]
  (if k 
    (js/parseInt (name k))
    0
    )
  )
(defn character-position
  [c]
  (get-in @s [:characters c :loc])
  )
(def item-map {:caretaker "r" :cat "t" :fight "X" :cursor "\u2588" :front-door "\u250f" :side-door "\u250f" :check "e"})
(defn map-pos-to-char
  []
  (let [cm (if (= (character-position :cat) (character-position :caretaker))
             {(character-position :cat) "X"}
             {(character-position :cat) "t" (character-position :caretaker) "r"})
        dm (into {} (map #(hash-map % "$") (:treasure-map s)))
        vwalls (into {} (map (fn [s] (hash-map s "|")) (apply clojure.set/union (:vwalls s))))
        hwalls (into {} (map (fn [s] (hash-map s "_")) (apply clojure.set/union (:hwalls s))))
        ]
    (-> (into {} [dm cm vwalls hwalls])
        (assoc (get-in @s [:board :cursor]) "\u2588")
        (assoc (get-in @s [:characters (:turn @s) :noise-icon]) "\\/")
        )
     
    )
  )
(defn update-status
  [s & rest]
  (assoc-in s [:characters (:turn s) :message] (apply str rest))
  )
(defn update-and-change
  [s & rest]
  (change-character (assoc-in s [:characters (:turn s) :message] (apply str rest)))
  )
(defn noise
  [move-length]
  (dice-roll (- 4 move-length))
  )
(defn hear-path
  [s noise pos b e]
  (let [bpath (alg/bf-path (:sound-edges s) b pos)
        epath (alg/bf-path (:sound-edges s) e pos)
        ]
    (cond 
      (and bpath (>= noise (- (count bpath) 1))) bpath
      (and epath (>= noise (- (count epath) 1))) epath
      :else nil
      )
    )
  )
(defn alert
  [s noise b e]
  (let [pos (get-in s [:characters (other-character) :loc]) 
        noise-path (hear-path s noise pos b e)]
    (if noise-path
      (do 
        (reset! (other-message) (str "You hear " ((other-character) char-keystring)))
        (-> s
          (assoc-in [:characters (other-character) :noise-icon] (last (drop-last noise-path)))
          ))
      s
      )
    )
  )
(defn is-combat?
  []
  (if (= (character-position :cat) (character-position :caretaker)) true false)
  )
;; Combat
;(def subdue-keys {:arms "arms" :legs "legs" :mouth "mouth"})
(defn new-bind-mat
  []
  (rand-nth ["tape" "rope"])
  )
(defn subdue
  [s k]
  (let [cs (get-in s [:character (other-character) k])
        bp (name k)
        oc ((other-character) char-keystring)
        your-arms (get-in s [:character (:turn @s) :arms])
        roll (if (> your-arms 6) (- (dice-roll) 3) (dice-roll))
        opp-roll (if (> cs 6) (- (dice-roll) 3) (dice-roll))
        bind-mat (if (> cs 0) (get-in s [:character (other-character) (keyword (str bp "-bindmat"))]) (new-bind-mat))
        ]
  (cond
    (= your-arms 12) (swap! s update-status "You can't subdue with your arms bound.")
    (>= cs 12) (update-status s "Your opponent's " bp " are already fully bound.")
    (< roll opp-roll) (update-status s "You try to subdue the " oc "'s " bp ", but they escape!")
    (> cs 0) (update-status (assoc s [:character (other-character) k] (+ cs (- roll opp-roll))) "You grab more " bind-mat " and wrap it around " oc "'s " bp ".")
    :else (update-status 
            (-> s
            (assoc [:character (other-character) k] (+ cs (- roll opp-roll)))
            (assoc [:character (other-character) (keyword (str bp "-bindmat"))] bind-mat)
            ) "You grab some " bind-mat " and wrap it around " oc "'s " bp ".")
    ))
  )

;; -------------------------
;; Quil
(defn setup 
  []
  (q/smooth)
  (q/frame-rate 30)
  (q/background 000)
  s
  )
(defn change-position
  [k change]
  (keynum (+ (keyword-to-int k) change))
  )
(defn can-move?
  [g b e]
  (if-let [p (alg/bf-path g b e)]
    (if (< (count p) 6)
      (- (count p) 1)
      nil
      )
    nil
    )
  )
(defn move-cursor
  [s c]
  (let [next-cursor-pos (change-position (get-in s [:board :cursor]) c)
        char-pos (character-position (:turn @s))
        ]
  (if (or (= next-cursor-pos char-pos) (can-move? (:move-edges s) char-pos next-cursor-pos))
    (assoc-in s [:board :cursor] next-cursor-pos)
    s
    ))
  )
(defn is-key?
  [k c]
  (= (:key k) c)
  )
(defn on-key
  [s e]
  (let [cursor-pos (get-in s [:board :cursor])
        char-pos (character-position (:turn @s))
        ]
    (cond
      (and (nil? cursor-pos) (= (:key e) :m)) (assoc-in s [:board :cursor] char-pos)
      (and cursor-pos (= (:key e) :right)) (move-cursor s 1) 
      (and cursor-pos (= (:key e) :left)) (move-cursor s -1) 
      (and cursor-pos (= (:key e) :down)) (move-cursor s row-length) 
      (and cursor-pos (= (:key e) :up)) (move-cursor s (- row-length))
      (and cursor-pos (= (:key e) :m) (= cursor-pos char-pos)) (update-in s [:board] dissoc :cursor)
      (and cursor-pos (= (:key e) :m)) (let [move-length (can-move? (:move-edges s) char-pos cursor-pos)
                                             noise (noise move-length)
                                             ] 
                                             (swap! s (update-and-change 
                                               #(-> %
                                                   (assoc-in [:characters (:turn %) :loc] cursor-pos)
                                                   (update-in [:board] dissoc :cursor)
                                                   (alert noise char-pos cursor-pos)
                                                   )
                                               (cond
                                                 (> noise 4) (str "You make a racket as you move " move-length " spaces.")
                                                 (> noise 2) (str "You shuffle " move-length " spaces.")
                                                 (> noise 0) (str "You quietly move " move-length " spaces.")
                                                 :else (str "You silently move " move-length " spaces.")
                                                 ))))
      (and (contains? (:treasure-map s) char-pos) (is-key? e :t)) (update-and-change 
                                                                    (-> s
                                                                      (assoc :treasure-map (disj (:treasure-map s) char-pos))
                                                                      (update-in [:characters (:turn @s) :treasure] inc)
                                                                      )
                                                                    "You picked up 1 treasure!"
                                                                    )
      (and (is-combat?) (is-key? e :a)) (subdue s :arms)
      (and (is-combat?) (is-key? e :l)) (subdue s :legs)
      :else s
      ))
  )

(defn draw-string
  [x]
  (cond
    (contains? (map-pos-to-char) (keynum x)) ((keynum x) (map-pos-to-char))
    :else "."
    )
  )
(defn draw-rows [x-step y-step]
  (doseq [y (map inc (range (* y-step 4)))]
    (doseq [x (map inc (range (* x-step 4)))]
      (let [x-coord (* x-step (* 4 y))
            y-coord (* y-step (* 4 x))
            ] 
        (do
          (q/text (draw-string (+ x (* (* x-step 4) (- y 1)))) y-coord x-coord)
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
    (do (draw-rows fsize-tp ssize-tp))
    ;(q/text "." (* ssize-tp 4) (* fsize-tp 4))
    )
  )

;; World updaters
(defn make-obstacle
  "Make an obstacle in row r of length 1 and width w offset from the left by o."
  [r l w o]
   (set (map keynum (flatten (map (fn [m] (map (fn [c] (+ c (* dim m))) (map #(+ (+ (* dim (- r 1)) (+ o 1)) %) (range l)))) (range w)))))
  )
(def v-walls (clojure.set/union 
               (make-vwall 8 9 0 6) 
               (make-vwall 8 4 13 2) 
               (make-vwall 7 2 18) 
               (make-vwall 14 2 18) 
               (make-vwall 2 5 4 1)))
(def h-walls (clojure.set/union 
               (make-hwall 9 7 0 3) 
               (make-hwall 12 7 0 5) 
               (make-hwall 4 2 0) 
               (make-hwall 17 6 7 3)))
(def right-corners (set (map keynum [(+ 8 (* 9 20))])))
(def closets (set [:161 :373 :393 :394 :395 :396 :397 :398 :399 :400]))
(def obstacles (clojure.set/union (make-obstacle 2 5 1 1) (make-obstacle 5 1 4 3)))
(def window-left-rows (set '(2 3)))
(def window-right-rows (set '(17 18)))
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
    (let [character (reagent/atom nil)]
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
          [:p (when (and (not (nil? @user)) (not (nil? (:turn @s)))) [:div "Your username is " @user " and you wish to play as " ((:turn @s) (clojure.set/map-invert keystring-char)) ". Click " [:a {:href (path-for (if (= (:turn @s) :cat) :cat-page :care-page))} "here"] " to play the game!"])]
          ]
         ] 
        ] 
       ])   
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
           ;:update update-state
           ;:draw draw-state
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
     [:div (get-in @s [:characters :cat :message])]
    ]
    )
  )
(defn care-page
  []
  (fn []
    [:span.main [:div (str "Welcome to the game! Your name is " @user " and you are playing as the Caretaker.")]
     [game-board] 
     [:div (get-in @s [:characters :cat :message])]
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
