(ns cat-reagent.game
  (:require
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
                    (map (fn [sa d] (cond (contains? space-set sa) [(keynum s) (keynum sa) {:direction d}])) 
                         (adj-fn s) dir-vec)
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
(def s (atom {:board position-map :turn :cat :move-edges g :sound-edges g :treasure-map #{:5 :400} :vwalls vwalls :hwalls hwalls :door-list {:front-door (keynum (- (* dim dim) (/ dim 2))) :side-door (keynum (+ 1 (* 4 dim)))}
                  :characters {:cat {:loc :4 :message "You are the Cat."} :caretaker {:loc :198 :message "You are the Caretaker."}}
                  }))

;; Util
(defn other-character
  [s]
  (if (= (:turn s) :cat)
    :caretaker
    :cat
    )
  )
(defn other-message
  [s]
  (if (= (:turn s) :cat)
    (get-in s [:characters :cat :message]) 
    (get-in s [:characters :caretaker :message]) 
    )
  )
(defn change-character
  [s]
  (assoc s :turn (other-character s))
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
(defn parse-int
  [s]
  (Integer. (re-find  #"\d+" s ))
  )
(defn keyword-to-int
  [k]
  (if k 
    (parse-int (name k))
    0
    )
  )
(defn character-position
  [s c]
  (get-in s [:characters c :loc])
  )
(def item-map {:caretaker "r" :cat "t" :fight "X" :cursor "\u2588" :front-door "\u250f" :side-door "\u250f" :check "e"})
(defn map-pos-to-char
  [s]
  (let [cm (if (= (character-position s :cat) (character-position s :caretaker))
             {(character-position s :cat) "X"}
             {(character-position s :cat) "t" (character-position s :caretaker) "r"})
        dm (into {} (map #(hash-map % "$") (:treasure-map s)))
        vwalls (into {} (map (fn [s] (hash-map s "|")) (apply clojure.set/union (:vwalls s))))
        hwalls (into {} (map (fn [s] (hash-map s "_")) (apply clojure.set/union (:hwalls s))))
        ]
    (-> (into {} [dm cm vwalls hwalls])
        (cond-> (get-in s [:board :cursor]) (assoc (get-in s [:board :cursor]) "\u2588"))
        (cond-> (get-in s [:characters (:turn s) :noise-icon]) (assoc (get-in s [:characters (:turn s) :noise-icon]) "\\/"))
        ;(assoc (get-in s [:board :cursor]) "\u2588")
        ;(assoc (get-in s [:characters (:turn s) :noise-icon]) "\\/")
        )
    )
  )
;; Game play
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
  (let [pos (get-in s [:characters (other-character s) :loc]) 
        noise-path (hear-path s noise pos b e)]
    (if noise-path
        (-> s
          (assoc-in [:characters (other-character s) :noise-icon] (last (drop-last noise-path)))
          (assoc-in [:characters (other-character s) :message] (str "You hear " ((other-character s) char-keystring)))
          )
      s
      )
    )
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
  (if-let [cursor-pos (get-in s [:board :cursor])]
    (let [next-cursor-pos (change-position cursor-pos c)
          char-pos (character-position s (:turn s))
          ]
      (if (or (= next-cursor-pos char-pos) (can-move? (:move-edges s) char-pos next-cursor-pos))
        (assoc-in s [:board :cursor] next-cursor-pos)
        s
        ))
    s
    )
  )
(defn is-combat?
  [s]
  (if (= (character-position s :cat) (character-position s :caretaker)) true false)
  )
;; Combat
;(def subdue-keys {:arms "arms" :legs "legs" :mouth "mouth"})
(defn new-bind-mat
  []
  (rand-nth ["tape" "rope"])
  )
(defn subdue
  [s k]
  (let [cs (get-in s [:character (other-character s) k])
        bp (name k)
        oc ((other-character s) char-keystring)
        your-arms (get-in s [:character (:turn s) :arms])
        roll (if (> your-arms 6) (- (dice-roll) 3) (dice-roll))
        opp-roll (if (> cs 6) (- (dice-roll) 3) (dice-roll))
        bind-mat (if (> cs 0) (get-in s [:character (other-character s) (keyword (str bp "-bindmat"))]) (new-bind-mat))
        ]
  (cond
    (= your-arms 12) (update-status s "You can't subdue with your arms bound.")
    (>= cs 12) (update-status s "Your opponent's " bp " are already fully bound.")
    (< roll opp-roll) (update-status s "You try to subdue the " oc "'s " bp ", but they escape!")
    (> cs 0) (update-status (assoc s [:character (other-character s) k] (+ cs (- roll opp-roll))) "You grab more " bind-mat " and wrap it around " oc "'s " bp ".")
    :else (update-status 
            (-> s
            (assoc [:character (other-character s) k] (+ cs (- roll opp-roll)))
            (assoc [:character (other-character s) (keyword (str bp "-bindmat"))] bind-mat)
            ) "You grab some " bind-mat " and wrap it around " oc "'s " bp ".")
    ))
  )
(defn move
  [s m]
  (let [cursor-pos (get-in s [:board :cursor])
        turn (:turn s)
        char-pos (character-position s turn)
        treasure-map (:treasure-map s)
        in-combat? (is-combat? s)
        move-map {:m (cond
                       (nil? cursor-pos) (fn [s] (assoc-in s [:board :cursor] char-pos))
                       (= cursor-pos char-pos) (fn [s] (update-in s [:board] dissoc :cursor))
                       :else (fn [s] 
                               (let [move-length (can-move? (:move-edges s) char-pos cursor-pos)
                                     noise (noise move-length)
                                     ] 
                                 (update-and-change 
                                   (-> s
                                       (assoc-in [:characters turn :loc] cursor-pos)
                                       (update-in [:board] dissoc :cursor)
                                       (alert noise char-pos cursor-pos)
                                       )
                                   (cond
                                     (> noise 4) (str "You make a racket as you move " move-length " spaces.")
                                     (> noise 2) (str "You shuffle " move-length " spaces.")
                                     (> noise 0) (str "You quietly move " move-length " spaces.")
                                     :else (str "You silently move " move-length " spaces.")
                                     ))))
                       )
                  :right (fn [s] (move-cursor s 1)) 
                  :left  (fn [s] (move-cursor s -1)) 
                  :down  (fn [s] (move-cursor s row-length)) 
                  :up    (fn [s] (move-cursor s (- row-length)))
                  :t (when (contains? treasure-map char-pos) 
                       (fn [s] (update-and-change 
                                 (-> s
                                     (assoc :treasure-map (disj (:treasure-map s) (character-position s (:turn s))))
                                     (update-in [:characters (:turn s) :treasure] (fnil inc 0))
                                     )
                                 "You picked up 1 treasure!"
                                 ))
                       )
                  :a (when in-combat? (fn [s] (subdue s :arms)))
                  :l (when in-combat? (fn [s] (subdue s :legs)))
                  }]
    (if-let [move-fn (m move-map)] (move-fn s) (identity s))
    ))
(defn play
  [c player]
  (if (= player (:turn @s))  
    (swap! s #(move % c))
    @s
    )
  )
