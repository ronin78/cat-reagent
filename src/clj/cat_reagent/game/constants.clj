(ns cat-reagent.game.constants)

;; Board dimensions
(def dim 20)
(def size [800 600])
(def row-length (* (/ (first size) 100) 4))
(def num-rows (* (/ (second size) 100) 3))
(def space-nums (vec (map #(+ 1 %) (range (* (/ (first size) 100) (/ (second size) 100) 12)))))

;; Character mappings
(def keystring-char {"The Cat" :cat "The Caretaker" :caretaker})
(def char-keystring {:cat "Cat" :caretaker "Caretaker"})

;; Game settings
(def default-max-move 4)
(def see-around 2)
(def treasure-amt 50)

;; Clue system settings
(def footprint-decay-turns 3)
(def noise-icon-decay-turns 2)
(def disturbed-alert-threshold 3)
(def patrol-task-interval 8)  ;; Give Caretaker more time to reach tasks when unaware
(def patrol-task-overflow 3)

;; Patrol task pool templates
(def patrol-task-pool
  [{:name "Check front door" :loc-type :front-door}
   {:name "Check side door" :loc-type :side-door}
   {:name "Check phone" :loc-type :phone}
   {:name "Patrol living room" :loc-type :room}
   {:name "Check kitchen" :loc-type :room}
   {:name "Patrol hallway" :loc-type :room}])

;; Item display symbols
(def item-map {:caretaker "r"
               :cat "t"
               :fight "X"
               :cursor "\u2588"
               :phone "\u00b6"
               :vwall "|"
               :hwall "_"
               :treasure "$"
               :front-door "\u250f"
               :side-door "\u250f"
               :check "e"
               :noise "\\/"
               :footprint ","
               :disturbed "?"
               :patrol-target "!"
               :sabotaged "x"})
