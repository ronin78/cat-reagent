(ns cat-reagent.ascii
  (:gen-class)
  (:require [clojure.term.colors :as col]
            [clojure.string :as s]
            [quil.core :as q]
            [quil.middleware :as m]
            )
  (:use [cat-reagent.util])
  )

;; Functions for drawing ASCII boards.
(def dim 20)
(def item-map {:caretaker "r" :cat "t" :cursor "\u2588" :front-door "\u250f" :side-door "\u250f"})
(defn make-vwall 
  "Make a vertical wall in column c of length 1 offset from the top by o, with a hole in d."
  ([c l o]
   (set (map #(keynum (+ (* dim %) c)) (range o (+ o l)))))
  ([c l o d]
   (set (map #(keynum (+ (* dim %) c)) (drop-nth (- d 1) (range o (+ o l))))) 
   )
  )
(defn make-hwall
  "Make a horizontal wall in row r of length 1 offset from the left by o, with a hole in d."
  ([r l o]
   (set (map #(keynum (+ (+ (* dim (- r 1)) (+ o 1)) %)) (range l)))
   )
  ([r l o d]
   (set (drop-nth (- d 1) (map #(keynum (+ (+ (* dim r) (+ o 1)) %)) (range l))))
   )
  )
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
(defn draw-top-bot [] (do (dotimes [t (+ 2 (* 3 dim))] (print "_")) (prn)))
(defn draw-cells
  [pm]
  (doseq [x (range 1 (+ 1 (* dim dim)))] 
    (do 
        (cond (= 1 (mod x dim))
             (if (contains? window-left-rows (+ 1 (/ (- x 1) dim)))
               (q/text "\u250A")
               (print "|")
               )
          )
       (cond
          (contains? (clojure.set/map-invert pm) (keynum x)) (print (str " " (((keynum x) (clojure.set/map-invert pm)) item-map) " "))
          (contains? v-walls (keynum x)) (print " | ")
          (contains? h-walls (keynum x)) (print "___")
          (contains? right-corners (keynum x)) (print "_| ")
          (contains? closets (keynum x)) (print " c ")
          (contains? obstacles (keynum x)) (print "ooo")
          :else (print (str " . "))
          ) 
       (cond (= 0 (mod x dim)) 
             (if (contains? window-right-rows (/ x dim))
               (print "\u250A\n")
               (print "|\n")
               )
          )
        )
    )
  )
(defn draw-board [pm] (do (draw-top-bot) (draw-cells pm) (draw-top-bot)))

;; Quil sample functions
(def size [800 600])
(defn draw-string
  [s x]
  (let [pm (:board s)]
  (cond
    (contains? (clojure.set/map-invert pm) (keynum x)) (((keynum x) (clojure.set/map-invert pm)) item-map)
    :else "."
    ))
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
    (do (draw-rows s fsize-tp ssize-tp))
    ;(q/text "." (* ssize-tp 4) (* fsize-tp 4))
    )
  )
;(defn draw
;  []
;  (q/stroke (q/random 255) (q/random 255) (q/random 255))
;  (q/stroke-weight (q/random 10))
;  (q/fill (q/random 255))
;  (let [diam (q/random 100)
;        x (q/random (q/width))
;        y (q/random (q/height))
;        ]
;    (q/ellipse x y diam diam)
;    )
;  )
