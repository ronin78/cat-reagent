(ns cat-reagent.game.movement
  (:require [cat-reagent.game.constants :as c]
            [cat-reagent.game.util :as u]
            [cat-reagent.game.state :as st]
            [cat-reagent.game.board :as b]))

(defn change-position
  [k change]
  (u/keynum (+ (u/keyword-to-int k) change)))

(defn max-move
  [s]
  (let [turn (:turn s)
        legs (if-let [legs-from-map (get-in s [:characters turn :legs])] legs-from-map 0)]
    (int (Math/floor (- (if-let [max-move (get-in s [:characters turn :max-move])]
                          max-move
                          c/default-max-move) (/ legs 3))))))

(defn move-cursor
  [s c]
  (if-let [cursor-pos (get-in s [:characters (:turn s) :cursor])]
    (let [next-cursor-pos (change-position (last cursor-pos) c)
          char-pos (st/character-position s (:turn s))]
      (if (or (= next-cursor-pos char-pos) (b/can-move? (:move-edges s) char-pos next-cursor-pos (max-move s)))
        (update-in s [:characters (:turn s) :cursor] conj next-cursor-pos)
        s))
    s))
