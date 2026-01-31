(ns cat-reagent.game.util
  (:require [clojure.set :as set]))

;; String utilities
(defn rand-string
  []
  (apply str (take 10 (repeatedly #(char (+ (rand 26) 65))))))

(defn parse-int
  [s]
  (Integer. (re-find #"\d+" s)))

(defn keyword-to-int
  [k]
  (if k
    (parse-int (name k))
    0))

(defn keynum
  [num]
  (keyword (str num)))

(defn drop-nth
  [n coll]
  (keep-indexed #(if (not= %1 n) %2) coll))

;; Dice
(defn dice-roll
  ([]
   (+ 1 (rand-int 6)))
  ([i]
   (- (dice-roll) i)))

;; Set utilities
(defn union
  [s]
  (apply set/union s))

(defn union-all
  [& rest]
  (union (map union rest)))

(defn select-key-vals
  [m v]
  (vals (select-keys m v)))

(defn set-of-keys
  [m]
  (set (keys m)))
