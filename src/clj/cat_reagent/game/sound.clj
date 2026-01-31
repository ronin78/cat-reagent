(ns cat-reagent.game.sound
  (:require [loom.alg :as alg]
            [cat-reagent.game.util :as u]
            [cat-reagent.game.state :as st]))

(defn noise
  [move-length]
  (u/dice-roll (- 4 move-length)))

(defn noise-word
  [noise]
  (cond
    (> noise 4) "racket"
    (> noise 2) "clatter"
    :else "noise"))

(defn hear-path
  [s noise pos opos]
  (let [path (alg/shortest-path (:sound-edges s) pos opos)]
    (cond
      (and path (>= noise (second (alg/dijkstra-path-dist (:sound-edges s) pos opos)))) path
      :else nil)))

(defn hear-alert
  ([s noise]
   (hear-alert s noise (:loc (st/current-player s))))
  ([s noise pos]
   (let [opos (st/character-position (st/other-player s))
         onoise-path (hear-path s noise pos opos)
         n1-path (hear-path s noise pos :n1)
         n2-path (hear-path s noise pos :n2)
         current-turn (st/current-turn-number s)]
     (-> s
         (cond-> onoise-path (update-in [:characters (st/other-player-key s)]
                                        merge {:noise-icon {:pos (last (drop-last onoise-path))
                                                            :turn-created current-turn}
                                               :message (str (get-in s [:characters (st/other-player-key s) :message]) "\nYou hear "
                                                                                              (if (= (:turn s) :cat)
                                                                                                ;; Cat made noise - Caretaker hears
                                                                                                (if (st/is-aware? s) "the Cat" "something")
                                                                                                ;; Caretaker made noise - Cat hears
                                                                                                "the Caretaker") "!")}))
         (cond-> n1-path (update-in [:neighbors :n1] (fnil inc 0)))
         (cond-> n2-path (update-in [:neighbors :n2] (fnil inc 0))))))
  ([s noise b e]
   (let [opos (st/character-position (st/other-player s))
         noise-path-e (hear-path s noise e opos)]
     (if noise-path-e
       (hear-alert s noise e)
       (hear-alert s noise b)))))
