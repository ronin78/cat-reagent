(ns cat-reagent.game
  (:require [loom.alg :as alg]
            [clojure.set :as set]
            [cat-reagent.game.constants :as c]
            [cat-reagent.game.util :as u]
            [cat-reagent.game.state :as st]
            [cat-reagent.game.board :as b]
            [cat-reagent.game.sound :as snd]
            [cat-reagent.game.movement :as mv]
            [cat-reagent.game.combat :as combat]))

;; Re-export for handler.clj compatibility
(def map-pos-to-char b/map-pos-to-char)

;; Patrol task assignment
(defn assign-patrol-task
  "Assign a new patrol task from the pool"
  [s]
  (let [pool c/patrol-task-pool
        current-tasks (:patrol-tasks s)
        current-locs (set (map :loc current-tasks))
        template (rand-nth pool)
        loc-type (:loc-type template)
        available-locs (case loc-type
                         :front-door (keys (filter #(true? (val %)) (:door-list s)))
                         :side-door (keys (filter #(false? (val %)) (:door-list s)))
                         :phone (keys (:phone-map s))
                         :room (let [room-positions (set (map u/keynum (range 1 577)))]
                                 (set/difference room-positions
                                                  (set (keys (:treasure-map s)))
                                                  (set (keys (:phone-map s)))
                                                  (set (keys (:door-list s)))
                                                  current-locs)))
        chosen-loc (when (seq available-locs) (rand-nth (vec available-locs)))]
    (when chosen-loc
      (update s :patrol-tasks conj (assoc template :loc chosen-loc)))))

;; Wall and board setup - use defaults from board.clj
(def wall-set (set/union (apply set/union b/default-vwalls)
                          (apply set/union b/default-hwalls)))

;; Game initialization
(defn initialize
  []
  (let [base-g (b/cut-walls b/base-graph wall-set)
        door-map {(u/keynum (- (* c/num-rows c/row-length) (/ c/row-length 2))) true  ; front door
                  (u/keynum (+ 1 (* 4 c/row-length))) false}                          ; side door
        phone-map (into {} (map #(hash-map % true) (b/rand-non-overlap (set/union wall-set (set (keys door-map))) 2 false)))
        treasure-map (b/rand-non-overlap (set/union wall-set (set (keys door-map)) (set (keys phone-map))) c/treasure-amt true)
        caretaker-face (rand-nth [:up :down :left :right])
        cat-start (first (keys door-map))
        valid-caretaker-positions (set/difference (set (map u/keynum c/space-nums))
                                                   wall-set
                                                   (set (keys phone-map))
                                                   #{cat-start})
        caretaker-start (rand-nth (vec valid-caretaker-positions))
        sound-g (b/make-neighbors (b/sound-walls base-g wall-set))
        base-state {:turn :cat
                    :turn-number 0
                    :move-edges base-g
                    :sound-edges sound-g
                    :door-list door-map
                    :phone-map phone-map
                    :treasure-map treasure-map
                    :disturbed-treasure #{}
                    :patrol-tasks []
                    :footprints {}
                    :noise-icons {}
                    :sabotaged #{}
                    :scores {:cat 0 :caretaker 0}
                    :neighbors {:n1 0 :n2 0}
                    :characters {:cat {:loc cat-start :message "You are the Cat." :face :up}
                                 :caretaker {:loc caretaker-start
                                             :face caretaker-face
                                             :max-move 2
                                             :message "You are the Caretaker."}}}]
    (or (assign-patrol-task base-state) base-state)))

(def s (atom (initialize)))

(defn restart
  []
  (reset! s (initialize)))

;; Turn management
(defn maybe-assign-patrol-task
  "Assign a new patrol task every patrol-task-interval turns"
  [s]
  (let [turn-num (st/current-turn-number s)]
    (if (and (> turn-num 0)
             (zero? (mod turn-num c/patrol-task-interval)))
      (or (assign-patrol-task s) s)
      s)))

(defn check-patrol-task-completion
  "Check if Caretaker reached a patrol task location"
  [s]
  (let [caretaker-loc (st/character-position s :caretaker)
        tasks (st/get-pending-tasks s)
        matching-task (first (filter #(= (:loc %) caretaker-loc) tasks))]
    (if (and matching-task
             (not (st/is-sabotaged? s caretaker-loc)))
      (-> s
          (st/complete-patrol-task caretaker-loc)
          (st/add-points :caretaker 10))
      s)))

(defn change-character
  [s]
  (let [s-with-turn (st/increment-turn s)
        s-decayed (-> s-with-turn
                      st/decay-footprints
                      st/decay-all-noise-icons)
        new-turn (if (st/is-ambush? s-decayed) (st/unambush s-decayed) (assoc s-decayed :turn (st/other-player-key s-decayed)))
        ucc st/update-current-player
        new-player (st/current-player new-turn)
        new-other-player (st/other-player new-turn)
        sight-set (set (b/can-see? new-turn (st/character-position new-player) (st/character-attribute new-player :face) c/see-around))
        cat-treasure (get-in new-turn [:characters :cat :treasure])
        pending-tasks (count (st/get-pending-tasks new-turn))
        too-distracted? (>= pending-tasks 2)
        overwhelmed? (>= pending-tasks 3)
        s-with-disturbed (if (= (:turn new-turn) :caretaker)
                           (-> new-turn
                               (st/notice-disturbed sight-set (not too-distracted?))
                               (st/remember-footprints sight-set))
                           new-turn)
        caretaker-data (get-in s-with-disturbed [:characters :caretaker])
        caretaker-restrained? (or (st/body-part caretaker-data :arms)
                                  (st/body-part caretaker-data :legs))
        s-with-awareness (if (and (= (:turn s-with-disturbed) :caretaker)
                                   overwhelmed?
                                   (st/is-aware? s-with-disturbed)
                                   (not (st/is-combat? s-with-disturbed))
                                   (not caretaker-restrained?))
                           (-> s-with-disturbed
                               st/lose-awareness
                               (st/update-status-message "You're overwhelmed with tasks... what was that about an intruder?"))
                           s-with-disturbed)
        s-with-patrol (if (= (:turn s-with-awareness) :caretaker)
                        (check-patrol-task-completion s-with-awareness)
                        s-with-awareness)
        s-with-new-task (maybe-assign-patrol-task s-with-patrol)]
    (do (update-in s-with-new-task [:characters (:turn s-with-new-task)] dissoc :status-message)
        (cond
          (and cat-treasure (>= cat-treasure c/treasure-amt) (contains? (:door-list s) (get-in s-with-new-task [:characters :cat :loc])))
          (st/update-status (st/victory s-with-new-task :cat :treasure) "The Cat escapes with all the treasure! The Cat wins!")

          (some #(> % 3) (vals (:neighbors s)))
          (st/update-status (st/victory s-with-new-task :caretaker :neighbors) "The neighbors call the police! The Caretaker wins!")

          (st/is-combat? s-with-new-task)
          (ucc (st/make-aware s-with-new-task) :status-message "You are in combat!")

          (contains? sight-set (st/character-position new-other-player))
          (st/see-alert s-with-new-task (:turn s-with-new-task))

          (contains? (:treasure-map s-with-new-task) (:loc new-player))
          (st/update-status-message s-with-new-task "There be treasure here!")

          (contains? (:phone-map s-with-new-task) (:loc new-player))
          (st/update-status-message s-with-new-task "There is a phone here.")

          :else (st/update-status-message s-with-new-task "")))))

(defn update-and-change
  [s & rest]
  (change-character (assoc-in s [:characters (:turn s) :message] (apply str rest))))

;; Main move dispatch
(defn move
  [s m]
  (let [p (st/current-player s)
        ucc st/update-current-player
        turn (:turn s)
        cursor-pos (st/character-attribute p :cursor)
        char-pos (st/character-position p)
        treasure-map (:treasure-map s)
        in-combat? (st/is-combat? s)
        phone-map (:phone-map s)
        move-map {:m (cond
                       (< (mv/max-move s) 1) (fn [s] (st/update-status s "You cannot move with your legs bound."))
                       (nil? cursor-pos) (fn [s] (ucc s :cursor [char-pos]))
                       (= (last cursor-pos) char-pos) (fn [s] (update-in s [:characters turn] dissoc :cursor))
                       (and in-combat? (:legs p) (< (u/dice-roll (/ (:legs p) 4)) 4)) (fn [s] (update-and-change (update-in s [:characters turn] dissoc :cursor) "Your opponent blocks your way!"))
                       (and in-combat? (not (:legs p))
                            (let [arms (or (:arms p) 0)
                                  arm-penalty (if (> arms 0) (min 2 (/ arms 3)) 0)
                                  cat-bonus (if (= turn :cat) 1 0)
                                  my-roll (- (+ (u/dice-roll) cat-bonus) arm-penalty)
                                  opp-roll (u/dice-roll)]
                              (<= my-roll opp-roll)))
                       (fn [s] (update-and-change (update-in s [:characters turn] dissoc :cursor) "Your opponent grabs you as you try to disengage!"))
                       :else (fn [s]
                               (let [legs (st/body-part p :legs)
                                     path (alg/dijkstra-path-dist (:move-edges s) char-pos (last cursor-pos))]
                                 (if (nil? path)
                                   (-> s
                                       (update-in [:characters turn] dissoc :cursor)
                                       (st/update-status "You can't reach there!"))
                                   (let [move-length (b/can-move? (second path) (mv/max-move s))
                                         dir (b/direction (vec (take-last 2 cursor-pos)))
                                         noise (snd/noise move-length)
                                         dest (last cursor-pos)
                                         see-set (set (b/can-see? s dest dir c/see-around))
                                         ;; Check for automatic treasure pickup (Cat only, arms not bound)
                                         has-treasure (and (= turn :cat)
                                                           (contains? (:treasure-map s) dest)
                                                           (<= (or (st/body-part p :arms) 0) 0))
                                         treasure-noise (when has-treasure (u/dice-roll 1))
                                         base-state (-> s
                                                        (cond-> (= turn :cat) (st/add-footprint char-pos))
                                                        (ucc :loc dest)
                                                        (ucc :face dir)
                                                        (ucc :memory-map (fn [mem]
                                                                          (merge mem
                                                                                 (b/map-to-symbol s
                                                                                                  (b/can-see? s (st/character-position s turn) (st/character-attribute (st/current-player s) :face) c/see-around)
                                                                                                  turn))))
                                                        (update-in [:characters turn] dissoc :cursor)
                                                        (snd/hear-alert noise char-pos dest)
                                                        (st/make-ambush)
                                                        (cond-> (contains? see-set (st/character-position s (st/opponent-key turn))) (st/see-alert turn))
                                                        (cond-> in-combat? (ucc :muffled? false))
                                                        (cond-> in-combat? (ucc :status-message "You escape combat!"))
                                                        (cond-> has-treasure
                                                          (-> (st/update-current-player :treasure (fnil #(+ 1 %) 0))
                                                              (assoc :treasure-map (dissoc (:treasure-map s) dest))
                                                              (st/mark-treasure-disturbed dest)
                                                              (snd/hear-alert treasure-noise))))
                                         move-msg (cond
                                                    (> noise 4) (str "You make a racket as you move " move-length " spaces.")
                                                    (> noise 2) (str "You shuffle " move-length " spaces.")
                                                    (> noise 0) (str "You quietly move " move-length " spaces.")
                                                    :else (str "You silently move " move-length " spaces."))
                                         treasure-msg (when has-treasure
                                                        (str " You grab treasure! (" (snd/noise-word treasure-noise) ")"))]
                                     (update-and-change base-state (str move-msg treasure-msg)))))))
                  :right (fn [s] (mv/move-cursor s 1))
                  :left  (fn [s] (mv/move-cursor s -1))
                  :down  (fn [s] (mv/move-cursor s c/row-length))
                  :up    (fn [s] (mv/move-cursor s (- c/row-length)))
                  :s (let [at-phone (contains? phone-map char-pos)
                           at-door (contains? (:door-list s) char-pos)
                           can-sabotage (or at-phone at-door)]
                       (when can-sabotage
                         (fn [s]
                           (let [noise (u/dice-roll 1)]
                             (cond
                               (st/is-sabotaged? s char-pos) (st/update-status s "This location is already sabotaged.")
                               (= turn :caretaker) (st/update-status s "Why would you sabotage your own house?")
                               :else (update-and-change
                                       (-> s
                                           (st/sabotage-location char-pos)
                                           (snd/hear-alert noise))
                                       (if at-phone
                                         (str "You cut the phone line! You make a " (snd/noise-word noise) ".")
                                         (str "You jam the door! You make a " (snd/noise-word noise) "."))))))))
                  :c (when (and (contains? phone-map char-pos) (st/is-aware? s))
                       (fn [s]
                         (let [noise (u/dice-roll 2)]
                           (cond
                             (st/is-sabotaged? s char-pos) (st/update-status s "The phone line has been cut!")
                             (= turn :cat) (st/update-status s "You're the burglar, remember?")
                             :else (update-and-change
                                     (-> s
                                         (st/victory :caretaker :phone)
                                         (snd/hear-alert noise))
                                     "You call 911! The police are on their way!")))))
                  :f (when (st/is-sabotaged? s char-pos)
                       (fn [s]
                         (let [noise (u/dice-roll 1)
                               at-phone (contains? phone-map char-pos)
                               at-door (contains? (:door-list s) char-pos)]
                           (cond
                             (= turn :cat) (st/update-status s "Why would you fix something you sabotaged?")
                             at-phone (update-and-change (st/fix-sabotaged s char-pos) "You fix the phone line!")
                             at-door (update-and-change (st/fix-sabotaged s char-pos) "You unjam the door!")
                             :else (update-and-change (st/fix-sabotaged s char-pos) "You fix the sabotaged location.")))))
                  :t (when (contains? treasure-map char-pos)
                       (fn [s] (let [treasure (char-pos (:treasure-map s))
                                     noise (u/dice-roll 1)]
                                 (cond
                                   (> (st/body-part (st/current-player s) :arms) 0) (st/update-status s "You can't pick up treasure with your arms bound.")
                                   :else (update-and-change
                                           (-> s
                                               (st/update-current-player :treasure (fnil #(+ treasure %) 0))
                                               (assoc :treasure-map (dissoc (:treasure-map s) char-pos))
                                               (st/mark-treasure-disturbed char-pos)
                                               (snd/hear-alert noise))
                                           "You picked up 1 treasure! You make a " (snd/noise-word noise) ".")))))
                  :i (fn [s] (combat/inventory s))
                  :A (when in-combat? (fn [s] (let [{:keys [state message end-turn?]} (combat/subdue s :arms)]
                                                (if end-turn?
                                                  (update-and-change state message)
                                                  (st/update-status state message)))))
                  :L (when in-combat? (fn [s] (let [{:keys [state message end-turn?]} (combat/subdue s :legs)]
                                                (if end-turn?
                                                  (update-and-change state message)
                                                  (st/update-status state message)))))
                  :G (when in-combat? (fn [s] (let [{:keys [state message end-turn?]} (combat/gag s)]
                                                (if end-turn?
                                                  (update-and-change state message)
                                                  (st/update-status state message)))))
                  :u (when in-combat? (fn [s] (let [{:keys [state message end-turn?]} (combat/muffle s)]
                                                (if end-turn?
                                                  (update-and-change state message)
                                                  (st/update-status state message)))))
                  :a (fn [s] (let [{:keys [state message end-turn?]} (combat/resist s :arms)]
                               (if end-turn?
                                 (update-and-change state message)
                                 (st/update-status state message))))
                  :l (fn [s] (let [{:keys [state message end-turn?]} (combat/resist s :legs)]
                               (if end-turn?
                                 (update-and-change state message)
                                 (st/update-status state message))))
                  :g (fn [s] (let [{:keys [state message end-turn?]} (combat/ungag s)]
                               (if end-turn?
                                 (update-and-change state message)
                                 (st/update-status state message))))
                  :y (fn [s] (let [{:keys [state message end-turn?]} (combat/yell s)]
                               (if end-turn?
                                 (update-and-change state message)
                                 (st/update-status state message))))}]
    (if-let [move-fn (m move-map)] (move-fn s) (identity s))))

;; Main entry point
(defn play
  [c player]
  (if (and (= player (:turn @s)) (not (:game-over @s)))
    (swap! s #(move % c))
    @s))
