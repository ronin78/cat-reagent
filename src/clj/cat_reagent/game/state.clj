(ns cat-reagent.game.state
  (:require [cat-reagent.game.constants :as c]
            [clojure.set]))

;; Player accessors based on :turn
(defn current-player-key
  [s]
  (:turn s))

(defn current-player
  [s]
  (get-in s [:characters (:turn s)]))

(defn update-current-player
  [s k v]
  (cond
    (fn? v) (update-in s [:characters (:turn s) k] v)
    :else (assoc-in s [:characters (:turn s) k] v)))

(defn other-player-key
  [s]
  (if (= (:turn s) :cat)
    :caretaker
    :cat))

(defn other-player
  [s]
  (get-in s [:characters (other-player-key s)]))

(defn update-other-player
  [s k v]
  (cond
    (fn? v) (update-in s [:characters (other-player-key s) k] v)
    :else (assoc-in s [:characters (other-player-key s) k] v)))

(defn other-player-message
  [s]
  (:message (other-player s)))

;; Character accessors by key
(defn character-position
  ([p]
   (:loc p))
  ([s c]
   (get-in s [:characters c :loc])))

(defn character-attribute
  ([p a]
   (a p))
  ([s c a]
   (get-in s [:characters c a])))

(defn opponent-key
  [s]
  (if (= s :cat)
    :caretaker
    :cat))

;; Predicates
(defn is-combat?
  [s]
  (if (= (character-position s :cat) (character-position s :caretaker)) true false))

(defn is-aware?
  [s]
  (get-in s [:characters :caretaker :aware]))

(defn is-ambush?
  [s]
  (get-in s [:characters :caretaker :ambush?]))

;; Body parts and color
(defn body-part
  [p k]
  (if-let [bp (k p)] bp 0))

(defn color-vec
  [s player m]
  (let [color-map {"$" :green
                   "\\/" :orange
                   "t" :white
                   "r" :white
                   "," :grey      ; footprint
                   "?" :yellow    ; disturbed treasure
                   "!" :cyan      ; patrol target
                   "x" :red       ; sabotaged
                   "|" :grey      ; vertical wall
                   "_" :grey}]    ; horizontal wall
    (cond
      m [s :grey]
      (get color-map s) [s (get color-map s)]
      :else [s :white])))

;; State modifiers
(defn make-aware
  [s]
  (-> s
      (assoc-in [:characters :caretaker :aware] true)
      (assoc-in [:characters :caretaker :max-move] 3)))

(defn lose-awareness
  "Caretaker becomes distracted and loses awareness"
  [s]
  (-> s
      (assoc-in [:characters :caretaker :aware] false)
      (assoc-in [:characters :caretaker :max-move] 2)))

(defn make-ambush
  [s]
  (let [in-combat? (is-combat? s)
        was-aware? (is-aware? s)
        cat-initiated? (= (:turn s) :cat)]
    (cond
      ;; Only Cat gets ambush bonus when surprising unaware Caretaker
      (and in-combat? (not was-aware?) cat-initiated?) (assoc-in s [:characters :caretaker :ambush?] true)
      :else s)))

(defn unambush
  [s]
  (assoc-in s [:characters :caretaker :ambush?] false))

;; Status updates
(defn update-status
  [s & rest]
  (assoc-in s [:characters (:turn s) :message] (apply str rest)))

(defn update-status-message
  [s & rest]
  (assoc-in s [:characters (:turn s) :status-message] (apply str rest)))

(defn update-other-message
  [s & rest]
  (assoc-in s [:characters (other-player-key s) :message] (apply str rest)))

(defn update-other-status-message
  [s & rest]
  (assoc-in s [:characters (other-player-key s) :status-message] (apply str rest)))

;; Alerts
(defn see-alert
  [s player]
  (-> s
      (update-current-player :message (str (get-in s [:characters player :message]) "\nYou see the " ((opponent-key player) c/char-keystring) "!"))
      (cond-> (= player :caretaker) (make-aware))))

;; Victory conditions
(defn victory
  [s char-key message]
  (cond
    (= message :911) (do (println "The Caretaker wins by dialing 911!")
                         (assoc s :game-over :caretaker))
    (= message :neighbors) (do (println "The neighbors get suspicious and call the police. The Caretaker wins!")
                               (assoc s :game-over :caretaker))
    (= message :treasure) (do (println "The Cat escapes with all the treasure! The Cat wins!")
                              (assoc s :game-over :cat))
    :else (do (println "Someone won?")
              s)))

;; Turn counter
(defn current-turn-number
  [s]
  (or (:turn-number s) 0))

(defn increment-turn
  [s]
  (update s :turn-number (fnil inc 0)))

;; Footprints
(defn add-footprint
  "Add a footprint at pos with current turn number"
  [s pos]
  (assoc-in s [:footprints pos] {:turn-created (current-turn-number s)}))

(defn decay-footprints
  "Remove footprints older than footprint-decay-turns"
  [s]
  (let [current (current-turn-number s)
        decay-threshold c/footprint-decay-turns]
    (update s :footprints
            (fn [fps]
              (into {} (filter (fn [[_ v]]
                                 (<= (- current (:turn-created v)) decay-threshold))
                               fps))))))

(defn get-visible-footprints
  "Get footprints visible in sight-set"
  [s sight-set]
  (let [fps (:footprints s)]
    (set (filter #(contains? sight-set %) (keys fps)))))

(defn remember-footprints
  "Add visible footprints to Caretaker's footprint memory"
  [s sight-set]
  (let [visible-fps (get-visible-footprints s sight-set)]
    (update-in s [:characters :caretaker :footprint-memory]
               (fnil #(clojure.set/union % visible-fps) #{}))))

;; Noise icons
(defn decay-noise-icon
  "Remove noise icon if older than noise-icon-decay-turns"
  [s player-key]
  (let [current (current-turn-number s)
        noise-icon (get-in s [:characters player-key :noise-icon])]
    (if (and noise-icon
             (> (- current (:turn-created noise-icon)) c/noise-icon-decay-turns))
      (update-in s [:characters player-key] dissoc :noise-icon)
      s)))

(defn decay-all-noise-icons
  "Decay noise icons for both players"
  [s]
  (-> s
      (decay-noise-icon :cat)
      (decay-noise-icon :caretaker)))

;; Disturbed treasure
(defn mark-treasure-disturbed
  "Mark a position as having had treasure taken"
  [s pos]
  (update s :disturbed-treasure (fnil conj #{}) pos))

(defn get-visible-disturbed
  "Get disturbed positions visible in sight-set"
  [s sight-set]
  (let [disturbed (:disturbed-treasure s)]
    (set (filter #(contains? sight-set %) disturbed))))

(defn notice-disturbed
  "Check for newly noticed disturbed spots and potentially make Caretaker aware.
   If can-gain-awareness? is false (Caretaker is distracted by tasks), awareness won't trigger."
  ([s sight-set]
   (notice-disturbed s sight-set true))
  ([s sight-set can-gain-awareness?]
   (let [visible (get-visible-disturbed s sight-set)
         already-seen (get-in s [:characters :caretaker :disturbed-memory] #{})
         new-spots (clojure.set/difference visible already-seen)]
     (if (empty? new-spots)
       s
       (let [updated (-> s
                         (update-in [:characters :caretaker :disturbed-memory]
                                    (fnil #(clojure.set/union % new-spots) #{}))
                         (update-in [:characters :caretaker :disturbed-noticed]
                                    (fnil + 0) (count new-spots)))
             total-noticed (get-in updated [:characters :caretaker :disturbed-noticed] 0)]
         (if (and can-gain-awareness?
                  (>= total-noticed c/disturbed-alert-threshold)
                  (not (is-aware? updated)))
           (-> updated
               make-aware
               (update-status-message "Something is wrong... things are missing!"))
           updated))))))

;; Patrol tasks
(defn add-patrol-task
  "Add a patrol task to the queue"
  [s task]
  (update s :patrol-tasks (fnil conj []) task))

(defn get-pending-tasks
  "Get the list of pending patrol tasks"
  [s]
  (or (:patrol-tasks s) []))

(defn complete-patrol-task
  "Remove completed task from queue (first matching location)"
  [s loc]
  (let [tasks (get-pending-tasks s)
        remaining (vec (remove #(= (:loc %) loc) tasks))]
    (assoc s :patrol-tasks remaining)))

(defn check-patrol-overflow
  "Check if too many patrol tasks have accumulated"
  [s]
  (>= (count (get-pending-tasks s)) c/patrol-task-overflow))

(defn get-current-patrol-target
  "Get the location of the first pending patrol task"
  [s]
  (when-let [task (first (get-pending-tasks s))]
    (:loc task)))

;; Sabotage
(defn sabotage-location
  "Mark a location as sabotaged"
  [s loc]
  (update s :sabotaged (fnil conj #{}) loc))

(defn fix-sabotaged
  "Remove sabotage from a location"
  [s loc]
  (update s :sabotaged (fnil disj #{}) loc))

(defn is-sabotaged?
  "Check if a location is sabotaged"
  [s loc]
  (contains? (or (:sabotaged s) #{}) loc))

;; Scores
(defn add-points
  "Add points to a player's score"
  [s player points]
  (update-in s [:scores player] (fnil + 0) points))

(defn get-score
  "Get a player's score"
  [s player]
  (get-in s [:scores player] 0))
