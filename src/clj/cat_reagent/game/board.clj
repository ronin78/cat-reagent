(ns cat-reagent.game.board
  (:require [loom.graph :as graph]
            [loom.alg :as alg]
            [clojure.set :as set]
            [bigml.sampling.simple :as simple]
            [cat-reagent.game.constants :as c]
            [cat-reagent.game.util :as u]
            [cat-reagent.game.state :as st]))

;; Graph operations
(defn make-edges
  "Make edges from grid."
  ([size]
   (let [sv c/space-nums
         space-set (set sv)
         adj-vec [-1 +1 -32 +32]
         dir-vec [:left :right :up :down]
         adj-fn (fn [s]
                  (map #(+ % s) adj-vec))
         make-link (fn [s]
                     (map (fn [sa d] (cond (contains? space-set sa) [(u/keynum s) (u/keynum sa) {:direction d}]))
                          (adj-fn s) dir-vec))]
     (vec (filter #(not (nil? %)) (mapcat make-link sv)))))
  ([s w]
   (let [sv (vec (map #(+ 1 %) (range (* (/ (first s) 100) (/ (second s) 100) 12))))
         space-set (set sv)
         adj-vec [-1 +1 (- c/row-length) c/row-length]
         adj-fn (fn [s]
                  (map #(+ % s) adj-vec))
         make-link (fn [s]
                     (map #(cond (contains? space-set %) [(u/keynum s) (u/keynum %) w]) (adj-fn s)))]
     (vec (filter #(not (nil? %)) (mapcat make-link sv))))))

(defn cut-edge
  [g e]
  (graph/remove-edges g e (reverse e)))

(defn add-edge
  ([g e]
   (graph/add-edges g e (reverse e)))
  ([g e w]
   (graph/add-edges g (conj e w) (conj (apply vector (reverse e)) w))))

(defn cut-edges
  [g l]
  (if (empty? l)
    g
    (recur (cut-edge g (first l)) (next l))))

(defn add-edges
  ([g l]
   (if (empty? l)
     g
     (recur (add-edge g (first l)) (next l))))
  ([g l w]
   (if (empty? l)
     g
     (recur (add-edge g (first l) w) (next l) w))))

(defn cut-borders
  [g]
  (let [row-ends (mapv #(* c/row-length %) (range 1 (+ 1 c/num-rows)))
        cut (mapv #(vector (u/keynum %) (u/keynum (+ 1 %))) row-ends)]
    (cut-edges g cut)))

(defn cut-walls
  [g ws]
  (let [adj-vec [1 -1 c/row-length (- c/row-length)]]
    (cut-edges g (vec (mapcat (fn [w] (mapv #(vector w (u/keynum (+ % (u/keyword-to-int w)))) adj-vec)) ws)))))

(defn sound-walls
  [g ws]
  (let [adj-vec [1 -1 c/row-length (- c/row-length)]]
    (add-edges g (vec (mapcat (fn [w] (mapv #(vector w (u/keynum (+ % (u/keyword-to-int w)))) adj-vec)) ws)) 2)))

;; Wall creation
(defn make-vwall
  "Make a vertical wall in column c of length 1 offset from the top by o, with a hole in d."
  ([col l o]
   (set (map #(u/keynum (+ (* c/row-length %) col)) (range o (+ o l)))))
  ([col l o d]
   (set (map #(u/keynum (+ (* c/row-length %) col)) (u/drop-nth (- d 1) (range o (+ o l)))))))

(defn make-hwall
  "Make a horizontal wall in row r of length 1 offset from the left by o, with a hole in d."
  ([r l o]
   (set (map #(u/keynum (+ (+ (* c/row-length (- r 1)) (+ o 1)) %)) (range l))))
  ([r l o d]
   (set (u/drop-nth (- d 1) (map #(u/keynum (+ (+ (* c/row-length r) (+ o 1)) %)) (range l))))))

(defn make-neighbors
  [g]
  (let [house-edges1 (map u/keynum (map #(+ 1 (* % c/row-length)) (range c/num-rows)))
        house-edges2 (map u/keynum (map #(+ c/row-length (* % c/row-length)) (range c/num-rows)))]
    (-> g
        (graph/add-nodes [:n1 :n2])
        (add-edges (mapv #(vector :n1 %) house-edges1) 3)
        (add-edges (mapv #(vector :n2 %) house-edges2) 3))))

;; Random placement
(defn rand-non-overlap
  [st n stack?]
  (let [wall-set (set (map u/keynum c/space-nums))
        sample-vec (vec (set/difference wall-set st))]
    (cond
      stack? (frequencies (take n (simple/sample sample-vec :replace true)))
      :else (set (take n (simple/sample sample-vec))))))

(defn rand-concentrated-treasure
  "Place total-treasure amount across num-positions spots.
   Results in higher-value treasure piles."
  [excluded-set total-treasure num-positions]
  (let [all-positions (set (map u/keynum c/space-nums))
        available (vec (clojure.set/difference all-positions excluded-set))
        ;; Pick the positions
        positions (take num-positions (simple/sample available))
        ;; Distribute treasure: sample positions with replacement
        distribution (frequencies (take total-treasure (simple/sample (vec positions) :replace true)))]
    distribution))

;; Map symbol generation
(defn map-to-symbol
  ([s]
   (let [[vwall-set hwall-set] (mapv u/union (u/select-key-vals s [:vwalls :hwalls]))
         [phone-set treasure-set door-set] (mapv u/set-of-keys (u/select-key-vals s [:phone-map :treasure-map :door-list]))]
     (into {} (map (fn [c k] (into {} (map #(hash-map % (k c/item-map)) c))) [vwall-set hwall-set phone-set treasure-set door-set] [:vwall :hwall :phone :treasure :front-door]))))
  ([s v char-key]
   (let [other-player (st/other-player s)
         character-map (hash-map (st/character-position other-player) ((st/other-player-key s) c/item-map))
         ;; Add clue markers for Caretaker
         footprint-map (when (= char-key :caretaker)
                         (let [visible-fps (st/get-visible-footprints s (set v))]
                           (into {} (map #(hash-map % (:footprint c/item-map)) visible-fps))))
         disturbed-map (when (= char-key :caretaker)
                         (let [visible-disturbed (st/get-visible-disturbed s (set v))]
                           (into {} (map #(hash-map % (:disturbed c/item-map)) visible-disturbed))))
         patrol-target-map (when (= char-key :caretaker)
                             (when-let [target (st/get-current-patrol-target s)]
                               ;; Always show patrol target, even outside visible area
                               {target (:patrol-target c/item-map)}))
         sabotaged-map (when (= char-key :caretaker)
                         (let [sabotaged (:sabotaged s)
                               visible-sabotaged (set/intersection (or sabotaged #{}) (set v))]
                           (into {} (map #(hash-map % (:sabotaged c/item-map)) visible-sabotaged))))
         ;; Priority (highest to lowest): character > sabotaged > patrol-target > objects > disturbed > footprint
         ;; Later maps in merge override earlier ones, so put lowest priority first
         ;; Patrol target merged last so it's always visible (even outside normal view range)
         visible-objects (select-keys (merge footprint-map disturbed-map (map-to-symbol s) sabotaged-map character-map) v)
         object-map (merge visible-objects patrol-target-map)]
     (merge object-map (into {} (map #(hash-map % ".") (filter #(not (contains? object-map %)) v)))))))

;; Direction and movement
(defn direction
  [v]
  (let [diff (- (u/keyword-to-int (second v)) (u/keyword-to-int (first v)))]
    (cond
      (= diff 1) :right
      (= diff -1) :left
      (= diff (- c/row-length)) :up
      (= diff c/row-length) :down)))

(defn can-move?
  ([move-length max-move]
   (when (<= move-length max-move) move-length))
  ([g b e max-move]
   (when-let [p (alg/dijkstra-path-dist g b e)]
     (when (<= (second p) max-move) (second p)))))

(defn pad-out-spaces
  ([g w space d l]
   (let [dir-step (case d
                    :up (- c/row-length)
                    :down c/row-length
                    :left -1
                    :right 1)
         step (map u/keynum (iterate (partial + dir-step) (u/keyword-to-int space)))]
     (loop [cp space
            steps (next step)
            v []]
       (cond
         (can-move? g cp (first steps) l) (recur cp (next steps) (conj v (first steps)))
         (contains? w (first steps)) (conj (conj v (first steps)) cp)
         :else (conj v cp))))))

;; Line of sight
(defn can-see?
  ([s pos dir]
   (let [g (:move-edges s)
         w (u/union-all (:vwalls s) (:hwalls s))
         dir-step (case dir
                    :up (- c/row-length)
                    :down c/row-length
                    :left -1
                    :right 1)
         step (map u/keynum (iterate (partial + dir-step) (u/keyword-to-int pos)))]
     (loop [cp pos
            steps (next step)
            v []]
       (cond
         (can-move? g cp (first steps) 1) (recur (first steps) (next steps) (conj v cp))
         (contains? w (first steps)) (conj (conj v (first steps)) cp)
         :else (conj v cp)))))
  ([s pos dir limit]
   (let [g (:move-edges s)
         w (u/union-all (:vwalls s) (:hwalls s))
         compare-fn #(compare (u/keyword-to-int %1) (u/keyword-to-int %2))
         sort-fn (fn [v]
                   (cond
                     (or (= dir :up) (= dir :left)) (reverse (sort compare-fn v))
                     :else (sort compare-fn v)))
         v (sort-fn (can-see? s pos dir))
         dir-vec (cond
                   (or (= dir :up) (= dir :down)) [:left :right]
                   :else [:up :down])
         pad-fn (fn [g w space d l]
                  (let [dir-step (case d
                                   :up (- c/row-length)
                                   :down c/row-length
                                   :left -1
                                   :right 1)
                        step (map u/keynum (iterate (partial + dir-step) (u/keyword-to-int space)))]
                    (loop [cp space
                           steps (next step)
                           v []]
                      (cond
                        (can-move? g cp (first steps) l) (recur cp (next steps) (conj v (first steps)))
                        (contains? w (first steps)) (conj (conj v (first steps)) cp)
                        :else (conj v cp)))))]
     (set
       (flatten
         (map
           #(loop [s v
                   l limit
                   v []]
              (let [result (pad-fn g w (first s) % l)
                    newl (min l (- (count (filter (fn [r] (not (contains? w r))) result)) 1))]
                (cond
                  (empty? s) v
                  :else (recur (next s) newl (conj v result)))))
           dir-vec))))))

;; Map rendering
(defn treasure-color
  "Return color based on treasure value"
  [value]
  (cond
    (>= value 5) "gold"      ; jackpot!
    (>= value 3) "orange"    ; good haul
    (>= value 2) "yellow"    ; decent
    :else "green"))          ; standard

(defn map-pos-to-char
  [s pk]
  (let [p (get-in s [:characters pk])
        visible-set (set (can-see? s (:loc p) (:face p) c/see-around))
        select-map (map-to-symbol s visible-set pk)
        memory-map (:memory-map p)
        ;; Add footprint memory for Caretaker (shown in grey)
        footprint-memory-map (when (= pk :caretaker)
                               (let [fps-memory (get-in s [:characters :caretaker :footprint-memory] #{})
                                     ;; Only show remembered footprints not currently visible
                                     old-fps (set/difference fps-memory visible-set)]
                                 (into {} (map #(hash-map % (:footprint c/item-map)) old-fps))))
        cm (if (= (st/character-position s :cat) (st/character-position s :caretaker))
             {(st/character-position s :cat) (st/color-vec (:fight c/item-map) pk false)}
             {(st/character-position s pk) (st/color-vec (pk c/item-map) pk false)})
        mm (into {} (map #(hash-map (key %) (st/color-vec (val %) pk true)) memory-map))
        ;; Add footprint memory to memory map (in grey)
        fpmm (into {} (map #(hash-map (key %) (st/color-vec (val %) pk true)) footprint-memory-map))
        sm (into {} (map #(hash-map (key %) (st/color-vec (val %) pk false)) select-map))
        ;; Color treasure based on value
        treasure-map (:treasure-map s)
        visible-treasure (filter #(contains? visible-set (key %)) treasure-map)
        tm (into {} (map (fn [[pos val]]
                           {pos [(:treasure c/item-map) (treasure-color val)]})
                         visible-treasure))
        om (when (contains? select-map (st/character-position s (st/opponent-key pk))) (hash-map (get-in s [:characters (st/opponent-key pk) :loc]) (st/color-vec ((st/opponent-key pk) c/item-map) pk false)))
        nm (when-let [noise-icon (:noise-icon p)]
             (when-let [noise-loc (:pos noise-icon)]
               (hash-map noise-loc (st/color-vec (:noise c/item-map) pk false))))]
    (-> (merge mm fpmm sm tm nm om cm)  ; tm after sm to override default treasure colors
        (cond-> (get-in s [:characters pk :cursor]) (assoc (last (get-in s [:characters pk :cursor])) (st/color-vec (:cursor c/item-map) pk false))))))

;; Default wall configurations
(def default-vwalls [(make-vwall 13 8 0 6)
                     (make-vwall 3 4 8 2)
                     (make-vwall 11 5 (- c/num-rows 5) 2)])

(def default-hwalls [(make-hwall 3 4 4)
                     (make-hwall 7 12 0 4)
                     (make-hwall 12 11 0 5)])

;; Base graph
(def base-graph (cut-borders (apply graph/weighted-digraph (make-edges c/size 1))))
