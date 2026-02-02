(ns cat-reagent.game.combat
  (:require [cat-reagent.game.constants :as c]
            [cat-reagent.game.util :as u]
            [cat-reagent.game.state :as st]
            [cat-reagent.game.sound :as snd]))

;; Combat functions return a map with :state and :message
;; The caller (game.clj) handles turn changes via update-and-change

(defn subdue
  "Attempt to subdue opponent's body part. Returns {:state s :message m :end-turn? bool}"
  [s k]
  (let [p (st/current-player s)
        op (st/other-player s)
        opk (st/other-player-key s)
        ucc st/update-current-player
        uoc st/update-other-player
        cs (if-let [opp-bp (st/body-part op k)] opp-bp 0)
        bp (name k)
        oc ((st/other-player-key s) c/char-keystring)
        your-arms (if-let [yours (st/body-part p :arms)] yours 0)
        roll (cond
               (and (= :cat (:turn s)) (> your-arms 6)) (u/dice-roll 2)
               (= :cat (:turn s)) (+ 2 (u/dice-roll))  ;; Cat is an expert: +2 bonus
               (> your-arms 6) (- (u/dice-roll) 3)
               :else (u/dice-roll))
        opp-roll (if (> cs 6) (- (u/dice-roll) 3) (u/dice-roll))
        bind-mat (if (> cs 0) (st/character-attribute s opk (keyword (str bp "-bindmat"))) (rand-nth ["tape" "rope"]))]
    (cond
      (>= your-arms 12) {:state s :message "You can't subdue with your arms bound." :end-turn? false}
      (>= cs 12) {:state s :message (str "Your opponent's " bp " are already fully bound.") :end-turn? false}
      (<= roll opp-roll) {:state (st/update-other-message s "Your opponent tries to subdue your " bp ", but you escape!")
                          :message (str "You try to subdue the " oc "'s " bp ", but they escape!")
                          :end-turn? true}
      (> cs 0) {:state (-> s
                           (uoc k (+ cs (- roll opp-roll)))
                           (cond-> (= k :arms) (ucc :muffled? false))
                           (st/update-other-message "Your opponent wraps more " bind-mat " around your " bp "!"))
                :message (str "You grab more " bind-mat " and wrap it around " oc "'s " bp ".")
                :end-turn? true}
      :else {:state (-> s
                        (uoc k (+ cs (- roll opp-roll)))
                        (cond-> (= k :arms) (ucc :muffled? false))
                        (uoc (keyword (str bp "-bindmat")) bind-mat)
                        (st/update-other-message "Your opponent wraps " bind-mat " around your " bp "!"))
             :message (str "You grab some " bind-mat " and wrap it around " oc "'s " bp ".")
             :end-turn? true})))

(defn resist
  "Attempt to escape bindings. Returns {:state s :message m :end-turn? bool}"
  [s k]
  (let [p (st/current-player s)
        turn (:turn s)
        arms (st/body-part p :arms)
        bind-mat ((keyword (str (name k) "-bindmat")) p)
        roll (cond
               (> arms 6) (u/dice-roll 4)
               (> arms 0) (u/dice-roll 2)
               :else (u/dice-roll))
        current-subdue (st/body-part p k)]
    (cond
      (= current-subdue 0) {:state s :message "You are not bound there." :end-turn? false}
      (<= roll 0) {:state s :message (str "You struggle but make no headway against the " bind-mat ".") :end-turn? true}
      (>= roll current-subdue) {:state (-> s
                                           (st/update-current-player k 0)
                                           (update-in [:characters turn] dissoc (keyword (str (name k) "-bindmat"))))
                                :message (str "You struggle free of the " bind-mat "!")
                                :end-turn? true}
      :else {:state (st/update-current-player s k #(- % roll))
             :message (str "You loosen the " bind-mat "!")
             :end-turn? true})))

(defn ungag
  "Remove gag. Automatic success if arms are free. Returns {:state s :message m :end-turn? bool}"
  [s]
  (let [p (st/current-player s)
        turn (:turn s)
        arms (st/body-part p :arms)
        over-mouth (:over-mouth-mat p)
        in-mouth (:in-mouth-mat p)]
    (cond
      (not in-mouth) {:state s :message "You are not gagged." :end-turn? false}
      (> arms 0) {:state s :message "You can't reach your mouth!" :end-turn? false}
      ;; Arms free - automatic success
      over-mouth {:state (-> s (update-in [:characters turn] dissoc :over-mouth-mat))
                  :message (str "You peel the " over-mouth " away from your lips!")
                  :end-turn? true}
      :else {:state (-> s (update-in [:characters turn] dissoc :in-mouth-mat))
             :message (str "You spit out the " in-mouth "!")
             :end-turn? true})))

(defn gag
  "Attempt to gag opponent. Returns {:state s :message m :end-turn? bool}"
  [s]
  (let [p (st/current-player s)
        op (st/other-player s)
        opk (st/other-player-key s)
        uoc st/update-other-player
        cs (if-let [opp-bp (st/body-part op :arms)] opp-bp 0)
        bp "mouth"
        cc ((st/current-player-key s) c/char-keystring)
        oc ((st/other-player-key s) c/char-keystring)
        your-arms (if-let [yours (st/body-part p :arms)] yours 0)
        roll (cond
               (and (= :cat (:turn s)) (> your-arms 6)) (u/dice-roll 2)
               (= :cat (:turn s)) (+ 2 (u/dice-roll))  ;; Cat is an expert: +2 bonus
               (> your-arms 6) (- (u/dice-roll) 3)
               :else (u/dice-roll))
        opp-roll (if (> cs 6) (- (u/dice-roll) 3) (u/dice-roll))
        in-mouth-mat (rand-nth ["ball" "sock" "handkerchief" "stuffed animal"])
        over-mouth-mat (rand-nth ["tape" "a scarf" "a handkerchief"])]
    (cond
      (>= your-arms 12) {:state s :message "You can't subdue with your arms bound." :end-turn? false}
      (< cs 1) {:state s :message (str "The " oc "'s arms are free - you can't gag them!") :end-turn? false}
      (st/character-attribute s opk :over-mouth-mat) {:state s :message (str "The " oc " is already gagged.") :end-turn? false}
      (<= roll opp-roll) {:state (st/update-other-message s "Your opponent tries to subdue your mouth, but you escape!")
                          :message (str "You try to subdue the " oc "'s mouth, but they escape!")
                          :end-turn? true}
      (st/character-attribute s opk :in-mouth-mat) {:state (-> s
                                                               (uoc :over-mouth-mat over-mouth-mat)
                                                               (st/update-other-message "The " cc " gags you with " over-mouth-mat "!"))
                                                    :message (str "You gag the " oc " with " over-mouth-mat "!")
                                                    :end-turn? true}
      :else {:state (-> s
                        (uoc :in-mouth-mat in-mouth-mat)
                        (st/update-other-message "The " cc " shoves a " in-mouth-mat " in your mouth!"))
             :message (str "You shove a " in-mouth-mat " in the " oc "'s mouth!")
             :end-turn? true})))

(defn sabotage
  [s pos noise]
  (-> s
      (assoc-in [:phone-map pos] false)
      (snd/hear-alert noise)))

(defn fix-phone
  [s pos]
  (let [roll (u/dice-roll)
        noise (u/dice-roll 1)]
    (if (>= roll 4)
      [(-> s (assoc-in [:phone-map pos] true) (snd/hear-alert noise)) "You fix the phone! You make a " (snd/noise-word noise) "."]
      [(-> s (snd/hear-alert noise)) "You fail to fix the phone! You make a " (snd/noise-word noise) "."])))

(defn yell
  "Attempt to yell for help. Returns {:state s :message m :end-turn? bool}
   Muffled yells produce reduced noise - effectiveness depends on distance to windows."
  [s]
  (let [noise (+ (u/dice-roll) (u/dice-roll))
        muffled-noise (max 0 (- noise 8))
        gagged-noise (max 0 (- noise 6))
        stuffed-noise (max 0 (- noise 4))]
    (cond
      (= (:turn s) :cat) {:state s :message "What part of 'cat burglar' don't you understand?" :end-turn? false}
      (not (st/is-aware? s)) {:state s :message "What would you yell about?" :end-turn? false}
      ;; Hand over mouth - heavily muffled but might reach nearby windows
      (st/character-attribute (st/current-player s) :muffled?)
      {:state (snd/hear-alert s muffled-noise)
       :message (str "You try to yell through the Cat's hand! (muffled noise: " muffled-noise ")")
       :end-turn? true}
      ;; Tape/cloth over mouth - muffled
      (st/character-attribute (st/current-player s) :over-mouth-mat)
      {:state (snd/hear-alert s gagged-noise)
       :message (str "You try to yell through your gag! (muffled noise: " gagged-noise ")")
       :end-turn? true}
      ;; Something in mouth only - less muffled
      (st/character-attribute (st/current-player s) :in-mouth-mat)
      {:state (snd/hear-alert s stuffed-noise)
       :message (str "Your yell is muffled by the " (st/character-attribute (st/current-player s) :in-mouth-mat) "! (noise: " stuffed-noise ")")
       :end-turn? true}
      ;; Normal yell
      :else {:state (snd/hear-alert s noise) :message (str "You yell! (noise: " noise ")") :end-turn? true})))

(defn muffle
  "Attempt to cover opponent's mouth. Returns {:state s :message m :end-turn? bool}"
  [s]
  (let [p (st/current-player s)
        opp-key (st/other-player-key s)
        opp-string (opp-key c/char-keystring)]
    (cond
      (> (st/body-part p :arms) 0) {:state s :message "You can't muffle anyone with your arms bound." :end-turn? false}
      (st/character-attribute (st/other-player s) :muffled?) {:state s :message (str "The " opp-string " is already muffled.") :end-turn? false}
      :else {:state (st/update-other-message (st/update-other-player s :muffled? true) "The " ((st/current-player-key s) c/char-keystring) " covers your mouth!")
             :message (str "You cover the " opp-string "'s mouth with your hand!")
             :end-turn? true})))

(defn inventory
  [s]
  (let [p (st/current-player s)]
    (st/update-status s "You have " (if-let [t (:treasure p)] t 0) " treasure.")))
