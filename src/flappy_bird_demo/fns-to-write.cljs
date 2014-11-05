; v0
(defn jump [state]
  (assoc state
         :user-has-clicked true
         :time-of-last-click (get state :current-time)
         :flappy-velocity jump-velocity))

; Final
(defn jump [{:keys [current-time] :as state}]
  (assoc state
         :user-has-clicked true
         :time-of-last-click current-time
         :flappy-velocity jump-velocity))


; v0
(defn sine-wave [state]
  (let [time-delta (:time-delta state)]
    (assoc state
          :flappy-y
          (.sin js/Math time-delta))))

; Final
(defn sine-wave [state]
  (->> (:time-delta state)
       (* 0.0033)
       (.sin js/Math)
       (* 30)
       (+ start-y)
       (assoc state :flappy-y)))


; v0
(defn score [state]
  (let [elapsed-time (- (:current-time state) (:game-start-time state))]
    (assoc state :score elapsed-time)))

; v1
(defn score [state]
  (let [elapsed-time (- (:current-time state) (:game-start-time state))
        distance-traveled (- (* elapsed-time horizontal-velocity)
                             pillar-free-distance)
        pillars-passed (floor (/ distance-traveled pillar-spacing))]
    (assoc state :score pillars-passed)))

; Final
(defn score [{:keys [current-time game-start-time] :as state}]
  (let [elapsed-time (- current-time game-start-time)
        distance-traveled (- (* elapsed-time horizontal-velocity)
                             pillar-free-distance)
        pillars-passed (floor (/ distance-traveled pillar-spacing))]
    (assoc state :score (if (neg? pillars-passed) 0 pillars-passed))))


; v0
(defn collision? [state]
  (assoc
    state
    :game-is-running
    (not (touching-ground? state))))

; v1
(defn collision? [state]
  (assoc
    state
    :game-is-running
    (and (not (touching-ground? state))
         (not-any? (fn
                     [pillar]
                     (and (in-pillar? pillar)
                          (not (in-pillar-gap? state pillar))))
                   (:pillars state)))))

; Final
(defn- touching-pillar? [state pillar]
  (and (in-pillar? pillar)
       (not (in-pillar-gap? state pillar))))

(defn collision? [{:keys [pillars] :as state}]
  (assoc
    state
    :game-is-running
    (and (not-any? (partial touching-pillar? state) pillars)
         (not (touching-ground? state)))))

