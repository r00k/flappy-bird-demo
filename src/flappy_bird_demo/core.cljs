(ns flappy-bird-demo.core
  (:require
   [sablono.core :as sab :include-macros true]
   [figwheel.client :as fw]
   [cljs.core.async :refer [<! timeout]]
   [flappy-bird-demo.templates :as templates])
  (:require-macros
   [cljs.core.async.macros :refer [go-loop go]]))

(enable-console-print!)

(defn floor [x] (.floor js/Math x))

(defn translate [start-pos vel time]
  (floor (+ start-pos (* time vel))))

;    |   0
;    |   1
;    \/  2

(def horizontal-velocity "How fast flappy flies to the right" -0.15)
(def gravity "The force of gravity" 0.035)
(def jump-velocity "Velocity of flappy's jumps" 15)
(def start-y "Flappy's starting height" 312)
(def bottom-y "The height of the ground" 561)
(def flappy-x "Flappy's x position" 212)
(def flappy-width "How wide flappy's avatar is" 57)
(def flappy-height "How tall flappy's avatar is" 41)
(def pillar-spacing "The spacing between pillars" 324)
(def pillar-gap "The space between the top and bottom of a pillar" 258)
(def pillar-width 86)
(def update-interval "Time between game ticks in ms" 10)

(def starting-state {:game-is-running false
                     :should-detect-collisions true
                     :user-has-clicked false
                     :initial-velocity 0
                     :flappy-y start-y
                     :pillars [{:creation-time 0
                                :pos-x 900
                                :cur-x 900
                                :gap-top 200}]})

(defonce game-state (atom starting-state))

(defn curr-pillar-pos [current-time {:keys [pos-x creation-time] }]
  (translate pos-x horizontal-velocity (- current-time creation-time)))

(defn in-pillar? [{:keys [cur-x]}]
  (and (>= (+ flappy-x flappy-width)
           cur-x)
       (< flappy-x (+ cur-x pillar-width))))

(defn in-pillar-gap? [{:keys [flappy-y]} {:keys [gap-top]}]
  (and (< gap-top flappy-y)
       (> (+ gap-top pillar-gap)
          (+ flappy-y flappy-height))))

(defn bottom-collision? [{:keys [flappy-y]}]
  (>= flappy-y (- bottom-y flappy-height)))

(defn collision? [{:keys [pillars] :as st}]
  (if (:should-detect-collisions st)
    (if (some #(or (and (in-pillar? %)
                        (not (in-pillar-gap? st %)))
                   (bottom-collision? st))
              pillars)
      (assoc st :game-is-running false)
      st)
    st))

(defn new-pillar [current-time pos-x]
  {:creation-time current-time
   :pos-x      pos-x
   :cur-x      pos-x
   :gap-top    (+ 60 (rand-int (- bottom-y 120 pillar-gap)))})

(defn update-pillars [{:keys [pillars current-time] :as st}]
  (let [pillars-with-pos (map #(assoc % :cur-x (curr-pillar-pos current-time %)) pillars)
        pillars-in-world (sort-by
                          :cur-x
                          (filter #(> (:cur-x %) (- pillar-width)) pillars-with-pos))]
    (assoc st
      :pillars
      (if (< (count pillars-in-world) 3)
        (conj pillars-in-world
              (new-pillar
               current-time
               (+ pillar-spacing
                  (:cur-x (last pillars-in-world)))))
        pillars-in-world))))

(defn sine-wave [st]
  (assoc st
    :flappy-y
    (+ start-y (* 30 (.sin js/Math (/ (:time-delta st) 300))))))

(defn update-flappy [{:keys [time-delta initial-velocity flappy-y user-has-clicked] :as st}]
  (if user-has-clicked
    (let [cur-vel (- initial-velocity (* time-delta gravity))
          new-y   (- flappy-y cur-vel)
          new-y   (if (> new-y (- bottom-y flappy-height))
                    (- bottom-y flappy-height)
                    new-y)]
      (assoc st
        :flappy-y new-y))
    (sine-wave st)))

(defn score [{:keys [current-time game-start-time] :as st}]
  (let [score (- (.abs js/Math (floor (/ (- (* (- current-time game-start-time) horizontal-velocity) 544)
                               pillar-spacing)))
                 4)]
  (assoc st :score (if (neg? score) 0 score))))


(defn jump [{:keys [current-time user-has-clicked] :as state}]
  (-> state
      (assoc
          :user-has-clicked true
          :time-of-last-click current-time
          :initial-velocity jump-velocity)))

;; derivatives

(defn border [{:keys [current-time] :as state}]
  (-> state
      (assoc :border-pos (mod (translate 0 horizontal-velocity current-time) 23))))

(defn pillar-offset [{:keys [current-time]} {:keys [gap-top] :as p}]
  (assoc p
    :upper-height gap-top
    :lower-height (- bottom-y gap-top pillar-gap)))

(defn pillar-offsets [state]
  (update-in state [:pillars]
             (fn [pillars]
               (map (partial pillar-offset state)
                    pillars))))

(defn world [state]
  (-> state
      border
      pillar-offsets))

(defn update-game [time]
  (swap! game-state (fn [state]
                      (-> state
                          (assoc
                            :current-time time
                            :time-delta (- time (:time-of-last-click state)))
                          update-flappy
                          update-pillars
                          collision?
                          score))))

(defn time-loop [time]
  (update-game time)
  (when (:game-is-running @game-state)
    (go
      (<! (timeout update-interval))
      (.requestAnimationFrame js/window time-loop))))

(defn set-initial-game-state [time]
  (reset! game-state
    (-> starting-state
        (update-in [:pillars]
                   (fn [pillars] (map #(assoc % :creation-time time) pillars)))
        (assoc
            :game-start-time time
            :time-of-last-click time
            :game-is-running true))))

(defn start-game []
  (.requestAnimationFrame
   js/window
   (fn [time]
     (set-initial-game-state time)
     (time-loop time))))

; TODO: pass in a chan for events, rather than fns
(let [node (.getElementById js/document "board-area")
      jump-callback-fn (fn [e] (swap! game-state jump) (.preventDefault e))]
  (defn render [full-state]
    (.renderComponent js/React
                      (templates/main
                        (-> full-state
                            (assoc :start-fn start-game)
                            (assoc :event-chan event-chan)
                            (assoc :jump-callback-fn jump-callback-fn)))
                      node)))

(add-watch game-state :renderer (fn [_ _ _ new-state]
                                  (render (world new-state))))

#_(add-watch
  game-state
  :state-dump
  (fn [_ _ _ n]
    (.log js/console (str "" (:time-of-last-click n)))))

(reset! game-state @game-state)

(fw/watch-and-reload :jsload-callback #((reset! game-state @game-state)))
