(ns flappy-bird-demo.core
  (:require
   [sablono.core :as sab :include-macros true]
   [figwheel.client :as fw]
   [cljs.core.async :refer [<! chan sliding-buffer put! close! timeout]])
  (:require-macros
   [cljs.core.async.macros :refer [go-loop go]]))

(enable-console-print!)

(defn floor [x] (.floor js/Math x))

(defn translate [start-pos vel time]
  (floor (+ start-pos (* time vel))))

(def horiz-vel -0.15)
(def gravity 0.05)
(def jump-vel 21)
(def start-y 312)
(def bottom-y 561)
(def flappy-x 212)
(def flappy-width 57)
(def flappy-height 41)
(def pillar-spacing 324)
(def pillar-gap 258) ;; 158
(def pillar-width 86)

(def starting-state {:game-is-active false
                     :jump-count 0
                     :initial-vel 0
                     :flappy-y start-y
                     :pillars [{:creation-time 0
                                :pos-x 900
                                :cur-x 900
                                :gap-top 200}]})


(defonce game-state (atom starting-state))

(defn curr-pillar-pos [current-time {:keys [pos-x creation-time] }]
  (translate pos-x horiz-vel (- current-time creation-time)))

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
  (if (some #(or (and (in-pillar? %)
                      (not (in-pillar-gap? st %)))
                 (bottom-collision? st)) pillars)
    (assoc st :game-is-active false)
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

(defn update-flappy [{:keys [time-delta initial-vel flappy-y jump-count] :as st}]
  (if (pos? jump-count)
    (let [cur-vel (- initial-vel (* time-delta gravity))
          new-y   (- flappy-y cur-vel)
          new-y   (if (> new-y (- bottom-y flappy-height))
                    (- bottom-y flappy-height)
                    new-y)]
      (assoc st
        :flappy-y new-y))
    (sine-wave st)))

(defn score [{:keys [current-time game-start-time] :as st}]
  (let [score (- (.abs js/Math (floor (/ (- (* (- current-time game-start-time) horiz-vel) 544)
                               pillar-spacing)))
                 4)]
  (assoc st :score (if (neg? score) 0 score))))


(defn jump [{:keys [current-time jump-count] :as state}]
  (-> state
      (assoc
          :jump-count (inc jump-count)
          :time-of-last-click current-time
          :initial-vel jump-vel)))

;; derivatives

(defn border [{:keys [current-time] :as state}]
  (-> state
      (assoc :border-pos (mod (translate 0 horiz-vel current-time) 23))))

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

(defn px [n] (str n "px"))

(defn pillar [{:keys [cur-x pos-x upper-height lower-height]}]
  [:div.pillars
   [:div.pillar.pillar-upper {:style {:left (px cur-x)
                                       :height upper-height}}]
   [:div.pillar.pillar-lower {:style {:left (px cur-x)
                                       :height lower-height}}]])

(defn time-update [timestamp state]
  (-> state
      (assoc
        :current-time timestamp
        :time-delta (- timestamp (:time-of-last-click state)))
      update-flappy
      update-pillars
      collision?
      score))

(defn time-loop [time]
  (swap! game-state (partial time-update time))
  (when (:game-is-active @game-state)
    (go
      (<! (timeout 30))
      (.requestAnimationFrame js/window time-loop))))

(defn set-initial-game-state [time]
  (reset! game-state
    (-> starting-state
        (update-in [:pillars]
                   (fn [pillars] (map #(assoc % :creation-time time) pillars)))
        (assoc
            :game-start-time time
            :time-of-last-click time
            :game-is-active true))))

(defn start-game []
  (.requestAnimationFrame
   js/window
   (fn [time]
     (set-initial-game-state time)
     (time-loop time))))

(defn main-template [{:keys [score current-time jump-count
                             game-is-active border-pos
                             flappy-y pillars]}]
  (sab/html [:div.board {:onMouseDown (fn [e]
                                        (swap! game-state jump)
                                        (.preventDefault e))}
             [:h1.score score]
             (if-not game-is-active
               [:a.start-button {:onClick #(start-game)}
                (if (< 1 jump-count) "RESTART" "START")]
               [:span])
             [:div (map pillar pillars)]
             [:div.flappy {:style {:top (px flappy-y)}}]
             [:div.scrolling-border {:style { :background-position-x (px border-pos)}}]]))

(let [node (.getElementById js/document "board-area")]
  (defn renderer [full-state]
    (.renderComponent js/React (main-template full-state) node)))

(add-watch game-state :renderer (fn [_ _ _ n]
                                  (renderer (world n))))

#_(add-watch
  game-state
  :state-dump
  (fn [_ _ _ n]
    (.log js/console (str "" (:time-of-last-click n)))))

(reset! game-state @game-state)

(fw/watch-and-reload  :jsload-callback (fn []
                                         ;; you would add this if you
                                         ;; have more than one file
                                         #_(reset! game-state @game-state)
                                         ))
