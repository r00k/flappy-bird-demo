(ns flappy-bird-demo.templates
  (:require
    [sablono.core :as sab :include-macros true]))

(defn px [n] (str n "px"))

(defn pillar [{:keys [cur-x upper-height lower-height]}]
  [:div.pillars
   [:div.pillar.pillar-upper {:style {:left (px cur-x)
                                      :height upper-height}}]
   [:div.pillar.pillar-lower {:style {:left (px cur-x)
                                      :height lower-height}}]])

(defn main [{:keys [score current-time user-has-clicked
                    game-is-running border-pos
                    flappy-y pillars start-fn jump-callback-fn event-chan]}]
  (sab/html [:div.board {:onMouseDown jump-callback-fn}
             [:h1.score score]
             (if-not game-is-running
               [:a.start-button {:onClick #(start-fn)}
                (if user-has-clicked "RESTART" "START")]
               [:span])
             [:div (map pillar pillars)]
             [:div.flappy {:style {:top (px flappy-y)}}]
             [:div.scrolling-border {:style
                                     {:background-position-x
                                      (px border-pos)}}]]))
