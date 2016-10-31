(ns canvas.core
  (:require [thi.ng.geom.core.vector :as v :refer [vec2]]
            [thi.ng.geom.core :as g]
            [goog.Timer :as Timer]
            [goog.events :as events]
            [goog.dom :as dom]
            [goog.events.EventType :as event-type]))

(enable-console-print!)

(.log js/console "weee")

(defn surface [canvas]
  [(.getContext canvas "2d")
   (. canvas -width)
   (. canvas -height)])

(defn fill-rect [[surface] [x y width height] [r g b]]
  (set! (. surface -fillStyle) (str "rgb(" r "," g "," b ")"))
  (.fillRect surface x y width height))

(defn stroke-rect [[surface] [x y width height] line-width [r g b]]
  (set! (. surface -strokeStyle) (str "rgb(" r "," g "," b ")"))
  (set! (. surface -lineWidth) line-width)
  (.strokeRect surface x y width height))

(defn fill-circle [[surface] coords [r g b]]
  (let [[x y d] coords]
    (set! (. surface -fillStyle) (str "rgb(" r "," g "," b ")"))
    (. surface (beginPath))
    (.arc surface x y d 0 (* 2 Math/PI) true)
    (. surface (closePath))
    (. surface (fill))))

(def particle-dots [0 45 90 135 180 225 270 315])
(def TO_RADIANS (/ js/Math.PI 180))

(defn draw-particle [[surface] {:keys [x y r] :as particle}]
  (let [radius (v/vec2 r 0)
        rotated (->> particle-dots
                     (map (fn [angle]
                            (g/rotate radius (* angle TO_RADIANS)))))]
    (. surface (save))
    (. surface (translate x y))
    (set! (. surface -fillStyle) (str "#ffffff"))
    (set! (. surface -lineWidth) 1)
    (. surface (beginPath))
    (doseq [[x y] rotated]
      (. surface (lineTo x y)))
    (. surface (closePath))
    (. surface (stroke))
    (. surface (restore))))

(defn clear [[surface]]
  (. surface (clearRect 0 0 1200 880)))

(defn move-particle
  [{:keys [x y vel] :as particle}]
  (let [[vx vy] vel
        moved-particle (-> particle
                           (assoc :x (+ x vx)
                                  :y (+ y vy)))]
    (let [mirror-particle (cond-> moved-particle
                                  (< (+ (:x moved-particle) (:r moved-particle)) 0)
                                  (assoc :x (+ 1200 (:r moved-particle)))

                                  (< 1200 (- (:x moved-particle) (:r moved-particle)))
                                  (assoc :x (- (:r moved-particle)))

                                  (< (+ (:y moved-particle) (:r moved-particle)) 0)
                                  (assoc :y (+ 880 (:r moved-particle)))

                                  (< 880 (- (:y moved-particle) (:r moved-particle)))
                                  (assoc :y (- (:r moved-particle)))
                                  )]
      mirror-particle)))

(defn update-particles
  [{:keys [particles] :as state}]
  (let [destoyed-particles (filter (fn [p]
                                     (= nil (:destroy p))) particles)
        new-particles-pos (map move-particle destoyed-particles)]
    (assoc state :particles new-particles-pos)))

(defn loop! [surface !state]
  (clear surface)
  (swap! !state update-particles)
  (doseq [particle (:particles @!state)]
    (draw-particle surface particle)))

(defn init-canvas [width height]
  (let [canvas (dom/getElement "surface")]
    (set! (. canvas -width) width)
    (set! (. canvas -height) height)
    canvas))

(defn click-canvas [!state e]
  (let [x (.-clientX e)
        y (.-clientY e)
        !duplicate-particle (atom nil)
        clicked-particles (doall (->> (:particles @!state)
                                      (map (fn [particle]
                                             (let [diff-particle {:x (- (:x particle) x)
                                                                  :y (- (:y particle) y)}
                                                   distance-squared (+ (* (:x diff-particle) (:x diff-particle)) (* (:y diff-particle) (:y diff-particle)))
                                                   clicked? (< distance-squared (* (:r particle) (:r particle)))]

                                               (if clicked?
                                                 (do (if (< (:r particle) 1)
                                                       (assoc particle :destroy true)
                                                       (do
                                                         (reset! !duplicate-particle {:x (:x particle)
                                                                                      :y (:y particle)
                                                                                      :r (/ (:r particle) 2)
                                                                                      :vel [(rand-nth (range -5 5)) (rand-nth (range -5 5))]})
                                                         (assoc particle :r (/ (:r particle) 2)))))
                                                 particle))))))]
    (if @!duplicate-particle
      (swap! !state assoc :particles (conj clicked-particles @!duplicate-particle))
      (swap! !state assoc :particles clicked-particles))

    ))

(defn ^:export init []
  (let [canvas (init-canvas 1200 880)
        surface (surface canvas)
        timer (goog.Timer. (/ 1000 60))
        points (v/vec2 50 0)
        rotated (for [angle particle-dots]
                  (let [[x y] (g/rotate points (* angle TO_RADIANS))]
                    ;(println "x:" x " y: " y)
                    ;(println "angle:" angle)
                    [x y]))
        octagne-points (map (fn [[x y]]
                              [x y]
                              ) rotated)
        particles (take 20 (repeatedly #(hash-map :x (rand-int 1201)
                                                  :y (rand-int 881)
                                                  :r (rand-nth (range 5 50))
                                                  :vel [(rand-nth (range -5 5)) (rand-nth (range -5 5))])))
        !state (atom {:particles particles})]

    (events/listen timer Timer/TICK #(loop! surface !state))
    (.start timer)

    (events/listen canvas event-type/CLICK #(click-canvas !state %))))


(defn ^:export reload []
  (println "reload!")
  )