(ns canvas.core
  (:require [thi.ng.geom.core.vector :as v :refer [vec2]]
            [thi.ng.geom.core :as g]
            [goog.Timer :as Timer]
            [goog.events :as events]
            [goog.dom :as dom]))

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
        movied-particle (-> particle
                            (assoc :x (+ x vx)
                                   :y (+ y vy)))]
    (let [test (cond-> movied-particle
                       (< (+ (:x movied-particle) (:r movied-particle)) 0)
                       (assoc :x (+ 1200 (:r movied-particle)))

                       (< 1200 (- (:x movied-particle) (:r movied-particle)))
                       (assoc :x (- (:r movied-particle)))

                       (< (+ (:y movied-particle) (:r movied-particle)) 0)
                       (assoc :y (+ 880 (:r movied-particle)))

                       (< 880 (- (:y movied-particle) (:r movied-particle)))
                       (assoc :y (- (:r movied-particle)))
                       )]
      ;(println movied-particle)
      test)

    ))

(defn update-particles
  [{:keys [particles] :as state}]
  (let [new-particles-pos (map move-particle particles)]
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
                                                  :r (rand-int 51)
                                                  :vel [(rand-nth (range -5 5)) (rand-nth (range -5 5))]
                                                  :points octagne-points)))
        !state (atom {:particles particles})]
    ;(println octagne-points)



    #_(update-canvas (init-round surface) surface)
    (events/listen timer Timer/TICK #(loop! surface !state))
    (.start timer)

    #_(events/listen js/window event-type/CLICK #(click timer state surface %))
    #_(events/listen js/window event-type/MOUSEMOVE #(mouse-move state surface %))))


(defn ^:export reload []
  (println "reload!")
  )