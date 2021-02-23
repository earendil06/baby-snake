(ns snake.core
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [cljs.spec.alpha :as s]))

(def SIZE 500)
(def SNAKE-SIZE 10)
(def FRAME-RATE 4)
(def MAX-SWEETS 3)
(def FREQ-SWEETS 10)
(def KEY-CODES {39 :right 37 :left 38 :up 40 :down})

(defn arrow->direction [code]
  (or (get KEY-CODES code) :right))

(defn direction->arrow [symbol]
  (let [reversed (clojure.set/map-invert KEY-CODES)]
    (or (get reversed symbol) 39)))

(defn incompatible? [direction1 direction2]
  (let [prod (* (direction->arrow direction1) (direction->arrow direction2))
        forbidden1 (* (direction->arrow :right) (direction->arrow :left))
        forbidden2 (* (direction->arrow :up) (direction->arrow :down))]
    (or (= prod forbidden1) (= prod forbidden2))))

(defn next-direction [current player]
  (if (incompatible? current player) current player))

(defn coords [x y]
  {:x x :y y})

(defn compute-next-position [current next]
  (let [fct #(cond (= next %1) SNAKE-SIZE
                   (= next %2) (- SNAKE-SIZE)
                   :else 0)
        {currentX :x currentY :y} current]
    (coords (+ currentX (fct :right :left)) (+ currentY (fct :down :up)))))

(def any? (complement not-any?))

(defn match? [p1 p2]
  (and (= (:x p1) (:x p2)) (= (:y p1) (:y p2))))

(defn any-match? [coords coll]
  (any? #(match? coords %) coll))


(defn bad-head-position? [head queue]
  (let [size2 (/ SIZE 2)
        {:keys [x y]} head]
    (or (>= x size2) (>= y size2) (<= x (- size2)) (<= y (- size2)) (any-match? head queue))))

(defn queue [size positions]
  (take (- size 1) (rest positions)))

(defn random-coords []
  (coords
    (- (* SNAKE-SIZE (rand-int (/ SIZE SNAKE-SIZE))) (/ SIZE 2))
    (- (* SNAKE-SIZE (rand-int (/ SIZE SNAKE-SIZE))) (/ SIZE 2))))

(defn generate-sweets [sweets head]
  (let [no-matches (filter #(not (match? head %)) sweets)]
    (if (and (< (rand-int 100) FREQ-SWEETS) (< (count no-matches) MAX-SWEETS))
      (cons (random-coords) no-matches)
      no-matches)))

(defn create-state [direction positions size sweets]
  {:direction direction
   :positions positions
   :size      size
   :sweets    sweets})


(defn update-state [state]
  (let [{:keys [sweets size direction positions]} state
        first-pos (first positions)]
    (if (bad-head-position? first-pos (queue size positions))
      state
      (create-state
        (next-direction direction (arrow->direction (q/key-code)))
        (cons (compute-next-position first-pos direction) (take size positions))
        (if (any-match? first-pos sweets) (+ size 1) size)
        (generate-sweets sweets first-pos)))))


(defn draw-state [state]
  (let [{:keys [size positions sweets]} state
        first-pos (first positions)
        {:keys [x y]} first-pos]
    (q/frame-rate (min (+ FRAME-RATE size) 40))
    (q/background 240)
    (q/with-translation
      [(/ (q/width) 2)
       (/ (q/height) 2)]
      (q/fill 0 250 0 200)
      (q/rect x y SNAKE-SIZE SNAKE-SIZE)
      (q/fill 0 0 0 200)
      (doseq [elt (queue size positions)]
        (q/rect (:x elt) (:y elt) SNAKE-SIZE SNAKE-SIZE))
      (q/fill 255 0 0 200)
      (doseq [elt sweets]
        (q/rect (:x elt) (:y elt) SNAKE-SIZE SNAKE-SIZE)))))



(defn setup []
  (q/color-mode :rgb)
  (create-state (arrow->direction 0) (list (coords 0 0)) 3 (list)))

(defn ^:export run-sketch []
  (q/defsketch snake
               :host "snake"
               :size [SIZE SIZE]
               :setup setup
               :update update-state
               :draw draw-state
               :middleware [m/fun-mode]))
