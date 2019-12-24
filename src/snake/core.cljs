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

(s/fdef arrow->direction
        :args (s/cat :code number?)
        :ret symbol?)

(defn incompatible? [direction1 direction2]
  (or (and (= direction1 :left) (= direction2 :right))
      (and (= direction1 :right) (= direction2 :left))
      (and (= direction1 :down) (= direction2 :up))
      (and (= direction1 :up) (= direction2 :down))))

(s/fdef incompatible?
        :args (s/cat :direction1 symbol? :direction2 symbol?)
        :ret boolean?)

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

(defn update-state [state]
  (let [{:keys [sweets size direction positions dead?]} state
        first-pos (first positions)]
    (if dead?
      state
      {:direction (next-direction direction (arrow->direction (q/key-code)))
       :positions (cons (compute-next-position first-pos direction) (take (+ size MAX-SWEETS) positions))
       :size      (if (any-match? first-pos sweets) (+ size 1) size)
       :sweets    (generate-sweets sweets first-pos)
       :dead?     (or dead? (bad-head-position? first-pos (queue size positions)))})))

(defn draw-state [state]
  (let [{:keys [size positions sweets]} state
        first-pos (first positions)]
    (q/frame-rate (min (+ FRAME-RATE size) 40))
    (q/background 240)
    (q/with-translation
      [(/ (q/width) 2)
       (/ (q/height) 2)]
      (q/fill 0 250 0 200)
      (q/rect (:x first-pos) (:y first-pos) SNAKE-SIZE SNAKE-SIZE)
      (q/fill 0 0 0 200)
      (doseq [elt (queue size positions)]
        (q/rect (:x elt) (:y elt) SNAKE-SIZE SNAKE-SIZE))
      (q/fill 255 0 0 200)
      (doseq [elt sweets]
        (q/rect (:x elt) (:y elt) SNAKE-SIZE SNAKE-SIZE)))))

(defn setup []
  (q/color-mode :rgb)
  {:direction (arrow->direction 0)
   :positions (list (coords 0 0))
   :size      3
   :sweets    (list)
   :dead?     false
   })

(defn ^:export run-sketch []
  (q/defsketch snake
               :host "snake"
               :size [SIZE SIZE]
               :setup setup
               :update update-state
               :draw draw-state
               :middleware [m/fun-mode]))
