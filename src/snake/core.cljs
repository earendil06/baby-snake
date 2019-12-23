(ns snake.core
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(def SIZE 500)
(def SNAKE-SIZE 10)
(def FRAME-RATE 4)
(def KEY-CODES {39 :right 37 :left 38 :up 40 :down})

(defn arrow->direction [code]
  (or (get KEY-CODES code) :right))

(defn incompatible? [direction1 direction2]
  (or (and (= direction1 :left) (= direction2 :right))
      (and (= direction1 :right) (= direction2 :left))
      (and (= direction1 :down) (= direction2 :up))
      (and (= direction1 :up) (= direction2 :down))))

(defn next-direction [current player]
  (if (incompatible? current player) current player))

(defn compute-next-position [current next]
  (let [f (fn [a1 a2]
            (cond (= next a1) SNAKE-SIZE
                  (= next a2) (- SNAKE-SIZE)
                  :else 0))
        {currentX :x currentY :y} current]
    {:x (+ currentX (f :right :left))
     :y (+ currentY (f :down :up))}))

(def any? (complement not-any?))

(defn bad-head-position? [head queue]
  (any? #(and (= (:x head) (:x %)) (= (:y head) (:y %))) queue))

(defn queue [size positions]
  (take (- size 1) (rest positions)))

(defn update-state [state]
  (let [{size :size direction :direction positions :positions dead? :dead?} state
        first-pos (first positions)]
    (if dead?
      state
      {:direction (next-direction direction (arrow->direction (q/key-code)))
       :positions (cons (compute-next-position first-pos direction) (take (+ size 10) positions))
       :size      (if (< (rand-int 100) 10) (+ size 1) size)
       :dead?     (or dead? (bad-head-position? first-pos (queue size positions)))})))

(defn draw-state [state]
  (let [{size :size positions :positions} state
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
        (q/rect (:x elt) (:y elt) SNAKE-SIZE SNAKE-SIZE)))))

(defn setup []
  (q/color-mode :rgb)
  {:direction (arrow->direction 0)
   :positions (list {:x 0 :y 0})
   :size      3
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
