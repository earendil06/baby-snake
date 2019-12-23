(ns snake.core
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(def SIZE 500)
(def SNAKE-SIZE 10)
(def FRAME-RATE 4)
(def KEY-CODES {39 :right 37 :left 38 :up 40 :down})

(defn arrow->direction [code]
  (let [default :right]
    (or (get KEY-CODES code) default)))

(defn incompatible? [direction1 direction2]
  (or
    (and (= direction1 :left) (= direction2 :right))
    (and (= direction1 :right) (= direction2 :left))
    (and (= direction1 :down) (= direction2 :up))
    (and (= direction1 :up) (= direction2 :down))))

(defn next-direction [current player]
  (if (incompatible? current player) current player))

(defn compute-next-position [current next]
  (let [{currentX :x currentY :y} current]
    {:x (+ currentX (cond
                      (= next :right) SNAKE-SIZE
                      (= next :left) (- SNAKE-SIZE)
                      :else 0
                      ))
     :y (+ currentY (cond
                      (= next :down) SNAKE-SIZE
                      (= next :up) (- SNAKE-SIZE)
                      :else 0
                      ))})
  )


(def any? (complement not-any?))

(defn bad-head-position? [head queue]
  (any? #(and (= (:x head) (:x %)) (= (:y head) (:y %))) queue))

(defn queue [size positions]
  (take (- size 1) (rest positions)))

(defn update-state [state]
  (if (:dead? state)
    state
    {:direction (next-direction
                  (:direction state)
                  (arrow->direction (q/key-code)))

     :positions (let [first-pos (first (:positions state))]
                  (cons
                    (compute-next-position
                      {:x (:x first-pos) :y (:y first-pos)}
                      (:direction state))
                    (take (+ (:size state) 10) (:positions state))))

     :size      (let [size (:size state)]
                  (if (< (rand-int 100) 10) (+ size 1) size))


     :dead?     (or
                  (:dead? state)
                  (bad-head-position? (first (:positions state)) (queue (:size state) (:positions state))))}))



(defn draw-state [state]
  (q/frame-rate (min (+ FRAME-RATE (:size state)) 40))
  (q/background 240)
  (q/with-translation
    [(/ (q/width) 2)
     (/ (q/height) 2)]
    (q/fill 0 250 0 200)
    (q/rect (:x (first (:positions state))) (:y (first (:positions state))) SNAKE-SIZE SNAKE-SIZE)
    (q/fill 0 0 0 200)
    (doseq [elt (queue (:size state) (:positions state))]
      (q/rect (:x elt) (:y elt) SNAKE-SIZE SNAKE-SIZE))))


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

; uncomment this line to reset the sketch:
; (run-sketch)
