(ns snake.core
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(def SIZE 500)
(def SNAKE-SIZE 10)
(def FRAME-RATE 4)


(defn arrow->direction [code]
  (let [default "right"]
    (cond
      (= code 37) "left"
      (= code 38) "up"
      (= code 39) "right"
      (= code 40) "down"
      :else default)))

(defn incompatible? [direction1 direction2]
  (or
    (and (= direction1 "left") (= direction2 "right"))
    (and (= direction1 "right") (= direction2 "left"))
    (and (= direction1 "down") (= direction2 "up"))
    (and (= direction1 "up") (= direction2 "down"))))

(defn next-direction [current-direction player-direction]
  (if (incompatible? current-direction player-direction)
    current-direction
    player-direction))

(defn compute-next-position [current-position next-direction]
  {:x (+ (:x current-position)
         (cond
           (= next-direction "right") SNAKE-SIZE
           (= next-direction "left") (- SNAKE-SIZE)
           :else 0
           ))
   :y (+ (:y current-position)
         (cond
           (= next-direction "down") SNAKE-SIZE
           (= next-direction "up") (- SNAKE-SIZE)
           :else 0
           ))}
  )

(defn bad-head-position? [head queue]
  (not (not-any? (fn [x]
                   (and (= (:x head) (:x x)) (= (:y head) (:y x)))) queue)))

(defn update-state [state]
  (if (:dead? state)
    state
    {:direction (next-direction
                  (:direction state)
                  (arrow->direction (q/key-code)))
     :positions (cons
                  (compute-next-position
                    {:x (:x (first (:positions state))) :y (:y (first (:positions state)))}
                    (:direction state))
                  (take (+ (:size state) 10) (:positions state)))
     :size      (if (< (rand-int 100) 10) (+ (:size state) 1) (:size state))
     :dead?     (or (:dead? state) (bad-head-position? (first (:positions state)) (rest (:positions state))))
     }))

(defn draw-state [state]
  (q/frame-rate (min (+ FRAME-RATE (:size state)) 40))
  (q/background 240)
  (q/with-translation
    [(/ (q/width) 2)
     (/ (q/height) 2)]
    (q/fill 0 250 0 200)
    (q/rect (:x (first (:positions state))) (:y (first (:positions state))) SNAKE-SIZE SNAKE-SIZE)
    (q/fill 0 0 0 200)
    (doseq [elt (take (- (:size state) 1) (rest (:positions state)))]
      (q/rect (:x elt) (:y elt) SNAKE-SIZE SNAKE-SIZE)
      )))


(defn setup []
  (q/frame-rate FRAME-RATE)
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
