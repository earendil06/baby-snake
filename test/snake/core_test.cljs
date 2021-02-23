(ns snake.core-test
  (:require [clojure.test :refer [is deftest]]
            [snake.core :as snake]))

(deftest arrow->direction-test
  (let [rand-key #(let [r (rand-int 100)]
                    (if (some #{r} '(37 38 39 40)) (recur) r))]
    "Test arrow code to direction conversion"
    (is (= :right (snake/arrow->direction 39)))
    (is (= :left (snake/arrow->direction 37)))
    (is (= :up (snake/arrow->direction 38)))
    (is (= :down (snake/arrow->direction 40)))
    (is (= :right (snake/arrow->direction (rand-key))))))


(deftest arrow->direction-test
  "Test incompatibility between direction and action"
  (is (true? (snake/incompatible? :right :left)))
  (is (false? (snake/incompatible? :right :right)))
  (is (false? (snake/incompatible? :right :up)))
  (is (false? (snake/incompatible? :right :down)))
  (is (false? (snake/incompatible? :left :left)))
  (is (true? (snake/incompatible? :left :right)))
  (is (false? (snake/incompatible? :left :up)))
  (is (false? (snake/incompatible? :left :down)))
  (is (false? (snake/incompatible? :up :left)))
  (is (false? (snake/incompatible? :up :right)))
  (is (false? (snake/incompatible? :up :up)))
  (is (true? (snake/incompatible? :up :down)))
  (is (false? (snake/incompatible? :down :left)))
  (is (false? (snake/incompatible? :down :right)))
  (is (true? (snake/incompatible? :down :up)))
  (is (false? (snake/incompatible? :down :down))))

(deftest next-direction
  "Test next direction with player action"
  (is (= :right (snake/next-direction :right :right)))
  (is (= :up (snake/next-direction :right :up)))
  (is (= :down (snake/next-direction :right :down)))
  (is (= :right (snake/next-direction :right :left))))

