(ns snake.runner
  (:require [doo.runner :refer-macros [doo-tests]]
            [snake.core-test]))

(doo-tests 'snake.core-test)
