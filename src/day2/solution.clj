(ns day2.solution
  (:require [clojure.string :as str])
  (:require [clojure.test :as test]))

(def input
  (map
    #(str/split %1 #" ")
    (clojure.string/split-lines (slurp "src/day2/input.txt"))))

;b solution added aim-value to x and y
(defn solve
  [cmds]
  (reduce *
          (take 2
                (reduce (fn [acc [cmd val]]
                          (let [parsed-v (read-string val)]
                            (case cmd
                              "forward" (-> acc
                                            (update-in [0] #(+ %1 parsed-v))
                                            (update-in [1] #(+ %1 (* (nth acc 2) parsed-v))))
                              "up" (update-in acc [2] #(- %1 parsed-v))
                              "down" (update-in acc [2] #(+ %1 parsed-v))
                              )))
                        [0 0 0]
                        cmds))))

(solve input)