(ns day1.solution
  (:require [clojure.test :as test]))

(def input
  (map read-string (clojure.string/split-lines (slurp "src/day1/input.txt"))))

(defn count-three
  [numbers]
  (map (fn [val]
         (reduce + val))
       (partition 3 1 numbers)))

(test/is (= (count-three '(1 2 3 4 5)) '(6 9 12)))

(defn solve
  [numbers]
  (:end (reduce (fn [ret val]
                  (if (> val (:prev ret))
                    {:prev val :end (inc (:end ret))}
                    (assoc ret :prev val)))
                {:prev (inc (first numbers)) :end 0}
                numbers)))

(test/is (= (solve '(199 200 208 210 200 207 240 269 260 263)) 7))
(solve input)
(solve (count-three input))