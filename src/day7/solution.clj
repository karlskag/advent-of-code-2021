(ns day7.solution
  (:require [clojure.string :as str])
  )

(def initial-test-data
  (as-> (slurp "src/day7/test-input.txt") $
        (str/split $ #",")
        (map read-string $)))
(def initial-data
  (as-> (slurp "src/day7/input.txt") $
        (str/split $ #",")
        (map read-string $)))

(defn calc-total-distance-a
  [x-val positions]
  (reduce
    #(+ %1 (Math/abs (int (- x-val %2))))
    0 positions))

(defn calc-total-distance-b
  [x-val positions]
  (reduce
    #(+ %1 (apply + (range 0 (inc (Math/abs (int (- x-val %2)))))))
    0 positions))

(defn solve
  [data calc-total-distance]
  (let [possible-x (range (apply min data) (inc (apply max data)))]
    (apply min (reduce (fn [acc val]
                         (conj acc (calc-total-distance val data))) [] possible-x))))

(solve initial-data calc-total-distance-a)
;quite slow but works
(solve initial-data calc-total-distance-b)