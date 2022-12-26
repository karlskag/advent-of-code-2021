(ns day3.solution
  (:require [clojure.test :as test]))

(def input
  (clojure.string/split-lines (slurp "src/day3/input.txt")))
(def test-input
  (clojure.string/split-lines (slurp "src/day3/test-input.txt")))

(defn get-nr
  [bins comp]
  (->> bins
       (apply map str)
       (map frequencies)
       (map (fn [map]
              (let [[v1 f1] (first map)
                    [v2 f2] (second map)]
                (if (comp f1 f2) v1 v2))))
       (apply str)
       ))

(defn find-value
  [comp equal bins]
  (loop
    [values bins
     i 0]
    (if (= (count values) 1)
      (first values)
      (let [freq (->> values
                      (apply map str)
                      (map frequencies))
            map (nth freq i)]
        (let [[v1 f1] (first map)
              [v2 f2] (if (nil? (second map))
                        ["-1" -1]
                        (second map))]
          (if (= f1 f2)
            (recur (filter #(= (str (nth % i)) (str equal)) values) (inc i))
            (if (comp f1 f2)
              (recur (filter #(= (nth % i) v1) values) (inc i))
              (recur (filter #(= (nth % i) v2) values) (inc i))))
          )
        )))
  )

(defn get-nr-2
  [bins comp equal]
  (->> bins
       (find-value comp equal)
       (apply str)
       ))

(defn solve-a
  [bins]
  (* (Integer/parseInt (get-nr bins >) 2) (Integer/parseInt (get-nr bins <) 2)))

(defn solve-b
  [bins]
  (* (Integer/parseInt (get-nr-2 bins > "1") 2) (Integer/parseInt (get-nr-2 bins < "0") 2)))

;(solve-a input)
;(solve-b input)