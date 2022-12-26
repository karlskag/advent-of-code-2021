(ns day5.solution
  (:require [clojure.string :as str])
  )

(def input
  (->> (clojure.string/split-lines (slurp "src/day5/input.txt"))
       (map #(str/split %1 #" -> "))
       (map (fn [[xy1 xy2]]
              [(str/split xy1 #",") (str/split xy2 #",")]))
       ))

(defn get-straight-coordinate-ranges
  [[[x1s y1s] [x2s y2s]]]
  (let [x1 (read-string x1s) y1 (read-string y1s)
        x2 (read-string x2s) y2 (read-string y2s)]
    (if (= x1 x2)
      (map (fn [new-y]
             (str x1 " " new-y)) (if (< y1 y2)
                                   (range y1 (inc y2))
                                   (range y2 (inc y1))))
      (map (fn [new-x]
             (str new-x " " y1)) (if (< x1 x2)
                                   (range x1 (inc x2))
                                   (range x2 (inc x1)))))))

(defn get-diagonal-coordinate-ranges
  [[[x1s y1s] [x2s y2s]]]
  (let [x1 (read-string x1s) y1 (read-string y1s)
        x2 (read-string x2s) y2 (read-string y2s)]
    ; going right
    (if (< x1 x2)
      (if (< y1 y2)
        ; going up
        (map (fn [i] (str (+ x1 i) " " (+ y1 i))) (range 0 (- (inc x2) x1)))
        ; going down
        (map (fn [i] (str (+ x1 i) " " (- y1 i))) (range 0 (- (inc x2) x1))))
      ; going left
      (if (< y1 y2)
        ; going up
        (map (fn [i] (str (- x1 i) " " (+ y1 i))) (range 0 (- (inc x1) x2)))
        ; going down
        (map (fn [i] (str (- x1 i) " " (- y1 i))) (range 0 (- (inc x1) x2))))
      )))

(defn calc-overlaps
  [coords]
  (reduce (fn [acc [xy freq]]
            (if (>= freq 2)
              (inc acc)
              acc)
            ) 0 coords))

(defn solve-a
  [input]
  (let [straight-lines (filter (fn [[[x1 y1] [x2 y2]]]
                                 (or (= x1 x2) (= y1 y2))) input)
        diagonal-lines (filter (fn [[[x1 y1] [x2 y2]]]
                                 (and (not (= x1 x2)) (not (= y1 y2)))) input)]
    (->> (concat
           (map get-straight-coordinate-ranges straight-lines)
           (map get-diagonal-coordinate-ranges diagonal-lines))
         flatten
         frequencies
         calc-overlaps)
    ))

(solve-a input)