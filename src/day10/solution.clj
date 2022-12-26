(ns day10.solution
  (:require [clojure.string :as str])
  (:require [clojure.test :as test]))

(def data
  (as-> (slurp "src/day10/input.txt") $
        (str/split-lines $)
        ))

(defn get-wanted-opening-tag
  [char]
  (case char
    \) \(
    \] \[
    \} \{
    \> \<
    ))

(defn get-value-a
  [char]
  (case char
    \) 3
    \] 57
    \} 1197
    \> 25137
    ))

(defn get-value-b
  [char]
  (case char
    \( 1
    \[ 2
    \{ 3
    \< 4
    ))

(defn find-first-error
  [line]
  (loop [index 1
         opening-tags [(first line)]]
    (if (< index (count line))
      (let [current-char (nth line index)]
        (if (some #{current-char} [\( \{ \[ \<])
          (recur (inc index) (concat opening-tags [current-char]))
          (let [maybe-match (last opening-tags)]
            (if (= maybe-match (get-wanted-opening-tag current-char))
              (recur (inc index) (drop-last opening-tags))
              current-char))
          ))
      opening-tags
      )))

(defn solve-a
  [lines]
  (->> (map find-first-error lines)
       ; because b :P
       (remove seq?)
       (map get-value-a)
       (apply +)))

(defn solve-b
  [lines]
  (as-> (map find-first-error lines) $
        (filter seq? $)
        (map #(map get-value-b (reverse %)) $)
        (map #(reduce (fn [acc val] (+ (* 5 acc) val)) 0 %) $)
        (sort $)
        (nth $ (/ (count $) 2))
        ))

(solve-a data)
(solve-b data)