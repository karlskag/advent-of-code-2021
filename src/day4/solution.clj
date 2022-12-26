(ns day4.solution
  (:require [clojure.string :as str])
  (:require [util :as util])
  (:require [clojure.data :as dat])
  (:require [clojure.test :as test]))

(def draws
  (->
    (first (clojure.string/split-lines (slurp "src/day4/input.txt")))
    (str/split #",")))

(def boards
  (->> (clojure.string/split-lines (slurp "src/day4/input.txt"))
       (drop 2)
       (partition 5 6)))

(defn parse-rows
  [rows]
  (map (fn [row]
         (filter #(not (clojure.string/blank? %)) row))
       rows))

(defn parse-board
  [board]
  (let [rows (map #(str/split % #" ") board)]
    (parse-rows rows)))

(defn rotate-board
  [parsed-board]
  (apply map list parsed-board))

(defn is-bingo?
  [rows draws]
  ;is any row subset to draws
  (not (empty? (first (filter #(clojure.set/subset? (set %) (set draws)) rows)))))

(defn maybe-get-bingo-board
  [boards draws]
  (loop [i 0]
    (if (= i (count boards))
      nil
      (let [board (parse-board (nth boards i))
            rotated-board (rotate-board board)
            rows (concat board rotated-board)]
        (if (is-bingo? rows draws)
          [board
           (reduce (fn [sum row]
                     (+ sum (->> (filter #(nil? (some #{%} draws)) row)
                                 (map read-string)
                                 (apply +)))) 0 board)]
          (recur (inc i))
          )))))

(defn solve
  [boards draws]
  (loop [i 0]
    (if (= i (count draws))
      "not found"
      (let [n-draws (take (inc i) draws)
            [bingo-board unmarked] (maybe-get-bingo-board boards n-draws)]
        (if (nil? bingo-board)
          (recur (inc i))
          (* unmarked (read-string (last n-draws)))
          ))
      )))

(defn get-all-bingo-boards
  [boards draws]
  (loop [i 0
         bingos '()]
    (if (= i (count boards))
      bingos
      (let [board (parse-board (nth boards i))
            rotated-board (rotate-board board)
            rows (concat board rotated-board)]
        (if (is-bingo? rows draws)
          (recur (inc i) (concat bingos (list board)))
          (recur (inc i) bingos)
          )))))

(defn get-missing-board
  [b1 b2]
  (first (filter #(not (util/contains-seq b2 %)) b1)))

(defn solve-b
  [boards draws]
  (let [bingo-boards (get-all-bingo-boards boards draws)]
    (loop [i 1]
      (let [n-draws (drop-last i draws)
            rem-bingos (get-all-bingo-boards boards n-draws)]
        (if (= (count bingo-boards) (count rem-bingos))
          (recur (inc i))
          (let [board (get-missing-board bingo-boards rem-bingos)
                last-removed-nr (read-string (last (drop-last (dec i) draws)))]
            (* last-removed-nr
               (reduce (fn [sum row]
                         (+ sum (->> (filter #(nil? (some #{%} n-draws)) row)
                                     (map read-string)
                                     (apply +)))) (- last-removed-nr) board)
               )
            ))
        ))
    ))

(solve boards draws)
(solve-b boards draws)

