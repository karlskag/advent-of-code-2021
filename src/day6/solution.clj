(ns day6.solution
  (:require [clojure.string :as str])
  (:require [clojure.test :as test])
  (:require [util :as util])
  )

(def initial-test-data
  (as-> (slurp "src/day6/test-input.txt") $
        (str/split $ #",")
        (map read-string $)))
(def initial-data
  (as-> (slurp "src/day6/input.txt") $
        (str/split $ #",")
        (map read-string $)))

(defn simulate-day
  [fish-data]
  (reduce (fn [acc num]
            (if (= num 0)
              (conj acc 6 8)
              (conj acc (dec num))))
          [] fish-data))

;old slow
(defn solve-slow
  [lanternfish-data simulate-days]
  (loop [i 0
         curr-lanternfish-data lanternfish-data]
    (if (= i simulate-days)
      (count curr-lanternfish-data)
      (recur (inc i) (simulate-day curr-lanternfish-data)))))

(defn get-nr-of-fish
  [fish-data]
  (reduce (fn [acc [_ v]] (+ acc v)) 0 fish-data))

(defn simulate-day-fast
  {:test (fn []
           (test/is (= (simulate-day-fast {:0 5 :1 4 :2 3 :3 2 :4 1 :5 0 :6 0 :7 0 :8 0})
                       {:0 4 :1 3 :2 2 :3 1 :4 0 :5 0 :6 5 :7 0 :8 5})))}
  [fish-data]
  (let [data-cpy (into {} fish-data)]
    (reduce (fn [acc [k v]]
              (if (= k :0)
                (-> (util/update-vals acc [:6 :8] #(+ % v))
                    (assoc :0 0))
                (-> (update acc (keyword (str (dec (read-string (name k))))) #(+ % v))
                    (update k #(- % v)))))
            data-cpy
            fish-data))
  )

(defn create-initial-state
  [data]
  (reduce (fn [acc num]
            (update acc (keyword (str num)) inc))
          {:0 0 :1 0 :2 0 :3 0 :4 0 :5 0 :6 0 :7 0 :8 0}
          data))

;new phast
(defn solve-fast
  [lanternfish-data simulate-days]
  (loop [i 0
         curr-lanternfish-data (create-initial-state lanternfish-data)]
    (if (= i simulate-days)
      (get-nr-of-fish curr-lanternfish-data)
      (recur (inc i) (simulate-day-fast curr-lanternfish-data)))))

;(test/is (= (solve-slow initial-test-data 100) 33893))
(test/is (= (solve-fast initial-test-data 256) 26984457539))
;(solve-fast initial-data 256)
