(ns day11.solution
  (:require [clojure.string :as str])
  (:require [clojure.test :as test]))

(def data
  (as-> (slurp "src/day11/input.txt") $
        (str/split-lines $)
        (map #(str/split % #"") $)
        ))

(defn create-coordinate-map
  [rows]
  (apply conj
         (flatten
           (let [map {:meta {:total-explosions 0 :step 0 :step-explosion 0}}]
             (map-indexed
               (fn [row-idx row]
                 (map-indexed
                   (fn [val-idx val]
                     (assoc map (keyword (str val-idx row-idx)) val))
                   row))
               rows))))
  )

(defn get-adjacent
  [coord all-nodes]
  (let [x (read-string (str (first (name coord))))
        y (read-string (str (second (name coord))))]
    (filter #(contains? all-nodes %)
            [(keyword (str (dec x) (dec y)))
             (keyword (str x (dec y)))
             (keyword (str (inc x) (dec y)))
             (keyword (str (dec x) y))
             (keyword (str (inc x) y))
             (keyword (str (dec x) (inc y)))
             (keyword (str x (inc y)))
             (keyword (str (inc x) (inc y)))])
    ))

(defn increase-neighbours
  [coords exploding-nodes]
  (reduce (fn [acc val]
            (let [adjacent (get-adjacent val coords)]
              ;for adjacent node, increase value by one in coordinate-map
              (reduce (fn [acc key]
                        (update acc key inc)) acc adjacent)
              )) coords exploding-nodes)
  )

(defn reset-exploded
  [coords]
  (reduce (fn [acc [key val]]
            (if (and (not (= key :meta)) (>= val 10))
              (assoc acc key 0)
              (assoc acc key val))
            ) {} coords))

(defn chain-explosions
  [coords]
  (loop [new-coords coords
         exploded-nodes []]
    (let [overloaded-nodes (map (fn [[k _]] k) (filter (fn [[k v]] (and (not (= k :meta)) (>= v 10))) new-coords))
          exploding-nodes (filter #(not (some #{%} exploded-nodes)) overloaded-nodes)]
      (if (= (count exploding-nodes) 0)
        (-> (update-in new-coords [:meta :total-explosions] + (count exploded-nodes))
            (assoc-in [:meta :step-explosion] (count overloaded-nodes)))
        (recur (increase-neighbours new-coords exploding-nodes) (concat exploded-nodes exploding-nodes))))
    )
  )

(defn perform-step
  [coordinate-to-value]
  (->> (reduce (fn [acc [k v]]
                 (if (not (= k :meta))
                   (assoc acc k (inc v))
                   (assoc acc k v))
                 ) {} coordinate-to-value)
       (chain-explosions)
       (reset-exploded)))

(defn step-n
  [n-steps coordinate-map]
  (loop [step 1
         updated-map coordinate-map]
    (if (> step n-steps)
      updated-map
      (recur (inc step) (perform-step updated-map))))
  )

(defn step-until-n-explode
  [n-explosion coordinate-map]
  (loop [step 1
         updated-map coordinate-map]
    (if (or (= n-explosion (:step-explosion (:meta updated-map))) (= step 1000))
      updated-map
      (recur (inc step) (assoc-in (perform-step updated-map) [:meta :step] step))))
  )

(defn solve-a
  [dumbo-rows]
  (->> (map #(map read-string %) dumbo-rows)
       (create-coordinate-map)
       (step-n 100)
       (:meta)
       (:total-explosions)
       ))

(defn solve-b
  [dumbo-rows]
  (->> (map #(map read-string %) dumbo-rows)
       (create-coordinate-map)
       (step-until-n-explode 100)
       (:meta)
       (:step)
       ))

(solve-a data)
(solve-b data)