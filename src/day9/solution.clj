(ns day9.solution
  (:require [clojure.string :as str])
  (:require [clojure.test :as test]))

(def data
  (as-> (slurp "src/day9/input.txt") $
        (str/split-lines $)
        (map #(str/split % #"") $)
        ))

(defn get-adjacent-nodes
  {:test (fn []
           (test/is (= (get-adjacent-nodes 1 1 [2 0 3] '([1 1 1] [2 0 3] [1 1 1])) [1 2 3 1]))
           (test/is (= (get-adjacent-nodes 2 0 [1 1 1] '([1 1 1] [2 0 3] [1 1 1])) [1 3])))}
  [val-idx row-idx current-row all-rows]
  (let [prev-row (if (> row-idx 0) (nth all-rows (dec row-idx)) nil)
        next-row (if (< row-idx (dec (count all-rows))) (nth all-rows (inc row-idx)) nil)]
    (remove nil? [(if (not (nil? prev-row)) {:coord [val-idx (dec row-idx)] :value (nth prev-row val-idx)} nil)
                  (if (> val-idx 0) {:coord [(dec val-idx) row-idx] :value (nth current-row (dec val-idx))} nil)
                  (if (< val-idx (dec (count current-row))) {:coord [(inc val-idx) row-idx] :value (nth current-row (inc val-idx))} nil)
                  (if (not (nil? next-row)) {:coord [val-idx (inc row-idx)] :value (nth next-row val-idx)} nil)])))

(defn create-adjacency-map
  [rows]
  (map-indexed
    (fn [row-idx row]
      (map-indexed
        (fn [val-idx val]
          {:coord [val-idx row-idx]
           :node  {:value    val
                   :adjacent (get-adjacent-nodes val-idx row-idx row rows)}
           }) row)
      ) rows)
  )

(defn find-deepest-nodes
  [adjacency-map]
  (flatten
    (remove empty?
                   (map (fn [row-data]
                          (filter
                            (fn [map-entry]
                              (not-any? #(>= (read-string (:value (:node map-entry))) (read-string (:value %))) (:adjacent (:node map-entry)))
                              ) row-data)
                          ) adjacency-map)))
  )

(defn create-coordinate-map
  [adjacency-map]
  (reduce (fn [acc entry]
            (assoc acc (keyword (str (:coord entry))) entry))
          {} (flatten adjacency-map))
  )

(defn get-basin
  [basin-set all-nodes current-node]
  (let [node-key (keyword (str (:coord current-node)))]
    (if (nil? (node-key basin-set))
      (reduce (fn [collected-nodes adjacent-node]
                (if (< (read-string (:value adjacent-node)) 9)
                  (clojure.set/union collected-nodes (get-basin collected-nodes all-nodes ((keyword (str (:coord adjacent-node))) all-nodes)))
                  collected-nodes)
                )
              (conj basin-set node-key)
              (:adjacent (:node current-node)))
      basin-set)
    ))

(defn solve-a
  [rows]
  (->> (create-adjacency-map rows)
       (find-deepest-nodes)
       (map :node)
       (map :value)
       (reduce (fn [acc val] (+ acc (inc (read-string val)))) 0)
       ))

(defn solve-b
  [data]
  (let [adjacency-map (create-adjacency-map data)
        nodes (create-coordinate-map adjacency-map)
        deepest-nodes (find-deepest-nodes adjacency-map)]
      (->> deepest-nodes
           (map #(get-basin #{} nodes %))
           (map count)
           (sort)
           (take-last 3)
           (apply *))
    ))

; -- B find basins --

; For each deep-point
; Add each adjacent node (that isn't value 9) coordinate to set recursively
; count(set) is nodes in basin

(solve-a data)
(solve-b data)
