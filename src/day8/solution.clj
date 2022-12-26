(ns day8.solution
  (:require [clojure.string :as str]))

(def data
  (as-> (slurp "src/day8/input.txt") $
        (str/split-lines $)
        (map #(str/split % #" ") $)
        ;(map #(nthrest % (inc (.indexOf % "| "))) $)
        ;(map #(str/split (str %) #" ") $)
        ;(map last $)
        ))

(defn count-nr-1478
  [digits]
  (reduce (fn [acc val]
            (let [len (count val)]
              (if (or (= len 2) (= len 3) (= len 4) (= len 7))
                (inc acc)
                acc)))
          0 digits))

(defn solve
  [input]
  (->> (map #(take-last 4 %) input)
       (reduce (fn [acc val] (+ acc (count-nr-1478 val))) 0))
  )


(defn contains-all?
  [val check]
  (let [split-val (set (str/split val #""))
        split-check (str/split check #"")]
    (not-any? #(not (contains? split-val %)) split-check)
    ))

(defn look-up
  [dict signal]
  (key (first (filter (fn [[_ val]]
                        (and
                          (= (count val) (count (str/split signal #"")))
                          (contains-all? val signal)
                          )) dict))))

(defn decode-signals
  [dictionary signals]
  (map #(look-up dictionary %) signals)
  )

(defn get-digits
  [wires]
  (let [dict {:0 nil
              :1 (first (filter #(= (count %) 2) wires))
              :2 nil
              :3 nil
              :4 (first (filter #(= (count %) 4) wires))
              :5 nil
              :6 nil
              :7 (first (filter #(= (count %) 3) wires))
              :8 (first (filter #(= (count %) 7) wires))
              :9 nil}]
    (as-> dict $
          (assoc $ :3 (first (filter #(and
                                        (= (count %) 5)
                                        (contains-all? % (:7 dict)))
                                     wires)))
          (assoc $ :0 (first (filter #(and
                                        (= (count %) 6)
                                        (contains-all? % (:1 dict))
                                        (not (contains-all? % (:3 $))))
                                     wires)))
          (assoc $ :9 (first (filter #(and
                                        (= (count %) 6)
                                        (contains-all? % (:3 $)))
                                     wires)))
          (assoc $ :6 (first (filter #(and
                                        (= (count %) 6)
                                        (not (= % (:0 $)))
                                        (not (= % (:9 $))))
                                     wires)))
          (assoc $ :5 (first (filter #(and
                                        (= (count %) 5)
                                        (contains-all? (:6 $) %))
                                     wires)))
          (assoc $ :2 (first (filter #(and
                                        (= (count %) 5)
                                        (not (= % (:5 $)))
                                        (not (= % (:3 $))))
                                     wires))))
    ))

(defn decode
  [row]
  (let [wires (take 10 row)
        signals (take-last 4 row)
        digits (get-digits wires)]
    (decode-signals digits signals)
    ))

(defn solve-b
  [input]
  (->> (map decode input)
       (map (fn [row]
              (str/join (map (fn [key] (name key)) row))))
       (reduce (fn [acc num]
                 (+ acc (Integer/parseInt num))) 0)
       ))

(solve data)
(solve-b data)