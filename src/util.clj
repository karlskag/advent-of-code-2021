(ns util)

(defn contains-seq
  [seq test]
  (not (empty? (filter #(= % test) (vec seq)))))

(defn update-vals [map vals f]
  (reduce #(update-in % [%2] f) map vals))