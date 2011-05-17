(ns lda.utils)

(use '[clojure.string :only (split-lines)])

(defn load-and-split
  "returns vector of the lines in the file named x"
  [x]
  (vec (split-lines (slurp x))))

(defn update-with-removal
  "like (assoc m k v) except that 0/nil valued items removed (empty colls not removed)."
  [m k v]
  (if (or (= v 0) (= v nil))
    (dissoc m k)
    (assoc m k v)))

(defn update-count
  "updates the count of item in map using function f. 0/nil valued items removed (but empty colls not removed)."
  ([m k f]
     (update-with-removal m k ((fnil f 0) (get m k))))
  ([m k1 k2 f]
     (let [new-val (update-count (get m k1) k2 f)]
       (update-with-removal m k1 new-val))))

(defn get-counts
  "returns map of each x to count of x in xs"
  [xs]
  (reduce #(update-count %1 %2 inc) {} xs))

(defn map-and-reduce
  "f should be a function with 2 inputs and 2 outputs. The output is a new coll and new x. For each entry in coll, f is called on that entry and x. The first output value goes into the output coll, and the second value becomes the new x."
  ([f coll x] (map-and-reduce f coll x []))
  ([f in-coll x out-coll]
     (let [[new-item x] (f (first in-coll) x)
           out-coll (conj out-coll new-item)]
              (if (= 1 (count in-coll)) [out-coll x] (recur f (rest in-coll) x out-coll)))))
