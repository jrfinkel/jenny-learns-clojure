(ns lda.core)

(:use '[clojure.contrib.probabilities.finite-distributions :only (normalize)])
(:use '[clojure.contrib.probabilities.monte-carlo :only (sample sample-reduce)])
(:use '[clojure.string :only (join)])
(use 'lda.utils)

;;(load-file "utils.clj")

;; -= NOTATION =-
;;
;; c --> corpus
;; d --> document
;; x --> token
;; w --> word
;; t --> topic
;; c-s --> corpus statistics
;; s-s --> sufficient statistics
;;

;; -= STRUCTS =-

(defstruct token :word :topic)
(defstruct document :tokens :topic-counts)
(defstruct corpus-statistics :word-counts-by-topic :vocab-size)
(defstruct corpus :documents :corpus-stats)
(defstruct sufficient-statistics :corpus-stats :topic-counts)

;; -= CONVENIENCE METHODS FOR STRUCTS =-

(defn topics [{c-s :corpus-stats}] (keys (:word-counts-by-topic c-s)))
(defn num-topics [x] (count (topics x)))

;; -= CONSTANTS =-

(def alpha 0.001)
(def beta 1)

;; -= COUNTING THINGS =-

(defn get-word-counts-by-topic
  [ds ts]
  (let [xs (reduce into (map :tokens ds))]
     (reduce #(update-count %1 (:topic %2) (:word %2) inc) (zipmap ts (repeat {})) xs)))

(defn update-stats
  [w t s-s f]
  (let [c-s (:corpus-stats s-s)
        w-counts-by-t (update-count (:word-counts-by-topic c-s) t w f)
        c-s (assoc c-s :word-counts-by-topic w-counts-by-t)
        t-counts (update-count (:topic-counts s-s) t f)] 
        (struct sufficient-statistics c-s t-counts)))

;; -= COMPUTING PROBABILITIES =-

(defn prob-word-given-topic
  [w t c-s]
  (let [word-counts-by-topic (:word-counts-by-topic c-s)
        vocab-size (:vocab-size c-s)]
    (/ (+ alpha
          (get (get word-counts-by-topic t) w 0))
       (+ (* alpha vocab-size)
          (reduce + (vals (get word-counts-by-topic t)))))))

(defn prob-topic-given-document
  [t s-s]
  (let [num-ts (num-topics s-s)
        t-counts (:topic-counts s-s)]        
    (/ (+ beta
          (get t-counts t 0))
       (+ (* beta num-ts)
          (reduce + (vals t-counts))))))

(defn get-prob
  [w t s-s]
  (let [prob-word-given-topic (prob-word-given-topic w t (:corpus-stats s-s))
        prob-topic-given-document (prob-topic-given-document t s-s)]
    (* prob-word-given-topic prob-topic-given-document)))

(defn get-distribution
  [w s-s]
  (let [ts (topics s-s)]
    (zipmap ts (map #(get-prob w %1 s-s) ts))))

;; -= SAMPLING THINGS =-

(defn draw-sample
  ([dist] (draw-sample dist (rand (reduce + (vals dist)))))     
  ([dist mass]
     (let [prob-of-first (val (first dist))]
       (if (>= prob-of-first mass)
         (key (first dist))
         (draw-sample (rest dist) (- mass prob-of-first))))))

(defn draw-samples
  "returns a lazy seq of items drawn uniformly at random (with replacement) from s"
  [s]
  (let [num (count s)]
    (repeatedly #(nth s (rand-int num)))))


(defn resample-token
  "resamples topic for token t, using sufficient statistics ss, and then returns updated versions of t and ss"
  [x s-s]
  (let [w (:word x)
        t (:topic x)
        s-s (update-stats w t s-s dec)
        dist (get-distribution w s-s)
        t (draw-sample dist)
        s-s (update-stats w t s-s inc)]
    [(struct token w t) s-s]))

(defn resample-document
  "resamples topics for tokens in document d, using sufficient statistics ss, and then returns updated versions of d and ss. uses :topic-counts from d instead of from ss."
  [d c-s]
  (let [xs (:tokens d)
        s-s (struct sufficient-statistics c-s (:topic-counts d))
        [xs s-s] (map-and-reduce resample-token xs s-s)
        c-s (:corpus-stats s-s)
        topic-counts (:topic-counts s-s)]
    [(struct document xs topic-counts) c-s]))
    
(defn resample-corpus
  [c]
  (let [c-s (:corpus-stats c)
        [ds c-s] (map-and-reduce resample-document (:documents c) c-s)]
    (struct corpus ds c-s)))
    
;; -= READING / CREATING DATA =-

(defn get-dictionary [] (load-and-split "words.txt"))

(defn get-topics
  [num-ts]
  (map #(keyword (clojure.string/join ["topic-" (str %1)])) (range num-ts)))

(defn rand-words
  "returns list of random words (with replacement) from dictionary"
  []
  (draw-samples (get-dictionary)))

(defn rand-topics
  "returns list of random topics (with replacement) from ts"
  [ts]
  (draw-samples ts))

(defn rand-tokens
  "returns list of randomly generated tokens, with words from dictionary and topics from ts"
  [ts]
  (map #(struct token %1 %2)
       (rand-words)
       (rand-topics ts)))
  
(defn rand-document
  "returns randomly generated document (list of tokens with topic counts)"
  [num-xs ts]
  (let [xs (take num-xs (rand-tokens ts))
        topic-counts (get-counts (map :topic xs))]    
    (struct document xs topic-counts)))

(defn rand-documents
  [num-ds num-xs-per-d ts]
  (take num-ds (repeatedly #(rand-document num-xs-per-d ts))))
        
(defn rand-corpus
  "returns randomly generated corpus (list of documents with topic/word counts)"
  [num-ds num-xs-per-d num-ts]
  (let [ts (get-topics num-ts)
        ds (rand-documents num-ds num-xs-per-d ts)
        word-counts-by-topic (get-word-counts-by-topic ds ts)
        vocab-size (count (set (map :word (reduce into (map :tokens ds)))))
        corpus-stats (struct corpus-statistics word-counts-by-topic vocab-size)]
    (struct corpus ds corpus-stats)))

;; -= MISC =-

;; (defn lf [] (load-file "lda.clj"))
;; (print "file reloaded")
