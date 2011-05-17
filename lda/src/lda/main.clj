(ns lda.main)

(use 'lda.core)

(defn do-it
  []
  (do 
;; args: num documents, num words per document, num topics
    (def orig-corpus (rand-corpus 5 5 2))
;; note that this only resamples everything once.
    (def resampled-corpus (resample-corpus orig-corpus))))