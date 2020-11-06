(ns geo-yoots.test-util
  (:require [clojure.test :refer [is]]))



(def distance-threshold 0.005) ; %
(def km->nautical-mile 0.539957)

(defn compare-distance
  [expected actual & {:keys [threshold factor]
                      :or {threshold distance-threshold
                           factor km->nautical-mile}}]
  (let [percent-diff (/ (Math/abs (- expected (* factor actual))) expected)]
    (println (format "ACTUAL[%s]=%s, THRESHOLD[%s]=%s" (* actual factor) percent-diff expected distance-threshold))
    (is (<= percent-diff threshold))))

(defn compare-boolean
  [expected actual]
  (loop [xs (seq (map vector expected actual))]
    (if-let [x (first xs)]
      (let [[a b] x]
        (is (= a b))
        (recur (rest xs))))))

(defn round-float
  [x scale]
  (.floatValue (.setScale (bigdec x) scale BigDecimal/ROUND_HALF_UP)))
