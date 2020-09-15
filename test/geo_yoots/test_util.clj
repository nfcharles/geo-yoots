(ns geo-yoots.test-util
  (:require [clojure.test :refer [is]]))


(defn round-float
  [x scale]
  (.floatValue (.setScale (bigdec x) scale BigDecimal/ROUND_HALF_UP)))

(defn compare-float
  [expected actual & {:keys [threshold]
                      :or {threshold 0.015}}]
  (is (<= (Math/abs (- expected actual)) threshold)))
