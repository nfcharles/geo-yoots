(ns geo-yoots.sphere.core-test
  (:require [clojure.test :refer :all]
            [geo-yoots.sphere.core :as geo.sphere]))



;; ---------
;; - Utils -
;; ---------

(defn round-float
  [x scale]
  (.floatValue (.setScale (bigdec x) scale BigDecimal/ROUND_HALF_EVEN)))

(defn compare-float
  [expected actual & {:keys [scale]
                      :or {scale 4}}]
  (is (= (round-float expected scale)
         (round-float actual scale))))



;; ---------
;; - Tests -
;; ---------

(deftest bearing-test
  )

(deftest crosstrack-distance-test
  )

(deftest crossarc-distance-test
  )

(deftest point-to-point-distance-test
  )

(deftest point-to-line-distance-test
  )

(deftest point-to-circle-distance-test
  )

(deftest point-to-polygon-distance-test
  )