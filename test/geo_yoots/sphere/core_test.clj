(ns geo-yoots.sphere.core-test
  (:require [clojure.test :refer :all]
            [geo-yoots.constants :as geo.const]
            [geo-yoots.sphere.core :as geo.sphere]))



;; ---------
;; - Utils -
;; ---------

(defn round-float
  [x scale]
  (.floatValue (.setScale (bigdec x) scale BigDecimal/ROUND_HALF_UP)))

(defn compare-float
  [expected actual & {:keys [scale]
                      :or {scale 4}}]
  (is (= (round-float expected scale)
         (round-float actual scale))))

(defn compare-distance
  [expected actual]
  (loop [xs (seq (map vector expected actual))]
    (if-let [x (first xs)]
      ;; Convert actual output to nautical miles
      (let [[exp act] x]
        (compare-float exp (* act geo.const/km->nm) :scale 2)
        (recur (rest xs))))))


;; ============
;; - Fixtures -
;; ============

;; ---
;; Case 1
;; ---

(def polygon
  (list
    [ 1.0   0.0]
    [ 0.0   1.0]
    [-1.0,  0.0]
    [ 0.0, -1.0]))

(def points
  (list
    [ 0.967166 -1.309784]
    [ 1.411533  0.873626]
    [ 0.019355  1.760223]
    [-1.089144  1.077389]
    [-0.956165 -2.189761]
    [ 0.000000 -2.004207]))

;; ---
;; Case 2
;; ---

(def polygon-2
  (list
    [ 1.0   0.0]
    [ 0.0   1.0]
    [-1.0  0.0]
    [ 0.0 -1.0]
    [0.410995 0.326637]))

(def points-2
  (list
    [0.625476 -0.177724]
    [0.871647 -0.133418]))

(def expected-2
  (list
    20.23665323
    10.7419377))


;; =========
;; - Tests -
;; =========


;; ---
;; - UTILS
;; ---

(deftest bearing-test
  )

(deftest crosstrack-distance-test
  )

(deftest crossarc-distance-test
  )

;; ---
;; - POINTS
;; ---

(deftest point-to-point-distance-test
  )

;; ---
;; - (POLY)LINES
;; ---

(deftest point-to-line-distance-test
  )

;; ---
;; - CIRCLES
;; ---

(deftest point-to-circle-distance-test
  )

;; ---
;; - POLYGONS
;; ---

(deftest point-to-polygon-distance-test
  (testing "Prime Meridian Simple Polygon 1"
    )

  (testing "Prime Meridian Simple Polygon 2"
    (let [actual (geo.sphere/min-distance-to-polygon points-2 polygon-2)]
      (compare-distance expected-2 actual))))
