(ns geo-yoots.sphere.core-test
  (:require [clojure.test :refer :all]
            [geo-yoots.constants :as geo.const]
            [geo-yoots.util.core :as geo.util]
            [geo-yoots.sphere.core :as geo.sphere]))



;; ---------
;; - Utils -
;; ---------

(def distance-threshold 0.0809935) ; nautical miles (150 meters)

(defn round-float
  [x scale]
  (.floatValue (.setScale (bigdec x) scale BigDecimal/ROUND_HALF_UP)))

(defn compare-float
  [expected actual & {:keys [threshold]
                      :or {threshold distance-threshold}}]
  (is (<= (Math/abs (- expected actual)) threshold)))

(defn compare-distance
  [expected actual & {:keys [threshold]
                      :or {threshold distance-threshold}}]
  (loop [xs (seq (map vector expected actual))]
    (if-let [x (first xs)]
      ;; Convert actual output to nautical miles
      (let [[exp act] x]
        (is (<= (Math/abs (- exp (* geo.const/km->nm act))) threshold))
        (recur (rest xs))))))

(defn compare-boolean
  [expected actual]
  (loop [xs (seq (map vector expected actual))]
    (if-let [x (first xs)]
      (let [[a b] x]
        (is (= a b))
        (recur (rest xs))))))


;; ============
;; - Fixtures -
;; ============


;; ---
;; ** (POLY)LINES **
;; ---

(def polyline-1
  (list
    [10.053470 -83.102527]
    [10.050460 -83.083483]
    [10.038999 -83.066965]
    [10.024276 -83.056931]))

(def pl-test-points-1
  (list
    [10.017388 -83.086182]
    [10.032660 -83.098842]
    [10.017036 -83.104704]
    [10.227243 -82.980491]))

(def pl-expected-1
  (list
    1.66648358
    1.19913249
    2.18027416
   12.23573625))


;; ---
;; ** CIRCLES **
;; ---

;; ---
;; - Case 1
;; ---

(def circle-1
  (hash-map
    :center [10.0108167 -83.09845]
    :radius (/ 300 geo.const/km->meters)))

(def c-test-points-1
  (list
    [10.017388 -83.086182]
    [10.032660 -83.098842]
    [10.017036 -83.104704]))

(def c-expected-1
  (list
    0.66372844
    1.1497013
    0.36352918))



;; ---
;; ** POLYGONS **
;; ---

;; ---
;; Case 1
;; ---

(def polygon
  (list
    [ 1.0   0.0]
    [ 0.0   1.0]
    [-1.0  0.0]
    [ 0.0 -1.0]))

(def pl-test-points
  (list
    [ 0.967166 -1.309784]
    [ 1.411533  0.873626]
    [ 0.019355  1.760223]
    [-1.089144  1.077389]
    [-0.956165 -2.189761]
    [ 0.000000 -2.004207]))

(def pl-expected
  (list
    54.20923531
    54.55440421
    45.65898913
    49.52030311
    91.6410489
    60.29313032))


;; ---
;; Case 2
;; ---

(def polygon-2
  (list
    [ 1.0  0.0]
    [ 0.0  1.0]
    [-1.0  0.0]
    [ 0.0 -1.0]
    [0.410995 0.326637]))

(def pl-test-points-2
  (list
    [0.625476 -0.177724]
    [0.871647 -0.133418]))

(def pl-expected-2
  (list
    20.23665323
    10.7419377))

;; --- 2.1

(def pl-test-points-2-1
  (list
    [0.600741 0.424787]))

(def pl-expected-2-1
  (list
    1.08127628))


;; ---
;; - Case 3
;; ---

(def polygon-3
  (list
    [11.387396 -89.081863]
    [ 8.448303 -87.881012]
    [ 7.816833 -86.533305]
    [ 6.343054 -88.996131]
    [ 5.335717 -91.427290]
    [ 7.608643 -92.361043]
    [ 8.975958 -91.411855]
    [ 9.777739 -89.516625]
    [11.495597 -89.052659]))

(def pl-test-points-3
  (list
    [ 5.841945 -92.985009]   ;t1
    [ 9.598825 -96.723864]   ;t4
    [10.262200 -92.179789]   ;t2
    [ 6.861973 -84.472807]   ;t3
    [ 1.769543 -89.618460])) ;t5

(def pl-expected-3
  (list
    74.66089467
    285.22001101
    89.6123676
    135.43121731
    239.34137904))

;; =========
;; - Tests -
;; =========


;; ---
;; - UTILS
;; ---

(deftest bearing-test
  )

(deftest alongtrack-distance-test
  (testing "GC 1"
    (let [ct-dist (geo.sphere/crosstrack-distance {:lat -1.089144 :lon 1.077389} {:lat 0.0 :lon 1.0}
                                                                                 {:lat -1.0 :lon 0.0})
          d13     (geo.util/haversine {:lat 0.0 :lon 1.0} {:lat -1.089144 :lon 1.077389})]
      (compare-float
        (* geo.const/km->nm (geo.sphere/alongtrack-distance2 d13 ct-dist))
        43.14639))))

(deftest crosstrack-distance-test
  (testing "GC 1"
    (compare-float
      (Math/abs (* geo.const/km->nm
                  (geo.sphere/crosstrack-distance {:lat -1.089144 :lon 1.077389} {:lat 0.0 :lon 1.0}
                                                                                 {:lat -1.0 :lon 0.0})))
      49.52030311)
    (compare-float
      (Math/abs (geo.sphere/crosstrack-distance {:lat 51 :lon 69} {:lat 40.5 :lon 60.5}
                                                                  {:lat 50.5 :lon 80.5}))
      479.6 :threshold 0.1)))

(deftest crossarc-distance-test
  (testing "GC 1"
    (compare-float
      (Math/abs (* geo.const/km->nm
                  (geo.sphere/crossarc-distance {:lat -1.089144 :lon 1.077389} {:lat 0.0 :lon 1.0}
                                                                               {:lat -1.0 :lon 0.0})))
      49.52030311 :threadshold 0.1)
    (compare-float
      (Math/abs (geo.sphere/crossarc-distance {:lat 51 :lon 69} {:lat 40.5 :lon 60.5}
                                                                {:lat 50.5 :lon 80.5}))
      479.6 :threadhold 0.1)))


;; ---
;; - POINTS
;; ---

(deftest point-to-point-distance-test
  )


;; ---
;; - (POLY)LINES
;; ---

(deftest point-to-line-distance-test
  (testing "Polyline 1"
    (let [actual (geo.sphere/min-distance-to-polyline pl-test-points-1 polyline-1)]
      (compare-distance pl-expected-1 actual))))


(deftest point-within-line-distance-test
  (testing "Within Polyline 1"
    (let [limit 3 ; kilometers
          actual (geo.sphere/within-distance-to-polyline? limit pl-test-points-1 polyline-1)]
      (compare-boolean [false true false false] actual)))

  (testing "Within Polyline 1 - 2"
    (let [limit 5 ; kilometers
          actual (geo.sphere/within-distance-to-polyline? limit pl-test-points-1 polyline-1)]
      (compare-boolean [true true true false] actual))))


;; ---
;; - CIRCLES
;; ---

(deftest point-to-circle-distance-test
  (testing "Circle 1"
    (let [actual (geo.sphere/distance-to-circle c-test-points-1 (:center circle-1) (:radius circle-1))]
      (compare-distance c-expected-1 actual)))

  (testing "Within Circle 1"
    (let [limit 1 ; kilometers
          actual (geo.sphere/within-distance-to-circle? limit c-test-points-1 (:center circle-1) (:radius circle-1))]
      (compare-boolean [false false true] actual)))

  (testing "Within Circle 1 - 2"
    (let [limit 1.5 ; kilometers
          actual (geo.sphere/within-distance-to-circle? limit c-test-points-1 (:center circle-1) (:radius circle-1))]
      (compare-boolean [true false true] actual))))




;; ---
;; - POLYGONS
;; ---

(deftest point-to-polygon-distance-test
  (testing "Prime Meridian Simple Polygon 1"
    (let [actual (geo.sphere/min-distance-to-polygon pl-test-points polygon)]
      (compare-distance pl-expected actual)))

  (testing "Prime Meridian Simple Polygon 2"
    (let [actual (geo.sphere/min-distance-to-polygon pl-test-points-2 polygon-2)]
      (compare-distance pl-expected-2 actual)))

  (testing "Prime Meridian Simple Polygon 2 - 1"
    (let [actual (geo.sphere/min-distance-to-polygon pl-test-points-2-1 polygon-2)]
      (compare-distance pl-expected-2-1 actual)))

  (testing "Complex Polygon 1"
    (let [actual (geo.sphere/min-distance-to-polygon pl-test-points-3 polygon-3)]
      (compare-distance pl-expected-3 actual)))

  (testing "Within Complex Polygon 1"
    (let [limit 200 ; kilometers
          actual (geo.sphere/within-distance-to-polygon? limit pl-test-points-3 polygon-3)]
      (compare-boolean [true false true false false] actual)))

  (testing "Within Complex Polygon 1 - 2"
    (let [limit 500 ; kilometers
          actual (geo.sphere/within-distance-to-polygon? limit pl-test-points-3 polygon-3)]
      (compare-boolean [true false true true true] actual))))
