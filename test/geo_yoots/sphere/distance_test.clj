(ns geo-yoots.sphere.distance-test
  (:require [clojure.test :refer :all]
            [geo-yoots.constants :as geo.const]
            [geo-yoots.util.core :as geo.util]
            [geo-yoots.sphere.distance :as geo.sphere.dist]))



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
;; Case 1: Prime Meridian
;; ---

(def polygon
  (list
    [ 1.0  0.0]
    [ 0.0  1.0]
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
;; Case 2: Prime Meridian
;; ---

(def polygon-2
  (list
    [ 1.000000  0.0]
    [ 0.000000  1.0]
    [-1.000000  0.0]
    [ 0.000000 -1.0]
    [ 0.410995  0.326637]))

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
;; - Case 3: Random Polygon
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


;; ---
;; - Case 4: Dateline
;; ---

(def polygon-4
  (list
    [ 0.000000 -179.0]
    [-1.000000  180.0]
    [ 0.000000  179.0]
    [ 0.549159 -179.796081]
    [ 1.000000  180.0]))

(def pl-test-points-4
  (list
    [-1.045232 -178.612838]
    [ 1.055650 -178.501717]
    [ 0.857212  179.596280]
    [ 0.775169  179.974959]
    [-2.217478  177.334105]))

(def pl-expected-4
  (list
    60.80810884
    65.96818653
    25.70796515
    6.9327234
    164.84375525))


;; ---
;; - Case 5: North Pole
;; ---

(def polygon-5
  (list
    [89.0    0.0]
    [89.0   90.0]
    [89.0 -180.0]
    [89.0  -90.0]))

(def pl-test-points-5
  (list
    [88.978568 -151.508393]
    [89.164211   47.284886]))

(def pl-expected-5
  (list
    16.34236583
    7.68410199))


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
    (let [ct-dist (geo.sphere.dist/crosstrack-distance [-1.089144 1.077389] [0.0 1.0] [-1.0 0.0])
          d13     (geo.sphere.dist/haversine [0.0 1.0] [-1.089144 1.077389])]
      (compare-float
        (* geo.const/km->nm (geo.sphere.dist/alongtrack-distance2 d13 ct-dist))
        43.14639))))


(deftest crosstrack-distance-test
  (testing "GC 1"
    (compare-float
      (Math/abs (* geo.const/km->nm
                  (geo.sphere.dist/crosstrack-distance [-1.089144 1.077389] [0.0 1.0] [-1.0 0.0])))
      49.52030311)
    (compare-float
      (Math/abs (geo.sphere.dist/crosstrack-distance [51 69] [40.5 60.5] [50.5 80.5]))
      479.6 :threshold 0.1)))


(deftest crossarc-distance-test
  (testing "GC 1"
    (compare-float
      (Math/abs (* geo.const/km->nm
                  (geo.sphere.dist/crossarc-distance [-1.089144 1.077389] [0.0 1.0] [-1.0 0.0])))
      49.52030311 :threadshold 0.1)
    (compare-float
      (Math/abs (geo.sphere.dist/crossarc-distance [51 69] [40.5 60.5] [50.5 80.5]))
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
    (let [actual (geo.sphere.dist/to-polyline pl-test-points-1 polyline-1)]
      (compare-distance pl-expected-1 actual))))


(deftest point-within-distince-to-line-test
  (testing "Within Polyline 1"
    (let [limit 3 ; kilometers
          actual (geo.sphere.dist/within-distance-to-polyline? limit pl-test-points-1 polyline-1)]
      (compare-boolean [false true false false] actual)))

  (testing "Within Polyline 1 - 2"
    (let [limit 5 ; kilometers
          actual (geo.sphere.dist/within-distance-to-polyline? limit pl-test-points-1 polyline-1)]
      (compare-boolean [true true true false] actual))))


;; ---
;; - CIRCLES
;; ---

(deftest point-to-circle-distance-test
  (testing "Circle 1"
    (let [actual (geo.sphere.dist/to-circle c-test-points-1 (:center circle-1) (:radius circle-1))]
      (compare-distance c-expected-1 actual))))


(deftest point-within-distince-to-circle-test
  (testing "Within Circle 1"
    (let [limit 1 ; kilometers
          actual (geo.sphere.dist/within-distance-to-circle? limit c-test-points-1 (:center circle-1) (:radius circle-1))]
      (compare-boolean [false false true] actual)))

  (testing "Within Circle 1 - 2"
    (let [limit 1.5 ; kilometers
          actual (geo.sphere.dist/within-distance-to-circle? limit c-test-points-1 (:center circle-1) (:radius circle-1))]
      (compare-boolean [true false true] actual))))



;; ---
;; - POLYGONS
;; ---

(deftest point-to-polygon-distance-test
  (testing "Prime Meridian Simple Polygon 1"
    (let [actual (geo.sphere.dist/to-polygon pl-test-points polygon)]
      (compare-distance pl-expected actual)))

  (testing "Prime Meridian Simple Polygon 2"
    (let [actual (geo.sphere.dist/to-polygon pl-test-points-2 polygon-2)]
      (compare-distance pl-expected-2 actual)))

  (testing "Prime Meridian Simple Polygon 2 - 1"
    (let [actual (geo.sphere.dist/to-polygon pl-test-points-2-1 polygon-2)]
      (compare-distance pl-expected-2-1 actual)))

  (testing "Complex Polygon 1"
    (let [actual (geo.sphere.dist/to-polygon pl-test-points-3 polygon-3)]
      (compare-distance pl-expected-3 actual)))

  (testing "Dateline Simple Polygon 1"
    (let [actual (geo.sphere.dist/to-polygon pl-test-points-4 polygon-4)]
        (compare-distance pl-expected-4 actual)))

  (testing "North Pole Simple Polygon 1"
    (let [actual (geo.sphere.dist/to-polygon pl-test-points-5 polygon-5)]
        (compare-distance pl-expected-5 actual))))


(deftest point-within-distance-to-polygon-test
  (testing "Within Complex Polygon 1"
    (let [limit 200 ; kilometers
          actual (geo.sphere.dist/within-distance-to-polygon? limit pl-test-points-3 polygon-3)]
      (compare-boolean [true false true false false] actual)))

  (testing "Within Complex Polygon 1 - 2"
    (let [limit 500 ; kilometers
          actual (geo.sphere.dist/within-distance-to-polygon? limit pl-test-points-3 polygon-3)]
      (compare-boolean [true false true true true] actual))))
