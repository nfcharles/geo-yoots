(ns geo-yoots.sphere.distance.core-test
  (:require [clojure.test :refer :all]
            [geo-yoots.test-util :as test.util]
            [geo-yoots.constants :as geo.const]
            [geo-yoots.util.core :as geo.util]
            [geo-yoots.sphere.distance.core :as geo.sphere.dist]))



;; ============
;; - Fixtures -
;; ============


;; ---
;; ** (POLY)LINES **
;; ---

(def ptest-1
  (hash-map
    :points   [[10.017388 -83.086182]
               [10.032660 -83.098842]
               [10.017036 -83.104704]
               [10.227243 -82.980491]]
    :polyline [[10.053470 -83.102527]
               [10.050460 -83.083483]
               [10.038999 -83.066965]
               [10.024276 -83.056931]]
    :dists    [ 1.66648358
                1.19913249
                2.18027416
               12.23573625]))



;; ---
;; ** CIRCLES **
;; ---

;; ---
;; - Case 1
;; ---

(def ctest-1
  (hash-map
    :center [10.0108167 -83.09845]
    :radius (/ 300 geo.const/km->meters)
    :points [[10.017388 -83.086182]
             [10.032660 -83.098842]
             [10.017036 -83.104704]]
    :dists  [0.66372844
             1.1497013
             0.36352918]))

;; ---
;; ** POLYGONS **
;; ---

;; ---
;; Case 1: Prime Meridian
;; ---

(def test-1
  (hash-map
    :points  [[ 0.967166 -1.309784]
              [ 1.411533  0.873626]
              [ 0.019355  1.760223]
              [-1.089144  1.077389]
              [-0.956165 -2.189761]
              [ 0.000000 -2.004207]]
    :polygon [[ 1.0  0.0]
              [ 0.0  1.0]
              [-1.0  0.0]
              [ 0.0 -1.0]]
    :dists   [54.20923531
              54.55440421
              45.65898913
              49.52030311
              91.6410489
              60.29313032]))


;; ---
;; Case 2: Prime Meridian
;; ---

(def test-2
  (hash-map
    :points  [[0.625476 -0.177724]
              [0.871647 -0.133418]
              [0.600741  0.424787]]
    :polygon [[ 1.000000  0.0]
              [ 0.000000  1.0]
              [-1.000000  0.0]
              [ 0.000000 -1.0]
              [ 0.410995  0.326637]]
    :dists   [20.23665323
              10.7419377
               1.08127628]))


;; ---
;; - Case 3: Random Polygon
;; ---

(def test-3
  (hash-map
    :points  [[ 5.841945 -92.985009]
              [ 9.598825 -96.723864]
              [10.262200 -92.179789]
              [ 6.861973 -84.472807]
              [ 1.769543 -89.618460]]
    :polygon [[11.387396 -89.081863]
              [ 8.448303 -87.881012]
              [ 7.816833 -86.533305]
              [ 6.343054 -88.996131]
              [ 5.335717 -91.427290]
              [ 7.608643 -92.361043]
              [ 8.975958 -91.411855]
              [ 9.777739 -89.516625]
              [11.495597 -89.052659]]
    :dists   [ 74.66089467
              285.22001101
               89.6123676
              135.43121731
              239.34137904]))

;; ---
;; - Case 4: Dateline
;; ---

(def test-4
  (hash-map
    :points  [[-1.045232 -178.612838]
              [ 1.055650 -178.501717]
              [ 0.857212  179.596280]
              [ 0.775169  179.974959]
              [-2.217478  177.334105]]
    :polygon [[ 0.000000 -179.0]
              [-1.000000  180.0]
              [ 0.000000  179.0]
              [ 0.549159 -179.796081]
              [ 1.000000  180.0]]
    :dists   [ 60.80810884
               65.96818653
               25.70796515
                6.9327234
              164.84375525]))

;; ---
;; - Case 5: North Pole
;; ---

(def test-5
  (hash-map
    :points  [[88.978568 -151.508393]
              [89.164211   47.284886]]
    :polygon [[89.0    0.0]
              [89.0   90.0]
              [89.0 -180.0]
              [89.0  -90.0]]
    :dists   [16.34236583
               7.68410199]))


;; =========
;; - Tests -
;; =========


;; ---
;; - UTILS
;; ---

;; ===
;; Test Source
;; ===
;; https://rdrr.io/cran/geosphere/man/alongTrackDistance.html
;; ---
;;
;; library(geosphere)
;; alongTrackDistance(c(1.0,0.0),c(0.0,-1.0),c(1.077389,-1.089144), r=.63728)
;; 0.007923432  M
;; 79.23432     KM
;; 42.783110151 NM

(deftest alongtrack-distance-test
  (testing "GC 1"
    (let [ct-dist (geo.sphere.dist/crosstrack-distance [-1.089144 1.077389] [0.0 1.0] [-1.0 0.0])
          d13     (geo.sphere.dist/haversine [0.0 1.0] [-1.089144 1.077389])]
      (test.util/compare-distance 42.783110151 (geo.sphere.dist/alongtrack-distance2 d13 ct-dist)))))

(deftest crosstrack-distance-test
  (testing "GC 2.1"
    (test.util/compare-distance 49.52030311 (Math/abs (geo.sphere.dist/crosstrack-distance [-1.089144 1.077389] [0.0 1.0] [-1.0 0.0]))))

  (testing "GC 2.2"
    (test.util/compare-distance 479.6 (Math/abs (geo.sphere.dist/crosstrack-distance [51 69] [40.5 60.5] [50.5 80.5])) :factor 1)))

(deftest crossarc-distance-test
  (testing "GC 3.1"
    (test.util/compare-distance 49.52030311 (Math/abs (geo.sphere.dist/crossarc-distance [-1.089144 1.077389] [0.0 1.0] [-1.0 0.0]))))

  (testing "GC 3.2"
    (test.util/compare-distance 479.6 (Math/abs (geo.sphere.dist/crossarc-distance [51 69] [40.5 60.5] [50.5 80.5])) :factor 1)))


;; ---
;; - (POLY)LINES
;; ---

(deftest point-to-polyline-distance-test
  (let [testcases [ptest-1]]
    (doseq [[test i] (map vector testcases (range 1 (inc (count testcases))))]
      (doseq [[latlon expected j] (map vector (:points test) (:dists test) (range 1 (inc (count (:points test)))))]
        (testing (format "Polygon Test Case %s.%s" i j)
          (let [poly (:polyline test)]
            (test.util/compare-distance expected (geo.sphere.dist/to-polyline latlon poly))))))))

(deftest point-within-distance-to-line-test
  (doseq [case [[3 [false true false false]] [5 [true true true false]]]]
    (let [[limit expected] case
          actual (map #(geo.sphere.dist/within-distance-to-polyline? limit % (:polyline ptest-1)) (:points ptest-1))]
      (testing (format "Within Polyline: limit %s" limit)
        (test.util/compare-boolean expected actual)))))


;; ---
;; - CIRCLES
;; ---

(deftest point-to-circle-distance-test
  (let [testcases [ctest-1]]
    (doseq [[test i] (map vector testcases (range 1 (inc (count testcases))))]
      (doseq [[latlon expected j] (map vector (:points test) (:dists test) (range 1 (inc (count (:points test)))))]
        (testing (format "Circle Test Case %s.%s" i j)
          (let [circle (:polygon test)]
            (test.util/compare-distance expected (geo.sphere.dist/to-circle latlon (:center test) (:radius test)) :threshold 0.001)))))))

(deftest point-within-distance-to-circle-test
  (doseq [case [[1 [false false true]] [1.5 [true false true]]]]
    (let [[limit expected] case
          actual (map #(geo.sphere.dist/within-distance-to-circle? limit % (:center ctest-1) (:radius ctest-1)) (:points ctest-1))]
      (testing (format "Within Circle: limit %s" limit)
        (test.util/compare-boolean expected actual)))))


;; ---
;; - POLYGONS
;; ---

(deftest point-to-polygon-distance-test
  (let [testcases [test-1 test-2 test-3 test-4 test-5]]
    (doseq [[test i] (map vector testcases (range 1 (inc (count testcases))))]
      (doseq [[latlon expected j] (map vector (:points test) (:dists test) (range 1 (inc (count (:points test)))))]
        (testing (format "Polygon Test Case %s.%s" i j)
          (let [poly (:polygon test)]
            ;; Test both orientations of polygons
            (test.util/compare-distance expected (geo.sphere.dist/to-polygon latlon (reverse poly)))
            (test.util/compare-distance expected (geo.sphere.dist/to-polygon latlon poly))))))))

(deftest point-within-distance-to-polygon-test
  (doseq [case [[200 [true false true false false]] [500 [true false true true true]]]]
    (let [[limit expected] case
          actual (map #(geo.sphere.dist/within-distance-to-polygon? limit % (:polygon test-3)) (:points test-3))]
      (testing (format "Within Simple Polygon: limit %s" limit)
        (test.util/compare-boolean expected actual)))))
