(ns geo-yoots.sphere.distance.signed-test
  (:require [clojure.test :refer :all]
            [geo-yoots.test-util :as test.util]
            [geo-yoots.constants :as geo.const]
            [geo-yoots.util.core :as geo.util]
            [geo-yoots.sphere.distance.core :as geo.sphere.dist]
            [geo-yoots.sphere.distance.signed :as geo.sphere.signed-dist]))



;; ---------
;; - Utils -
;; ---------

(def distance-threshold 0.0809935) ; nautical miles (150 meters)
(def km->nautical-mile 0.539957)


(defn compare-distance
  [expected actual & {:keys [threshold]
                      :or {threshold distance-threshold}}]
    (is (<= (Math/abs (- expected (* km->nautical-mile actual))) threshold)))

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
;; Circles
;; ---

(def ctest-1
  (hash-map
    :center [10.0108167 -83.09845]
    :radius 0.3
    :points [[10.017388 -83.086182]
             [10.032660 -83.098842]
             [10.017036 -83.104704]
             [10.010784 -83.099913]
             [10.010632 -83.098212]]
    :dists  [ 0.66372844
              1.1497013
              0.36352918
             -0.07546277
             -0.14407051]))


;; ---
;; - Polygons
;; ---

(def test-1
  (hash-map
    :points  [[ 5.841945 -92.985009]
              [ 9.598825 -96.723864]
              [10.262200 -92.179789]
              [ 6.861973 -84.472807]
              [ 1.769543 -89.618460]
              [10.671897 -89.086871]
              [ 7.507252 -90.384634]
              [ 7.706351 -86.932255]]

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
              239.34137904
              -10.77336361
              -96.30210153
              -6.52156558]))

(def test-2
  (hash-map
    :points  [[ 0.967166 -1.309784]
              [ 1.411533  0.873626]
              [ 0.019355  1.760223]
              [-1.089144  1.077389]
              [-0.956165 -2.189761]
              [ 0.000000 -2.004207]
              [ 2.222395 -0.930047]
              [-0.560560, 0.080745]]

    :polygon [[ 1.0  0.0]
              [ 0.0  1.0]
              [-1.0  0.0]
              [ 0.0 -1.0]
              [ 1.0  0.0]]

    :dists   [54.20923531
              54.55440421
              45.65898913
              49.52030311
              91.6410489
              60.29313032
              92.20710627
             -15.23009408]))

(def test-3
  (hash-map
    :points  [[ 0.600741  0.424787]
              [ 2.456152 -0.699912]
              [-2.039407 -1.301563]
              [ 0.330238  0.066512]]

    :polygon [[ 1.0       0.0]
              [ 0.0       1.0]
              [-1.0       0.0]
              [ 0.0      -1.0]
              [ 0.410995  0.326637]
              [ 1.0       0.0]]

    :dists   [ 1.08127628
              96.99443827
              99.36780063
              -0.01044732]))

(def test-4
  (hash-map
    :points  [[-1.045232 -178.612838]
              [ 1.055650 -178.501717]
              [ 0.857212  179.596280]
              [ 0.775169  179.974959]
              [-2.217478  177.334105]
              [0.917196, -178.902670]
              [0.181856,  179.962286]]

    :polygon [[ 0.0      -179.0]
              [-1.0       180.0]
              [ 0.0       179.0]
              [ 0.549159 -179.796081]
              [ 1.0       180.0]
              [ 0.0      -179.0]]

    :dists   [ 60.80810884
               65.96818653
               25.70796515
                6.9327234
              164.84375525
               43.06805484
              -14.04451015]))

(def test-5
  (hash-map
    :points  [[88.978568 -151.508393]
              [89.164211   47.284886]
              [87.891007  119.831293]
              [89.714872  124.031506]]

    :polygon [[89.0     0.0]
              [89.0   90.0]
              [89.0  180.0]
              [89.0  -90.0]
              [89.0    0.0]]

    :dists   [ 16.34236583
                7.68410199
               79.75625468
              -25.65070775]))


;; =========
;; - Tests -
;; =========


;; ---
;; - CIRCLES
;; ---


(deftest point-to-circle-distance-test
  (let [testcases [ctest-1]]
    (doseq [[test i] (map vector testcases (range 1 (inc (count testcases))))]
      (doseq [[latlon expected j] (map vector (:points test) (:dists test) (range 1 (inc (count (:points test)))))]
        (testing (format "Circle Test Case %s.%s" i j)
          (let [circle (:polygon test)]
            (compare-distance expected (geo.sphere.dist/to-circle latlon (:center test) (:radius test)) :threshold 0.001)))))))


;; ---
;; - POLYGONS
;; ---

(deftest point-to-polygon-signed-distance-test
  (let [testcases [test-1 test-2 test-3 test-4 test-5]]
    (doseq [[test i] (map vector testcases (range 1 (inc (count testcases))))]
      (doseq [[latlon expected j] (map vector (:points test) (:dists test) (range 1 (inc (count (:points test)))))]
        (testing (format "Polygon Test Case %s.%s" i j)
          (let [poly (:polygon test)]
            ;; Test both orientations of polygons
            (compare-distance expected (geo.sphere.signed-dist/to-polygon latlon (reverse poly)))
            (compare-distance expected (geo.sphere.signed-dist/to-polygon latlon poly))))))))
