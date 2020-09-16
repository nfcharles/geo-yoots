(ns geo-yoots.sphere.distance.signed
  (:require [geo-yoots.constants :as geo.const]
            [geo-yoots.util.core :as geo.util]
            [geo-yoots.sphere.distance.core :as geo.sphere.dist]
            [geo-yoots.sphere.impl.inclusion :as geo.sphere.incl]))



;;; ===
;;; - Signed distance from point to polygon
;;; ---

;; TODO: Calculate centroid and use as projection plane?
(defn to-polygon
  [pt plane vertices]
  (let [dst (geo.sphere.dist/to-polygon pt vertices)
        ins (geo.sphere.incl/point-in-polygon? pt plane vertices)]
    (if ins (* -1 dst) dst)))

(defn -within-distance-to-polygon?
  [limit pt vertices]
  (loop [xs vertices]
    (if-let [x (first xs)]
      (let [[arc-p1 arc-p2] x]
        (if (<= (geo.sphere.dist/crossarc-distance pt arc-p1 arc-p2) limit)
          true
          (recur (rest xs))))
      false)))

(defn within-distance-to-polygon?
  [limit pt plane vertices]
  (if (geo.sphere.incl/point-in-polygon? pt plane vertices)
    true
    (-within-distance-to-polygon? limit pt (geo.util/gen-polygon-edges vertices))))





;; ---------- TODO: Make proper unit tests


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
              [11.495597 -89.052659]]))

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
              [ 1.0  0.0]]))

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
              [ 1.0       0.0]]))

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
              [ 0.0      -179.0]]))

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
              [89.0    0.0]]))

(def km->nautical-mile 0.539957)

(defn -main
  [& args]
  (let []
    (time
      (doseq [test [test-1 test-2 test-3 test-4 test-5]]
        (println "Test...")
        (doseq [latlon (:points test)]
          (let [poly (:polygon test)]
            (println (format "%s -> %s" latlon (* km->nautical-mile (to-polygon latlon (first poly) poly))))))))))
