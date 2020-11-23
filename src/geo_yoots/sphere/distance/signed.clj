(ns geo-yoots.sphere.distance.signed
  (:require [geo-yoots.constants :as geo.const]
            [geo-yoots.util.core :as geo.util]
            [geo-yoots.sphere.distance.core :as geo.sphere.dist]
            [geo-yoots.sphere.inclusion :as geo.sphere.incl]))



;;; ===
;;; - Signed distance from point to circle
;;; ---

(defn -to-circle
  [pt center radius]
  (- (geo.sphere.dist/haversine pt center) radius))

(defn to-circle
  [pt center radius]
  (-to-circle pt center radius))

(defn within-distance-to-circle?
  [limit pt center radius]
  (<= (-to-circle pt center radius) limit))


;;; ===
;;; - Signed distance from point to polygon
;;; ---

(defn to-polygon
  [pt vertices]
  (let [dst (geo.sphere.dist/to-polygon pt vertices)]
    (if (geo.sphere.incl/point-in-polygon? pt vertices) (* -1 dst) dst)))

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
  [limit pt vertices]
  (if (geo.sphere.incl/point-in-polygon? pt vertices)
    true
    (-within-distance-to-polygon? limit pt (geo.util/gen-polygon-edges vertices))))
