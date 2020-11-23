(ns geo-yoots.sphere.centroid
  (:require [geo-yoots.sphere.util :as geo.sphere.util]))




;; ===
;; - Point set centroid
;; ---
;;
;; Find centroid of set of (lat, lon) points
;;
;; https://en.wikipedia.org/wiki/Centroid
;; ---


(defn points
  [pts]
  (let [len (count pts)]
    (loop [xs pts
           x 0
           y 0
           z 0]
      (if-let [pt (first xs)]
        (recur (rest xs) (+ x (geo.sphere.util/latlon->x pt))
                         (+ y (geo.sphere.util/latlon->y pt))
                         (+ z (geo.sphere.util/latlon->z pt)))
        (geo.sphere.util/cartesian->latlon [(/ x len) (/ y len) (/ z len)])))))




;; ===
;; - Polygon centroid
;; ---
;;
;; Find centroid of simple polygon
;;
;; https://en.wikipedia.org/wiki/Centroid#Of_a_polygon
;; ---

(defn polygon
  [vertices]
  (throw (java.lang.Exception. "Not Implemented Yet")))
