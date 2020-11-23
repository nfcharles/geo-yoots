(ns geo-yoots.sphere.inclusion
  (:require [clojure.pprint :as pp]
            [geo-yoots.util.core :as geo.util]
            [geo-yoots.sphere.transformations :as geo.sphere.xform]
            [clojure.core.matrix :as mtx]
            [clojure.core.matrix.operators :as mtx.op]))



;; ==========================
;; -  Point Inclusion Test  -
;; -
;; -  sources:
;; -   * http://erich.realtimerendering.com/ptinpoly/
;; -   * https://blackpawn.com/texts/pointinpoly/#:~:text=A%20common%20way%20to%20check,but%20it%20is%20very%20slow.
;; --------------------------

;; -----------------------
;; - Same Side Algorithm
;; ---

(defn same-side?
  [p1 p2 a b]
  (let [ab (geo.sphere.xform/a->b-vector a b)]
    (>= (mtx/dot
          (mtx/cross ab (geo.sphere.xform/a->b-vector a p1))
          (mtx/cross ab (geo.sphere.xform/a->b-vector a p2)))
	0)))

(defn within-sides?
  [pt [a b c]]
  #_(println (format "PT=%s, A=%s, B=%s, C=%s" pt a b c))
  (and
    (same-side? pt a b c)
    (same-side? pt b a c)
    (same-side? pt c a b)))

(defn point-in-polygon?
  "Returns true if point is in polygon, false otherwise"
  [pt vertices]
  (let [av         (geo.sphere.xform/latlon->vector pt)                    ;; Test Point
        projected  (geo.sphere.xform/vertices->projection-plane vertices)  ;; Projected Vertices to Plane
        triangles  (geo.sphere.xform/partition-polygon projected)]         ;; Triangle Partitions
    (loop [xs triangles
           acc 0]
      (if-let [x (first xs)]
        (recur (rest xs) (+ acc (if (within-sides? av x) 1 0)))

        ;; If point intersects with odd number of triangles, inside otherwise outside.
        (odd? acc)))))
