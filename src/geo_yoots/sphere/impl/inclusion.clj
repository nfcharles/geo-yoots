(ns geo-yoots.sphere.impl.inclusion
  (:require [clojure.pprint :as pp]
            [geo-yoots.util.core :as geo.util]
            [geo-yoots.sphere.util :as geo.sphere.util]
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

(defn same-side
  [p1 p2 a b]
  (let [ab (geo.sphere.util/a->b-vector a b)]
    (>= (mtx/dot
          (mtx/cross ab (geo.sphere.util/a->b-vector a p1))
          (mtx/cross ab (geo.sphere.util/a->b-vector a p2)))
	0)))

(defn same-side-algo
  [pt [a b c]]
  #_(println (format "PT=%s, A=%s, B=%s, C=%s" pt a b c))
  (and
    (same-side pt a b c)
    (same-side pt b a c)
    (same-side pt c a b)))

(defn project-vertices
  [pv unit-nv vertices]
  (loop [xs  vertices
         acc []]
    (if-let [x (first xs)]
      (let [av (mtx/matrix (geo.sphere.util/latlon->cartesian x))]
        (recur (rest xs) (conj acc (geo.sphere.util/ortho-plane-projection av pv unit-nv))))
      acc)))

(defn gen-triangle-partitions
  "Generates triangular partitions for a polygon"
  [vertices]
  (let [anchor (first vertices)]
    (loop [xs  (rest vertices)
           tri [anchor]
           acc []]
      (if-let [x (first xs)]
        (if (= (count tri) 2)
          (recur xs [anchor] (conj acc (conj tri x)))
          (recur (rest xs) (conj tri x) acc))
        acc))))

(defn point-in-polygon?
  "Returns true if point is in polygon, false otherwise"
  [pt vertices]
  (let [uniq-verts (geo.util/ensure-unique-vertices vertices)
        cent       (geo.sphere.util/centroid uniq-verts)        ;; Centroid for projection plane
        unit-nv    (geo.sphere.util/unit-normal-vector cent)    ;; Unit normal vector
        pv         (geo.sphere.util/latlon->vector cent)        ;; Projection Plane
        av         (geo.sphere.util/latlon->vector pt)          ;; Test Point
        projected  (project-vertices pv unit-nv uniq-verts)     ;; Projected Vertices to Plane
        triangles  (gen-triangle-partitions projected)]         ;; Triangle Partitions
    (loop [xs triangles
           acc 0]
      (if-let [x (first xs)]
        (recur (rest xs) (+ acc (if (same-side-algo av x) 1 0)))

        ;; If point intersects with odd number of triangles, inside otherwise outside.
        (odd? acc)))))
