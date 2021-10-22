(ns geo-yoots.sphere.inclusion
  (:require [clojure.pprint :as pp]
            [geo-yoots.constants :as geo.const]
            [geo-yoots.util.core :as geo.util]
            [geo-yoots.sphere.util :as geo.sphere.util]
            [geo-yoots.sphere.transformations :as geo.sphere.xform]
            [clojure.core.matrix :as mtx]
            [clojure.core.matrix.operators :as mtx.op]))


;;
;; New Projection Method
;;

(defn vertices->projection-plane3
  [vertices & {:keys [pt] ;; (lat,lon)
               :or {pt nil}}]
  (let [uniq-verts (geo.util/ensure-unique-vertices vertices)
;;        cent       (geo.sphere.util/centroid uniq-verts)
        ;; Solves antipodal pt -> polygon edgecase projection issues; use augmented centroid for projection plane.
        cent       (geo.sphere.util/centroid (vector (geo.sphere.util/centroid uniq-verts) pt))
;;      _            (println (format "CENT=%s, CENT_WITH_PT=%s, UBER_CENT=%s" (geo.sphere.util/centroid uniq-verts) (geo.sphere.util/centroid (conj uniq-verts pt)) cent))

        ;; projection plane pt
        cv         (geo.sphere.xform/latlon->vector cent)

        ;; projection plane unit normal
        unit-nv    (geo.sphere.xform/unit-normal-vector cent)]

    (loop [xs  uniq-verts
           acc []]
      (if-let [x (first xs)]
        (let [av (geo.sphere.xform/latlon->vector x)]
          (recur (rest xs) (conj acc (geo.sphere.xform/ortho-plane-projection av cv unit-nv))))
        (if pt ;; (mtx/matrix (geo.sphere.util/latlon->cartesian pt))
          [(geo.sphere.xform/ortho-plane-projection (geo.sphere.xform/latlon->vector pt) cv unit-nv) acc]
          [nil acc])))))

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


#_(defn point-in-polygon?
  "Returns true if point is in polygon, false otherwise"
  [pt vertices]
  (let [av         (geo.sphere.xform/latlon->vector pt)                    ;; Test Point
        projected  (geo.sphere.xform/vertices->projection-plane vertices)  ;; Projected Vertices to Plane
        ;;projected  (rest (vertices->projection-plane3 vertices :pt pt))  ;; Projected Vertices to Plane
        triangles  (geo.sphere.xform/partition-polygon projected)]         ;; Triangle Partitions
    (loop [xs triangles
           acc 0]
      (if-let [x (first xs)]
        (recur (rest xs) (+ acc (if (within-sides? av x) 1 0)))

        ;; If point intersects with odd number of triangles, inside otherwise outside.
        (odd? acc)))))


(defn point-in-polygon?
  "Returns true if point is in polygon, false otherwise"
  [pt vertices]
  (let [[pv projected] (vertices->projection-plane3 vertices :pt pt)       ;; Projected Vertices to Plane
        triangles      (geo.sphere.xform/partition-polygon projected)      ;; Triangle Partitions
        ;;_              (println (format "TRAIANGLES=%s" triangles))
        ;;av             (geo.sphere.xform/latlon->vector pv)
        ;;_              (println (format "PV=%s" pv))
        ]              ;; Test Point
    (loop [xs triangles
           acc 0]
      (if-let [x (first xs)]
        (recur (rest xs) (+ acc (if (within-sides? pv x) 1 0)))

        ;; If point intersects with odd number of triangles, inside otherwise outside.
        (odd? acc)))))




;;;;
;;;; Alt Impl
;;;;


(defn coor
  [poly x y]
  (nth (nth poly x) y))

(defn get-slope
  [x y i j poly]
  (let [a (- x (coor poly i 0))
        b (- (coor poly j 1) (coor poly i 1))
        c (- (coor poly j 0) (coor poly i 0))
        d (- y (coor poly i 1))]
    (float (- (* a b) (* c d)))))


(defn point-in-path?
  "Determine if point is in polygon"
  ;; IMPL: https://en.wikipedia.org/wiki/Even%E2%80%93odd_rule
  [x y poly]
  (let [n (count poly)]
    (loop [i 0
           j (dec n)
           c false]
      (if (< i n)
        (cond
          (and (= x (coor poly i 0)) (= y (coor poly i 1)))
            ;; point is corner
            true

          (not= (> (coor poly i 1) y) (> (coor poly j 1) y))
            (let [slope (get-slope x y i j poly)]
              (cond
                (= slope 0.0)
                  ;; bound is boundary
                  true
                (not= (< slope 0.0) (< (coor poly j 1) (coor poly i 1)))
                  (recur (inc i) i (not c))
                :else
                  (recur (inc i) i c)))
          :else
            (recur (inc i) i c))
        c))))

(def test-poly
  [[0 0]
   [1 1]
   [3 0]
   [5 2]
   [3 5]
   [2 2]
   [0 3]])

(def test-points
  [[false  1   0]
   [false  1  -2]
   [true   4   1]
   [true   3   2]
   [true   3   3]
   [true   1   2]
   [false  1   4]
   [false -1   2]
   [true   0   1]
   [true   5   2]
   [true   0   2]
   [true   2 0.5]
   [false  2 0.4]
   [false  4   4]])

#_(doseq [[expected x y] test-points]
  (println (format "(%s, %s), matched=%s" x y (= expected (point-in-path? x y test-poly)) )))


#_(defn vertices->projection-plane3
  [vertices & {:keys [pt]
               :or {pt nil}}]
  (println (format "INPUT: PT=%s, VERTICIES=%s" pt vertices))
  (let [uniq-verts (geo.util/ensure-unique-vertices vertices)
;;        cent       (geo.sphere.util/centroid uniq-verts)
        ;; Solves antipodal pt -> polygon edgecase projection issues; use augmented centroid for projection plane.
        cent       (geo.sphere.util/centroid (vector (geo.sphere.util/centroid uniq-verts) pt))
;;      _            (println (format "CENT=%s, CENT_WITH_PT=%s, UBER_CENT=%s" (geo.sphere.util/centroid uniq-verts) (geo.sphere.util/centroid (conj uniq-verts pt)) cent))

        ;; projection plane pt
        cv         (geo.sphere.xform/latlon->vector cent)

        ;; projection plane unit normal
        unit-nv    (geo.sphere.xform/unit-normal-vector cent)]

    (loop [xs  uniq-verts
           acc []]
      (if-let [x (first xs)]
        (let [av (mtx/matrix (geo.sphere.util/latlon->cartesian x))]
          (recur (rest xs) (conj acc (geo.sphere.xform/ortho-plane-projection av cv unit-nv))))
        (if pt
          [(geo.sphere.xform/ortho-plane-projection (mtx/matrix (geo.sphere.util/latlon->cartesian pt)) cv unit-nv) acc]
          [nil acc])))))

(def X 0)
(def Y 1)

#_(defn point-in-polygon?
  [pt vertices]
  (let [[pv pvtx]   (vertices->projection-plane3 vertices :pt pt)
        plane-norm  (geo.sphere.xform/polygon->normal pvtx)]  ;; normal vector of polygon projection plane
    (let [rot-mtx   (geo.sphere.xform/rotation-matrix plane-norm)]
      (loop [xs pvtx
             acc []]
        (if-let [x (first xs)]
          (let [vtx (mtx.op/* geo.const/earth-radius (geo.sphere.xform/rotate rot-mtx x))]
            #_(println (format "Z=%s" (.get vtx 2)))
            (recur (rest xs) (conj acc [(.get vtx X) (.get vtx Y)])))
          (let [_pt (mtx.op/* geo.const/earth-radius (geo.sphere.xform/rotate rot-mtx pv))]
            #_(println (format "POLY=%s, ROTATED PT=%s" acc _pt))
            (point-in-path? (.get _pt X) (.get _pt Y) acc)))))))
