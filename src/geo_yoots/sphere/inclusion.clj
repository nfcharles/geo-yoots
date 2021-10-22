(ns geo-yoots.sphere.inclusion
  (:require [clojure.pprint :as pp]
            [geo-yoots.constants :as geo.const]
            [geo-yoots.util.core :as geo.util]
            [geo-yoots.sphere.util :as geo.sphere.util]
            [geo-yoots.sphere.transformations :as geo.sphere.xform]
            [clojure.core.matrix :as mtx]
            [clojure.core.matrix.operators :as mtx.op]))


(def X 0)
(def Y 1)
(def Z 2)

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

;;
;; New Projection Method
;;

(defn vertices->projection-plane3
  [vertices & {:keys [pt] ;; (lat,lon)
               :or {pt nil}}]
  (let [uniq-verts (geo.util/ensure-unique-vertices vertices)
        ;; Solves antipodal pt -> polygon edgecase projection issues; use augmented centroid for projection plane.
        cent       (geo.sphere.util/centroid (vector (geo.sphere.util/centroid uniq-verts) pt))

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
;; -   * https://blackpawn.com/texts/pointinpoly/
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


;; ----------------------
;; - Barycentric
;; ---

(defn inside-bary?
  [pt [a b c]]
  (let [v0    (mtx.op/- c a)
        v1    (mtx.op/- b a)
        v2    (mtx.op/- pt a)
        dot00 (mtx/dot v0 v0)
        dot01 (mtx/dot v0 v1)
        dot02 (mtx/dot v0 v2)
        dot11 (mtx/dot v1 v1)
        dot12 (mtx/dot v1 v2)
        invdm (/ 1 (- (* dot00 dot11) (* dot01 dot01)))
        u     (float (* (- (* dot11 dot02) (* dot01 dot12)) invdm))
        v     (float (* (- (* dot00 dot12) (* dot01 dot02)) invdm))]
    (and (>= u 0.0) (>= v 0.0) (< (+ u v) 1.0))))



;; -----------------------
;; - Angle Sum
;; ---

(def epsilon                  0.0000001)
(def twopi   6.283185307179586476925287)
(def rtod                    57.2957795)

(defn dist
  [x y z]
  (let []
    (Math/sqrt (+ (* x x) (* y y) (* z z)))))

(defn angle-sum
  [q pts]
  ;; For Convex Polygons
  ;; http://www.eecs.umich.edu/courses/eecs380/HANDOUTS/PROJ2/InsidePoly.html
  (let [n (count pts)]
    (loop [i          0
           anglesum   0
           costheta nil]
      (if (< i n)
        (let [p1-x (- (.get (nth pts i) X) (.get q X))
              p1-y (- (.get (nth pts i) Y) (.get q Y))
              p1-z (- (.get (nth pts i) Z) (.get q Z))
              p2-x (- (.get (nth pts (mod (+ i 1) n)) X) (.get q X))
              p2-y (- (.get (nth pts (mod (+ i 1) n)) Y) (.get q Y))
              p2-z (- (.get (nth pts (mod (+ i 1) n)) Z) (.get q Z))
              d1   (dist p1-x p1-y p1-z)
              d2   (dist p2-x p2-y p2-z)]
          (if (<= (* d1 d2) epsilon)
            twopi
            (let [costheta (/ (+ (* p1-x p2-x) (* p1-y p2-y) (* p1-z p2-z)) (* d1 d2))]
              (recur (inc i) (+ anglesum (Math/acos costheta)) costheta))))
        anglesum))))


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


#_(defn point-in-polygon?
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


(defn point-in-polygon?
  "Returns true if point is in polygon, false otherwise"
  [pt vertices]
  (let [[pv projected] (vertices->projection-plane3 vertices :pt pt)       ;; Projected Vertices to Plane
        ;;angle-sum      (angle-sum pv projected)
        triangles      (geo.sphere.xform/partition-polygon projected)      ;; Triangle Partitions
        ]              ;; Test Point
    ;;(println (format "ANGLE_SUM=%s" angle-sum))
    (loop [xs triangles
           acc 0]
      (if-let [x (first xs)]
        (recur (rest xs) (+ acc (if (inside-bary? pv x) 1 0)))

        ;; If point intersects with odd number of triangles, inside otherwise outside.
        (odd? acc)))))

#_(defn point-in-polygon?
  "Returns true if point is in polygon, false otherwise"
  [pt vertices]
  (let [[pv projected] (vertices->projection-plane3 vertices :pt pt)  ;; Projected Vertices to Plane
        angle-sum      (angle-sum pv projected)]
    (println (format "ANGLE_SUM=(%s, %s)" twopi angle-sum))
    (<= (Math/abs (- twopi angle-sum)) epsilon)))




;;;; =================
;;;; - Alt Impl
;;;; -----------------


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



#_(doseq [[expected x y] test-points]
  (println (format "(%s, %s), matched=%s" x y (= expected (point-in-path? x y test-poly)) )))


;; Slow; needs rotation matrix to hold z-axis constant (3d-2d) to work.
;; Can we find 3d variant??

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
