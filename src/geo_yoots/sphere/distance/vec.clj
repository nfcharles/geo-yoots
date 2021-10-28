(ns geo-yoots.sphere.distance.vec
  (:require [geo-yoots.constants :as geo.const]
            [geo-yoots.util.core :as geo.util]
            [geo-yoots.sphere.inclusion :as geo.sphere.incl]
            [geo-yoots.sphere.util :as geo.sphere.util]
            [geo-yoots.sphere.transformations :as geo.sphere.xform]
            [geo-yoots.sphere.rotation :as geo.sphere.rot]
            [clojure.core.matrix :as mtx]
            [clojure.core.matrix.stats :as mtx.stats]
            [clojure.core.matrix.operators :as mtx.op]))



(mtx/set-current-implementation :vectorz)


;; =============
;; -   Utils   -
;; -------------

(def X 0)
(def Y 1)
(def Z 2)


;; https://mathworld.wolfram.com/Point-LineDistance3-Dimensional.html



;;;; ==================
;;;; - 2 Dimensional  -
;;;; ------------------


(defn arc-angle
  "Returns angle between two vectors"
  [av bv]
  (Math/acos (mtx.stats/cosine-similarity av bv)))

(defn obtuse
  [av bv]
  (let [angle (arc-angle av bv)]
    #_(println (format "ANGLE=%s" angle))
    (> angle (/ Math/PI 2))))

(defn -min-distance-to-segment
  [pv u v]
  (/
    (mtx/magnitude (mtx/cross (mtx.op/- pv u) (mtx.op/- pv v)))
    (mtx/magnitude (mtx.op/- v u))))



;; ==================
;; Min Distance Impl
;; ------------------
;; 
;;  . p'  . p''   . p'''
;;   .   .       .
;;    . .       .
;;     . . . . .
;;     u       v
;;
;; case i.
;; ---
;;   * arc u -> v -> p''' has obtuse angle
;;     - distance is |p''' - v|
;;
;;
;; case ii.
;; ---
;;   * arc v -> u -> p'
;;     - distance is | p' - u|
;;
;;
;; case iii.
;; ---
;;   * arc v -> u -> p''
;;     - distance is `min-distance` to line u -> v
;;

(defn min-distance-to-segment
  [pv u v]
  (cond
    (obtuse (mtx.op/- u v) (mtx.op/- pv v))
      (mtx/magnitude (mtx.op/- pv v))

    (obtuse (mtx.op/- v u) (mtx.op/- pv u))
      (mtx/magnitude (mtx.op/- pv u))

    :else
      (-min-distance-to-segment pv u v)))


(defn _poly_distance2
  "Determine min distance to polygon"
  [vtxs n_rows n_cols]
  (let [pv (.getRow vtxs 0)] ;; input list is unique
    #_(println (format "PV=%s, VTXS=%s" pv vtxs))
    (loop [i 1
           j 2
           min-d Double/MAX_VALUE]
      (if (< i n_rows)
        (let [u (.getRow vtxs i)
              v (.getRow vtxs (if (>= j n_rows) 1 j)) ;; wrap j around; usually `mod` would suffice but first row is `pv`: TODO: remove from matrix

              dist  (min-distance-to-segment pv u v)]
          #_(println (format "DIST[%s][%s, %s]=%s" pv u v dist))
          (if (< dist min-d)
            (recur (inc i) (inc j) dist)
            (recur (inc i) (inc j) min-d)))
        min-d))))


(defn polygon->normal
  [vtxs]
  (let []
    (mtx/cross (geo.sphere.xform/a->b-vector (.getRow vtxs 1) (.getRow vtxs 2))
               (geo.sphere.xform/a->b-vector (.getRow vtxs 1) (.getRow vtxs 3)))))


(defn point-in-polygon2?
  "Returns true if point is in polygon, false otherwise"
  [pt vertices]
  (let [pvtxs     (geo.sphere.xform/vertices->projection-plane2 vertices :pt pt)        ;; Projected Vertices to Plane

        ;;_         (println (format "PVTXS_MATRIX=%s" pvtxs))
        ;;_         (println (format "PVTXS_MATRIX_VEC=%s" (.getRow pvtxs 0)))
        ;;_         (println (format "PV=%s" (.get pvtxs 0 0)))
        ;; TODO: should separate this functionality
        ;; prepare for projection plane rotation
        plane-norm     (polygon->normal pvtxs)
        rot-mtx        (geo.sphere.rot/xy-plane-rotation plane-norm)
        rotated        (mtx/transpose (mtx.op/* geo.const/earth-radius (geo.sphere.xform/rotate rot-mtx (mtx/transpose pvtxs))))

        ;;_         (println (format "ROTATED=%s" rotated))

        shape  (.getShape rotated)
        n_rows (aget shape 0)
        n_cols (aget shape 1)
        ;;_      (println (format "ROWS=%s, COLUMNS=%s" n_rows n_cols))


        ;; prepare for inclusion test
        triangles      (geo.sphere.xform/matrix-partition-polygon rotated n_rows n_cols)      ;; Triangle Partitions
        dist           (_poly_distance2 rotated n_rows n_cols)
        pv             (.getRow rotated 0)]                      ;; Test Point
    ;;(println (format "ANGLE_SUM=%s" angle-sum))
    (loop [xs triangles
           acc 0]
      (if-let [x (first xs)]
        (recur (rest xs) (+ acc (if (geo.sphere.incl/inside-bary? pv x) 1 0)))

        ;; If point intersects with odd number of triangles, inside otherwise outside.
        (let [signed (odd? acc)]
          (if signed
            (* -1.0 dist) dist))))))



;; =========================
;; -  Distances Functions  -
;; -------------------------


(defn to-point
  [])


(defn to-circle
  []
  )

(defn to-polyline
  [])

#_(defn to-polygon
  [pt vertices]
  (point-in-polygon? pt vertices))

(defn to-polygon
  [pt vertices]
  (point-in-polygon2? pt vertices))



