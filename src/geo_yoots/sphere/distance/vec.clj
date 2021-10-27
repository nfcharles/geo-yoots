(ns geo-yoots.sphere.distance.vec
  (:require [geo-yoots.constants :as geo.const]
            [geo-yoots.util.core :as geo.util]
            [geo-yoots.sphere.inclusion :as geo.sphere.incl]
            [geo-yoots.sphere.util :as geo.sphere.util]
            [geo-yoots.sphere.transformations :as geo.sphere.xform]
            [clojure.core.matrix :as mtx]
            [clojure.core.matrix.stats :as mtx.stats]
            [clojure.core.matrix.operators :as mtx.op]))





;; =============
;; -   Utils   -
;; -------------

(def X 0)
(def Y 1)
(def Z 2)


(defn normal-vector-max-area-coordinates
  [norm]
  (loop [xs       [[X Y] [Y Z] [X Z]]
         min-area Integer/MIN_VALUE
         acc      nil]
    (if-let [coor (first xs)]
      (let [[_0 _1] coor
            area    (Math/abs (* (.get norm _0) (.get norm _1)))]
        (if (> area min-area)
          (recur (rest xs) area coor)
          (recur (rest xs) min-area acc)))
      acc)))



;; https://mathworld.wolfram.com/Point-LineDistance3-Dimensional.html
(defn min-distance-to-segment
  [pt u v]
  ;; 3D: norm(cross(x2-x1 , x1-x0)) / norm(x2-x1)
  (/
    (mtx/magnitude (mtx/cross (mtx.op/- v u) (mtx.op/- u pt)))
    (mtx/magnitude (mtx.op/- v u))))


(defn min-distance-to-segment
  [pt u v]
  ;; 3D: norm(cross(x2-x1 , x1-x0)) / norm(x2-x1)
  (/
    (mtx/magnitude (mtx/cross (mtx.op/- pt u) (mtx.op/- pt v)))
    (mtx/magnitude (mtx.op/- v u))))


(defn _poly_distance
  "Determine min distance to polygon"
  [pv pvtxs]
  (println (format "PV=%s, VTXS=%s" pv pvtxs))
  (let [a (first pvtxs)] ;; input list is unique
    (loop [xs    (partition 2 1 (conj pvtxs a))
           min-d Double/MAX_VALUE]
      (if-let [edge (first xs)]
        (let [[u v] edge
              ;;_     (println (format "EDGE=%s" (seq edge)))
              ;;dist  (* geo.const/earth-radius (min-distance-to-segment pv u v))
              dist  (min-distance-to-segment pv u v)]
          (println (format "DIST[%s]=%s" pv dist))
          (if (< dist min-d)
            (recur (rest xs) dist)
            (recur (rest xs) min-d)))
        min-d))))


(defn vertices->projection-plane
  [vertices & {:keys [pt] ;; (lat,lon)
               :or {pt nil}}]
  (let [uniq-verts (geo.util/ensure-unique-vertices vertices)
        ;; Solves antipodal pt -> polygon edgecase projection issues; use augmented centroid for projection plane.
        cent       (geo.sphere.util/centroid (vector (geo.sphere.util/centroid uniq-verts) pt))
        ;;cent       (geo.sphere.util/centroid uniq-verts)

        ;; projection plane pt
        cv         (geo.sphere.xform/latlon->vector cent)

        ;; projection plane unit normal
        unit-nv    (geo.sphere.xform/unit-normal-vector cent)

        
        ;;max-area   (normal-vector-max-area-coordinates unit-nv)
        ]

    #_(println (format "UNIT_NORMAL_MAX(%s, %s)" (first max-area) (last max-area)))
    (loop [xs  uniq-verts
           acc []]
      (if-let [x (first xs)]
        (let [av (geo.sphere.xform/latlon->vector x)]
          (recur (rest xs) (conj acc (mtx.op/* geo.const/earth-radius (geo.sphere.xform/ortho-plane-projection av cv unit-nv)))))
        (if pt ;; (mtx/matrix (geo.sphere.util/latlon->cartesian pt))
          [(mtx.op/* geo.const/earth-radius (geo.sphere.xform/ortho-plane-projection (geo.sphere.xform/latlon->vector pt) cv unit-nv)) acc]
          [nil acc])))))


(defn point-in-polygon?
  "Returns true if point is in polygon, false otherwise"
  [pt vertices]
  (let [[pv projected] (vertices->projection-plane vertices :pt pt)        ;; Projected Vertices to Plane
        ;;angle-sum      (angle-sum pv projected)
        triangles      (geo.sphere.xform/partition-polygon projected)      ;; Triangle Partitions
        dist           (_poly_distance pv projected)]                      ;; Test Point
    ;;(println (format "ANGLE_SUM=%s" angle-sum))
    (loop [xs triangles
           acc 0]
      (if-let [x (first xs)]
        (recur (rest xs) (+ acc (if (geo.sphere.incl/inside-bary? pv x) 1 0)))

        ;; If point intersects with odd number of triangles, inside otherwise outside.
        (let [signed (odd? acc)]
          (if signed
            (* -1.0 dist) dist))))))




;;;; ==================
;;;; - 2 Dimensional  -
;;;; ------------------

;; 2d: https://mathworld.wolfram.com/Point-LineDistance2-Dimensional.html

#_(defn min-distance-to-segment2
  [pt u v]
  (/
    (mtx/det (mtx.op/- v u) (mtx.op/- u pt))
    (mtx/magnitude (mtx.op/- v u))))



(defn arc-angle
  "Returns angle between two vectors"
  [av bv]
  (Math/acos (mtx.stats/cosine-similarity av bv)))

(defn obtuse
  [av bv]
  (let [angle (Math/toDegrees (arc-angle av bv))]
    #_(println (format "ANGLE=%s" angle))
    (> angle 90.0)))

(defn -min-distance-to-segment
  [pv u v]
  ;; 3D: norm(cross(x2-x1 , x1-x0)) / norm(x2-x1)
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



(defn vertices->projection-plane2
  [pt vertices]
  (let [uniq-verts (geo.util/ensure-unique-vertices vertices)
        ;; Solves antipodal pt -> polygon edgecase projection issues; use augmented centroid for projection plane.
        cent       (geo.sphere.util/centroid (vector (geo.sphere.util/centroid uniq-verts) pt))
        ;;cent       (geo.sphere.util/centroid uniq-verts)

        ;; projection plane pt
        cv         (geo.sphere.xform/latlon->vector cent)

        ;; projection plane unit normal
        unit-nv    (geo.sphere.xform/unit-normal-vector cent)

        
        ;;max-area   (normal-vector-max-area-coordinates unit-nv)
        ]

    (loop [xs  uniq-verts
           acc [(geo.sphere.xform/ortho-plane-projection (geo.sphere.xform/latlon->vector pt) cv unit-nv)]]
      (if-let [x (first xs)]
        (let [xv (geo.sphere.xform/latlon->vector x)]
          (recur (rest xs) (conj acc (geo.sphere.xform/ortho-plane-projection xv cv unit-nv))))
        (mtx/matrix acc)))))


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

(defn partition-polygon
  "Generates triangular partitions for a polygon"
  [vtxs n_rows n_cols]
  (let [anchor (.getRow vtxs 1)]
    (loop [i   2
           tri [anchor]
           acc []]
      (if (< i n_rows)
        (if (= (count tri) 2)
          (recur i [anchor] (conj acc (conj tri (.getRow vtxs i))))
          (recur (inc i) (conj tri (.getRow vtxs i)) acc))
        acc))))


(defn point-in-polygon2?
  "Returns true if point is in polygon, false otherwise"
  [pt vertices]
  (let [pvtxs     (vertices->projection-plane2 pt vertices)        ;; Projected Vertices to Plane

        ;;_         (println (format "PVTXS_MATRIX=%s" pvtxs))
        ;;_         (println (format "PVTXS_MATRIX_VEC=%s" (.getRow pvtxs 0)))
        ;;_         (println (format "PV=%s" (.get pvtxs 0 0)))
        ;; TODO: should separate this functionality
        ;; prepare for projection plane rotation
        plane-norm     (polygon->normal pvtxs)
        rot-mtx        (geo.sphere.xform/rotation-matrix plane-norm)
        rotated        (mtx/transpose
                         (mtx.op/* geo.const/earth-radius (geo.sphere.xform/rotate rot-mtx (mtx/transpose pvtxs))))

        ;;_         (println (format "ROTATED=%s" rotated))

        shape  (.getShape rotated)
        n_rows (aget shape 0)
        n_cols (aget shape 1)
        ;;_      (println (format "ROWS=%s, COLUMNS=%s" n_rows n_cols))


        ;; prepare for inclusion test
        triangles      (partition-polygon rotated n_rows n_cols)      ;; Triangle Partitions
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

(defn to-polygon
  [pt vertices]
  (point-in-polygon? pt vertices))

(defn to-polygon
  [pt vertices]
  (point-in-polygon2? pt vertices))



