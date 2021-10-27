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
        unit-nv    (geo.sphere.xform/unit-normal-vector cent)

        
        max-area   (normal-vector-max-area-coordinates unit-nv)]

    #_(println (format "UNIT_NORMAL_MAX(%s, %s)" (first max-area) (last max-area)))
    (loop [xs  uniq-verts
           acc []]
      (if-let [x (first xs)]
        (let [av (geo.sphere.xform/latlon->vector x)]
          (recur (rest xs) (conj acc (geo.sphere.xform/ortho-plane-projection av cv unit-nv))))
        (if pt ;; (mtx/matrix (geo.sphere.util/latlon->cartesian pt))
          [(geo.sphere.xform/ortho-plane-projection (geo.sphere.xform/latlon->vector pt) cv unit-nv) acc max-area]
          [nil acc max-area])))))


(defn vertices->projection-plane4
  [vertices & {:keys [pt] ;; (lat,lon)
               :or {pt nil}}]
  (let [uniq-verts (geo.util/ensure-unique-vertices vertices)
        ;; Solves antipodal pt -> polygon edgecase projection issues; use augmented centroid for projection plane.
        cent       (geo.sphere.util/centroid (vector (geo.sphere.util/centroid uniq-verts) pt))

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



;;; ===============
;;; - Drivers
;;; ----



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
  (let [[pv projected] (vertices->projection-plane4 vertices :pt pt)       ;; Projected Vertices to Plane
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

(defn point-in-polygon?
  "Returns true if point is in polygon, false otherwise"
  [pt vertices]
  (let [[pv projected] (vertices->projection-plane4 vertices :pt pt)       ;; Projected Vertices to Plane
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


#_(defn point-in-polygon?
  "Crossings impl"
  [pt vertices]
  (let [[pv pvtx [_1 _2]] (vertices->projection-plane3 vertices :pt pt)]
    ;; _1 & _2 are the plane normal coordinates that maximize plane area; this
    ;; is the conversion from 3d to 2d to in order to use the `point-in-path` algo.
    (loop [xs  pvtx
           acc []]
      (if-let [vtx (first xs)]
        ;; TODO: We can optimize the double iteration by encoding the (_1, _2) coordinates
        ;; in the `point-in-path` implementation above.  This will ensure we only do 1 iteration
        (recur (rest xs) (conj acc [(.get vtx _1) (.get vtx _2)]))
        (point-in-path? (.get pv _1) (.get pv _2) acc)))))





;;;; ========================
;;;; - Alt Implementations
;;;; ------------------------


#_(defn coor
  [poly vertex-idx coordinate]
  (.get (nth poly vertex-idx) coordinate))

#_(defn get-slope
  [x y i j poly _0 _1]
  (let [a (- x (coor poly i _0))
        b (- (coor poly j _1) (coor poly i _1))
        c (- (coor poly j _0) (coor poly i _0))
        d (- y (coor poly i _1))]
    (float (- (* a b) (* c d)))))

#_(defn point-in-path?
  "Determine if point is in polygon"
  ;; IMPL: https://en.wikipedia.org/wiki/Even%E2%80%93odd_rule
  [x y poly _0 _1]
  (let [n (count poly)]
    (loop [i 0
           j (dec n)
           c false]
      (if (< i n)
        (cond
          (and (= x (coor poly i _0)) (= y (coor poly i _1)))
            ;; point is corner
            true

          (not= (> (coor poly i _1) y) (> (coor poly j _1) y))
            (let [slope (get-slope x y i j poly _0 _1)]
              (cond
                (= slope 0.0)
                  ;; bound is boundary
                  true
                (not= (< slope 0.0) (< (coor poly j _1) (coor poly i _1)))
                  (recur (inc i) i (not c))
                :else
                  (recur (inc i) i c)))
          :else
            (recur (inc i) i c))
        c))))

#_(defn point-in-polygon?
  "Crossings impl"
  [pt vertices]
  (let [[pv pvtx [_0 _1]] (vertices->projection-plane3 vertices :pt pt)]
    ;; _1 & _2 are the plane normal coordinates that maximize plane area; this
    ;; is the conversion from 3d to 2d to in order to use the `point-in-path` algo.
    (point-in-path? (.get pv _0) (.get pv _1) pvtx _0 _1)

    #_(loop [xs  pvtx
           acc []]
      (if-let [vtx (first xs)]
        ;; TODO: We can optimize the double iteration by encoding the (_1, _2) coordinates
        ;; in the `point-in-path` implementation above.  This will ensure we only do 1 iteration
        (recur (rest xs) (conj acc [(.get vtx _1) (.get vtx _2)]))
        (point-in-path? (.get pv _1) (.get pv _2) acc)))))



#_(doseq [[expected x y] test-points]
  (println (format "(%s, %s), matched=%s" x y (= expected (point-in-path? x y test-poly)) )))


;; Slow; needs rotation matrix to hold z-axis constant (3d-2d) to work.
;; Can we find 3d variant?? DEPRECATED

#_(defn point-in-polygon?
  [pt vertices]
  (let [[pv pvtx _]   (vertices->projection-plane3 vertices :pt pt)

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



#_(def EPSILON 0.000001)

#_(defn get-x1_x2
  [vertices i n _0]
  (let [x1 (coor vertices i _0)
        x2 (coor vertices (mod (inc i) n) _0)]
    (if (< x1 x2 )
      [x1 x2]
      [x2 x1])))


#_(defn jordan-curve
  [px py vertices _0 _1] ;; _0:x, _1:y
  (let [n  (count vertices)]
    (loop [i 0
           crossings 0]
      (if (< i n)
        (let [[x1 x2] (get-x1_x2 vertices i n _0)]
          (if (and
                (>  px x1)
                (<= px x2)
                (or
                  (<  py (coor vertices i _1))
                  (<= py (coor vertices (mod (inc i) n) _1))))
            (let [dx (- (coor vertices (mod (inc i) n) _0) (coor vertices i _0))
                  dy (- (coor vertices (mod (inc i) n) _1) (coor vertices i _1))
                  k  (if (< (Math/abs dx) EPSILON) Double/POSITIVE_INFINITY (/ dy (* 1.0 dx)))
                  m  (- (coor vertices i _1) (* k (coor vertices i _0)))
                  y2 (+ (* k px) m) #_(* k (+ px m))]
              (if (<= py y2)
                (recur (inc n) (inc crossings))
                (recur (inc n) crossings)))
            (recur (inc n) crossings)))
      (let [ret (odd? crossings)]
       (println (format "CROSSINGS=%s" crossings))
        ret)))))



#_(defn point-in-polygon?
  "Crossings impl"
  [pt vertices]
  (let [[pv pvtx [_0 _1]] (vertices->projection-plane3 vertices :pt pt)]
    ;; _1 & _2 are the plane normal coordinates that maximize plane area; this
    ;; is the conversion from 3d to 2d to in order to use the `point-in-path` algo.
    (jordan-curve (.get pv _0) (.get pv _1) pvtx _0 _1)


    #_(loop [xs  pvtx
           acc []]
      (if-let [vtx (first xs)]
        ;; TODO: We can optimize the double iteration by encoding the (_1, _2) coordinates
        ;; in the `point-in-path` implementation above.  This will ensure we only do 1 iteration
        (recur (rest xs) (conj acc [(.get vtx _1) (.get vtx _2)]))
        (point-in-path? (.get pv _1) (.get pv _2) acc)))))



;; ------------


#_(defn jordan-curve
  ;; https://wrf.ecse.rpi.edu/Research/Short_Notes/pnpoly.html#The%20C%20Code
  [pv pvtxs _0 _1]
  (let [n  (count pvtxs)
        px (.get pv _0)
        py (.get pv _1)]
    (loop [i 0
           j (dec n)
           c  false]
      (if (< i n)
        (if (and
              (not= (> (coor pvtxs i _1) py) (> (coor pvtxs j _1) py))
              (< px
                 (-> (- (coor pvtxs j _0) (coor pvtxs i _0))
                     (* (- py (coor pvtxs i _1)))
                     (/ (- (coor pvtxs j _1) (coor pvtxs i _1)))
                     (+ (coor pvtxs i _0)))))
          (do
            #_(println (format "jordan-curve: i[true]=%s" i))
            (recur (inc i) i (not c)))
          (do
            #_(println (format "jordan-curve: i[false]=%s" i))
            (recur (inc i) i c)))
        (do
          #_(println "Done jordan-curve")
          c)))))

#_(defn point-in-polygon?
  "Crossings impl"
  [pt vertices]
  (let [[pv pvtx [_0 _1]] (vertices->projection-plane3 vertices :pt pt)]
    ;; _1 & _2 are the plane normal coordinates that maximize plane area; this
    ;; is the conversion from 3d to 2d to in order to use the `point-in-path` algo.
    (jordan-curve pv pvtx _0 _1)

    #_(loop [xs  pvtx
           acc []]
      (if-let [vtx (first xs)]
        ;; TODO: We can optimize the double iteration by encoding the (_1, _2) coordinates
        ;; in the `point-in-path` implementation above.  This will ensure we only do 1 iteration
        (recur (rest xs) (conj acc [(.get vtx _1) (.get vtx _2)]))
        (point-in-path? (.get pv _1) (.get pv _2) acc)))))
