(ns geo-yoots.sphere.transformations
  (:require [clojure.pprint :as pp]
            [geo-yoots.constants :as geo.consts]
            [geo-yoots.util.core :as geo.util]
            [geo-yoots.sphere.util :as geo.sphere.util]
            [clojure.core.matrix :as mtx]
            [clojure.core.matrix.operators :as mtx.op]))


(mtx/set-current-implementation :vectorz)



;; =============
;; -  Vectors  -
;; -------------

(defn latlon->vector
  [latlon]
  (mtx/matrix (geo.sphere.util/latlon->cartesian latlon)))

(defn a->b-vector
  "Create vector from points A & B"
  [av bv & {:keys [scale]
          :or {scale 1}}]
  (mtx.op/* scale (mtx.op/- bv av)))


;; ----------
;; - Normal -
;; ----------

;; http://mathworld.wolfram.com/SphericalCoordinates.html
;; http://mathworld.wolfram.com/NormalVector.html
(defn normal-vector
  "Calculates normal-vector.  For unit vector, `radius` is 1"
  [latlon & {:keys [radius]
              :or {radius geo.consts/earth-radius}}]
  ;; TODO: lat/lon directly to spherical???
  (let [cart  (geo.sphere.util/latlon->cartesian latlon)
        theta (geo.sphere.util/cartesian->theta cart)
        phi   (geo.sphere.util/cartesian->phi cart)]
    (mtx/matrix [(* radius (Math/cos theta) (Math/sin phi))
                 (* radius (Math/sin theta) (Math/sin phi))
                 (* radius (Math/cos phi))])))

(defn unit-normal-vector
  [latlon]
  (normal-vector latlon :radius 1))

(defn polygon->normal
  [vertices]
  (let [[a b c _] vertices]
    (mtx/cross (a->b-vector a b) (a->b-vector a c))))


;; -----------------------
;; - Distance to Surface -
;; -----------------------

(defn distance-to-plane
  "Distance of point to plane[n * (x - x0) = 0]"
  [pt plane normal]
  (/ (Math/abs (mtx/dot normal (a->b-vector plane pt))) (mtx/magnitude normal)))


;; ----------------------
;; - Vector Projections -
;; ----------------------

(defn ortho-plane-projection
  [av pv unit-normal]
  #_(println (format "ORTHO_PROJECTION: AV[%s], PV[%s], NORMAL[%s]" av pv unit-normal))
  (mtx.op/- av (mtx.op/* (mtx/dot (mtx.op/- av pv) unit-normal) unit-normal)))

(defn _vertices->projection-plane
  [pv unit-nv vertices]
  (loop [xs  vertices
         acc []]
    (if-let [x (first xs)]
      (let [av (mtx/matrix (geo.sphere.util/latlon->cartesian x))]
        (recur (rest xs) (conj acc (ortho-plane-projection av pv unit-nv))))
      acc)))

(defn vertices->projection-plane
  [vertices]
  (let [uniq-verts (geo.util/ensure-unique-vertices vertices)
        cent       (geo.sphere.util/centroid uniq-verts)]
    (_vertices->projection-plane
      (latlon->vector cent)      ;; projection plane pt
      (unit-normal-vector cent)  ;; projection plane unit normal
      uniq-verts)))


;; ----------------------
;; - Polygon Partitions -
;; ----------------------

(defn partition-polygon
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



;; =====================
;; -  Rotation Matrix  -
;; ---------------------


;; -----------
;; - Helpers -
;; -----------

(defn xx+yy
  [x y]
  (+ (geo.util/sq x) (geo.util/sq y)))

(defn sqrt-xx+yy+zz
  [x y z]
  (Math/sqrt (+ (geo.util/sq x) (geo.util/sq y) (geo.util/sq z))))

(defn a-div-xx+yy
  [a x y]
  (/ a (xx+yy x y)))

(defn a-div-sqrt-xx+yy+zz
  [a x y z]
  (/ a (sqrt-xx+yy+zz x y z)))


;; -----------
;; - Columns -
;; -----------

(def X 0)
(def Y 1)
(def Z 2)

(defn _00
  [x y z]
  (+
    (a-div-xx+yy (geo.util/sq y) x y)
    (a-div-sqrt-xx+yy+zz (* (- 1 (a-div-xx+yy (geo.util/sq y) x y)) z) x y z)))

(defn _01
  [x y z]
  (a-div-xx+yy (* -1 x y (- 1 (a-div-sqrt-xx+yy+zz z x y z))) x y))

(defn _02
  [x y z]
  (* -1 (a-div-sqrt-xx+yy+zz x x y z)))

(defn _10
  [x y z]
  (_01 x y z))

(defn _11
  [x y z]
  (+
    (a-div-xx+yy (geo.util/sq x) x y)
    (a-div-sqrt-xx+yy+zz (* (- 1 (a-div-xx+yy (geo.util/sq x) x y)) z) x y z)))

(defn _12
  [x y z]
  (* -1 (a-div-sqrt-xx+yy+zz y x y z)))

(defn _20
  [x y z]
  (a-div-sqrt-xx+yy+zz x x y z))

(defn _21
  [x y z]
  (a-div-sqrt-xx+yy+zz y x y z))

(defn _22
  [x y z]
  (a-div-sqrt-xx+yy+zz z x y z))



;; -----------------------
;; - Plane Rotation Matrix
;; -
;; - https://math.stackexchange.com/questions/1435018/change-a-3d-plane-to-xy-plane
;; ---

(defn rotation-matrix
  [norm]
  (try
    (let [x (.get norm X)
          y (.get norm Y)
          z (.get norm Z)]
      (mtx/matrix [[(_00 x y z) (_01 x y z) (_02 x y z)]
                   [(_10 x y z) (_11 x y z) (_12 x y z)]
                   [(_20 x y z) (_21 x y z) (_22 x y z)]]))
    (catch java.lang.ArithmeticException e
      ;; We're in translation plane already, use identity matrix - but we could probably test
      ;; for this condition and bypass altogether.
      (println (format "Error generating rotation matrix: %s; defaulting to identity matrix." e))
      (mtx/identity-matrix 3))))

;; rot-mtx[3 x 3] X pt-mtx[3 x 1] => 3 x 1
(defn rotate
  [rot-mtx pt]
  (mtx/inner-product rot-mtx pt))
