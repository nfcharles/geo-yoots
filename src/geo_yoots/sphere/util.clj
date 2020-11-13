(ns geo-yoots.sphere.util
  (:require [clojure.pprint :as pp]
            [geo-yoots.constants :as geo.consts]
            [geo-yoots.util.core :as geo.util]
            [geo-yoots.sphere.util :as geo.sphere.util]
            [clojure.core.matrix :as mtx]
            [clojure.core.matrix.operators :as mtx.op]))



(mtx/set-current-implementation :vectorz)

;; ====
;; - Lat/Lon coordinates to cartesian coordinates
;; ---
;;
;; Conversion:
;;   x = cos(λ)cos(φ)
;;   y = sin(λ)cos(φ)
;;   z = sin(φ)
;; where
;;   φ: latitude
;;   λ: longitude
;; ----

(defn latlon->x
  [[lat lon]]
  (* (Math/cos (Math/toRadians lat)) (Math/cos (Math/toRadians lon))))

(defn latlon->y
  [[lat lon]]
  (* (Math/cos (Math/toRadians lat)) (Math/sin (Math/toRadians lon))))

(defn latlon->z
  [[lat lon]]
  (Math/sin (Math/toRadians lat)))

(defn latlon->cartesian
  [coordinates]
  [(latlon->x coordinates) (latlon->y coordinates) (latlon->z coordinates)])


;; ===
;; - Cartesian coordinates to lat/lon coordinates
;; ---
;;
;; Conversion:
;;   λ: atan2(y,x)
;;   φ: atan2(z, sqrt(x^2 + y^2)
;; ---

(defn cartesian->lat
  [[x y z]]
  (Math/toDegrees (Math/atan2 z (Math/sqrt (+ (* x x) (* y y))))))

(defn cartesian->lon
  [[x y z]]
  (Math/toDegrees (Math/atan2 y x)))

(defn cartesian->latlon
  [cart]
  [(cartesian->lat cart) (cartesian->lon cart)])


;; ===
;; - Cartesian coordinates to Spherical Coordinates
;; ---
;; Formulas
;;  ρ: distance from origin to point (R)
;;  θ: angle between + x-axis and projection of ρ (r) onto xy plane
;;  φ: angle between + z-axis and line from origin to point (ρ) - 0 <= φ <= pi


(defn cartesian->rho
  [[x y z]]
  (Math/sqrt (+ (* x x) (* y y) (* z z))))

(defn cartesian->theta
  [[x y z]]
  (Math/atan2 y x))

(defn cartesian->phi
  [[x y z]]
  (Math/acos (/ z (cartesian->rho [x y z]))))

(defn cartesian->spherical
  [xyz]
  [(cartesian->rho xyz) (cartesian->theta xyz) (cartesian->phi xyz)])


;; ===
;; - Spherical Coordinates to Cartesian coordinates
;; ---

(defn spherical->x
  [[rho theta phi]]
  (* rho (Math/sin phi) (Math/cos theta)))

(defn spherical->y
  [[rho theta phi]]
  (* rho (Math/sin phi) (Math/sin theta)))

(defn spherical->z
  [[rho theta phi]]
  (* rho (Math/cos phi)))

(defn spherical->cartesian
  [r-theta-phi]
  [(spherical->x r-theta-phi) (spherical->y r-theta-phi) (spherical->z r-theta-phi)])


;; ===
;; - Centriod
;; ---
;;
;; Find centroid of set of (lat, lon) points
;; ---

(defn centroid
  [pts]
  (let [len (count pts)]
    (loop [xs pts
           x 0
           y 0
           z 0]
      (if-let [pt (first xs)]
        (recur (rest xs) (+ x (latlon->x pt)) (+ y (latlon->y pt)) (+ z (latlon->z pt)))
        (cartesian->latlon [(/ x len) (/ y len) (/ z len)])))))


;; ====
;; - Vectors
;; ====

(defn latlon->vector
  [latlon]
  (mtx/matrix (latlon->cartesian latlon)))

(defn a->b-vector
  "Create vector from points A & B"
  [av bv & {:keys [scale]
          :or {scale 1}}]
  (mtx.op/* scale (mtx.op/- bv av)))

;; -----
;; - Normals
;; -----

(defn normalize
  [a]
  (let [av (mtx/matrix a)]
    (mtx.op// av (mtx/magnitude av))))

;; http://mathworld.wolfram.com/SphericalCoordinates.html
;; http://mathworld.wolfram.com/NormalVector.html
(defn normal-vector
  "Calculates normal-vector.  For unit vector, `radius` is 1"
  [latlon & {:keys [radius]
              :or {radius geo.consts/earth-radius}}]
  ;; TODO: lat/lon directly to spherical???
  (let [cart  (latlon->cartesian latlon)
        theta (cartesian->theta cart)
        phi   (cartesian->phi cart)]
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

;; ---
;; - Distance
;; ---

(defn distance-to-plane
  "Distance of point to plane[n * (x - x0) = 0]"
  [pt plane normal]
  (/ (Math/abs (mtx/dot normal (a->b-vector plane pt))) (mtx/magnitude normal)))


;; ---
;; - Vector Projections
;; ---

(defn ortho-plane-projection
  [av pv unit-normal]
  #_(println (format "ORTHO_PROJECTION: AV[%s], PV[%s], NORMAL[%s]" av pv unit-normal))
  (mtx.op/- av (mtx.op/* (mtx/dot (mtx.op/- av pv) unit-normal) unit-normal)))

(defn _vertices->projection-plane
  [pv unit-nv vertices]
  (loop [xs  vertices
         acc []]
    (if-let [x (first xs)]
      (let [av (mtx/matrix (latlon->cartesian x))]
        (recur (rest xs) (conj acc (ortho-plane-projection av pv unit-nv))))
      acc)))

(defn vertices->projection-plane
  [vertices]
  (let [uniq-verts (geo.util/ensure-unique-vertices vertices)
        cent       (centroid uniq-verts)]
    (_vertices->projection-plane
      (latlon->vector cent)     ;; projection plane pt
      (unit-normal-vector cent) ;; projection plane unit normal
      uniq-verts)))


;; ---
;; - Polygon Partitions
;; ---

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
