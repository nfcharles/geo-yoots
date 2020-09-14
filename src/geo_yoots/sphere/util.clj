(ns geo-yoots.sphere.util
  (:require [clojure.pprint :as pp]
            [geo-yoots.constants :as geo.consts]
            [clojure.math.combinatorics :as comb]
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

(defn to-x
  [[lat lon]]
  (* (Math/cos (Math/toRadians lat)) (Math/cos (Math/toRadians lon))))

(defn to-y
  [[lat lon]]
  (* (Math/cos (Math/toRadians lat)) (Math/sin (Math/toRadians lon))))

(defn to-z
  [[lat lon]]
  (Math/sin (Math/toRadians lat)))

(defn to-cartesian
  [coordinates]
  [(to-x coordinates) (to-y coordinates) (to-z coordinates)])


;; ===
;; - Cartesian coordinates to lat/lon coordinates
;; ---
;;
;; Conversion:
;;   λ: atan2(y,x)
;;   φ: atan2(z, sqrt(x^2 + y^2)
;; ---

(defn to-lat
  [[x y z]]
  (Math/toDegrees (Math/atan2 z (Math/sqrt (+ (* x x) (* y y))))))

(defn to-lat-alt
  [[x y z] & {:keys [radius]
              :or {radius geo.consts/earth-radius}}]
  (Math/toDegrees (Math/sin (/ z radius))))

(defn to-lon
  [[x y z]]
  (Math/toDegrees (Math/atan2 y x)))




;; ===
;; - Spherical Coordinates
;; ---
;; Formulas
;;  ρ: distance from origin to point (R)
;;  θ: angle between + x-axis and projection of ρ (r) onto xy plane
;;  φ: angle between + z-axis and line from origin to point (ρ) - 0 <= φ <= pi


(defn to-rho
  [[x y z]]
  (Math/sqrt (+ (* x x) (* y y) (* z z))))

(defn to-theta
  [[x y z]]
  (Math/atan2 y x))

(defn to-phi
  [[x y z]]
  (Math/acos (/ z (Math/sqrt (+ (* x x) (* y y) (* z z))))))


;; ===
;; - Centriod
;; ---
;;
;; Find centroid of set of (lat, lon) points
;; ---

(defn centroid
  [points]
  (let [len (count points)]
    (loop [xs points
           x 0
           y 0
           z 0]
      (if-let [pt (first xs)]
        (recur (rest xs) (+ x (to-x pt)) (+ y (to-y pt)) (+ z (to-z pt)))
        (let [x-avg (/ x len)
              y-avg (/ y len)
              z-avg (/ z len)]
          [(to-lat [x-avg y-avg z-avg]) (to-lon [x-avg y-avg z-avg])])))))


;; ====
;; - Vectors
;; ====

(defn normalize
  [a]
  (let [av (mtx/matrix a)]
    (mtx.op// av (mtx/magnitude av))))

#_(defn unit-vector
  [lat-lon]
  (let [cart  (to-cartesian lat-lon)
        theta (to-theta cart)
        phi   (to-phi cart)]
    (mtx/matrix [(* (Math/cos theta) (Math/sin phi))
                 (* (Math/sin theta) (Math/sin phi))
                 (Math/cos phi)])))

(defn normal-vector
  "Calculates normal-vector.  For unit vector, `radius` is 1"
  [lat-lon & {:keys [radius]
              :or {radius geo.consts/earth-radius}}]
  ;; TODO: lat/lon directly to spherical???
  (let [cart  (to-cartesian lat-lon)
        theta (to-theta cart)
        phi   (to-phi cart)]
    (mtx/matrix [(* radius (Math/cos theta) (Math/sin phi))
                 (* radius (Math/sin theta) (Math/sin phi))
                 (* radius (Math/cos phi))])))

(defn unit-normal-vector
  [lat-lon]
  (normal-vector lat-lon :radius 1))

(defn point->plane-distance
  "Distance of point to plane[n * (x -x0) = 0]"
  [normal-v v]
  ;; normal-v & v are proper vectors
  (/ (Math/abs (mtx/dot normal-v v)) (mtx/magnitude normal-v)))


;; ---
;; - Raw Vector
;; ---

(defn a->b-vector
  "Create vector from points A & B"
  [a b & {:keys [scale]
          :or {scale 1}}]
  ;; a & b are raw vectors
  (let [av (mtx/matrix a)
        bv (mtx/matrix b)]
    (mtx.op/* scale (mtx.op/- bv av))))

(defn ortho-plane-projection
  [a p-plane unit-normal-v]
  (let [av (mtx/matrix a)
        pv (mtx/matrix p-plane)]
    (mtx.op/- av (mtx.op/* (mtx/dot (mtx.op/- av pv) unit-normal-v) unit-normal-v))))


;; ---
;; - Vectorized
;; ---

(defn a->b-vector2
  "Create vector from points A & B"
  [av bv & {:keys [scale]
          :or {scale 1}}]
  (mtx.op/* scale (mtx.op/- bv av)))

(defn ortho-plane-projection2
  [av pv unit-normal-v]
  #_(println (format "ORTHO_PROJECTION: AV[%s], PV[%s], NORMAL[%s]" av pv unit-normal-v))
  (mtx.op/- av (mtx.op/* (mtx/dot (mtx.op/- av pv) unit-normal-v) unit-normal-v)))



;;; ===
;;; - Bearing
;;; ---
;;;
;;; Formula:	θ = atan2( sin Δλ ⋅ cos φ2 , cos φ1 ⋅ sin φ2 − sin φ1 ⋅ cos φ2 ⋅ cos Δλ )
;;;   where:
;;;     φ1,λ1 is the start point, φ2,λ2 the end point (Δλ is the difference in longitude)
;;; ---

(defn bearing
  [[lat1 lon1] [lat2 lon2]]
  (let [dl    (Math/toRadians (- lon2 lon1))
        lat1r (Math/toRadians lat1)
        lat2r (Math/toRadians lat2)
        y     (* (Math/sin dl) (Math/cos lat2r))
        x     (- (* (Math/cos lat1r) (Math/sin lat2r))
                 (* (Math/sin lat1r) (Math/cos lat2r) (Math/cos dl)))]
    #_(println (format "y=%s, x=%s, dl=%s" y x dl))
    (Math/atan2 y x)))

(defn normalized-bearing
  "Normalize to compass bearing (0 - 360)"
  [p1 p2]
  (let [br (bearing p1 p2)]
    #_(println (format "BEARING=%s" br))
    (-> (Math/toDegrees br)
        (+ 360)
        (mod 360))))

(defn relative-bearing
  [b13 b12]
  (let [diff (Math/abs (- b13 b12))]
    (if (> diff Math/PI)
      (- (* 2 Math/PI) diff) diff)))


;; =============
;; - Triangles -
;; =============

;; ===
;; - Sum
;; ---

(defn triangle-angle-sum
  "Measures angle sum of triangle"
  [fractional-area-coverage]
  ;; fractional-area-coverage: fraction of the sphere's surface that is enclosed by the triangle
  (* 180 (+ 1 (* 4 fractional-area-coverage))))



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
  (let [ab (a->b-vector2 a b)]
    (>= (mtx/dot
          (mtx/cross ab (a->b-vector2 a p1))
          (mtx/cross ab (a->b-vector2 a p2)))
	0)))

(defn same-side-algo
  [pt [a b c]]
  #_(println (format "PT=%s, A=%s, B=%s, C=%s" pt a b c))
  (and
    (same-side pt a b c)
    (same-side pt b a c)
    (same-side pt c a b)))

#_(defn xform-vertices
  [pv unit-normal-vector vertices]
  (loop [xs  vertices
         acc []]
    (if-let [x (first xs)]
      (let [av (mtx/matrix (to-cartesian x))]
        (recur (rest xs) (conj acc {#_:cart #_av :proj (ortho-plane-projection2 av pv unit-normal-vector)})))
      acc)))

(defn xform-vertices
  [pv unit-normal-vector vertices]
  (loop [xs  vertices
         acc []]
    (if-let [x (first xs)]
      (let [av (mtx/matrix (to-cartesian x))]
        (recur (rest xs) (conj acc (ortho-plane-projection2 av pv unit-normal-vector))))
      acc)))

(defn select-projections
  [vertices]
  (loop [xs vertices
         acc []]
    (if-let [x (first xs)]
      (recur (rest xs) (conj acc (:proj x)))
      acc)))

(defn select-triangle-group
  "Select trangle fan group by anhor vertex.  Each vertex has it's own
   set of associated triangles.  Only one group is sufficient for point
   inclusion tests"
  [plane unit-normal-vector vertices & {:keys [idx]
                                        :or {idx 0}}]
  (let [superset (comb/combinations vertices 3)]
    #_(pp/pprint superset)
    (loop [xs superset
           acc []]
      (if-let [tri (first xs)]
        (let [anchor (first tri)]
          (if (= (:n anchor) idx)
            ;; Use orthogonal projection
            (do
              #_(println (format "Anchor[%s] Match: %s = %s" anchor (:n anchor) idx))
              (recur (rest xs) (conj acc (select-projections tri))))

            ;; Different anhor, don't include
            (do
              #_(println (format "Anchor[%s] No Match: %s = %s" anchor (:n anchor) idx))
              (recur (rest xs) acc))))
        acc))))

(defn generate-triangle-fans
  "Generates triangle partitions for a polygon"
  [vertices]
  (let [anchor (first vertices)]
    (loop [xs  (rest (drop-last vertices))
           tri [anchor]
           acc []]
      (if-let [x (first xs)]
        (if (= (count tri) 2)
          (do
            #_(println (format "TF: Remaing Vertices: %s" xs))
            (recur xs [anchor] (conj acc (conj tri x))))
          (recur (rest xs) (conj tri x) acc))
        acc))))


(defn point-in-polygon-ssa?
  "Returns true if point is in polygon, false otherwise"
  [pt plane vertices]
  (let [unit-nv    (unit-normal-vector plane)                   ;; Find unit normal vector
        pv         (mtx/matrix (to-cartesian plane))            ;; Plane
        av         (mtx/matrix (to-cartesian pt))               ;; Point
        xformed    (xform-vertices pv unit-nv vertices)         ;; Transform vertices
        triangles  (generate-triangle-fans xformed)]             ;; Get all triangles
    #_(pp/pprint triangles)
    (loop [xs triangles
           acc 0]
      (if-let [x (first xs)]
        (recur (rest xs) (+ acc (if (same-side-algo av x) 1 0)))

        ;; Point is inside if hitrate is odd, otherwise outside
        (do
          (println (format "TRIANGLE_HIT_RATE=%s" acc))
          (odd? acc))))))


;; ----------------------
;; -  Barycentric Algorithm
;; ---

(defn barycentric-algo
  [pt vertices]
  )


;; ====================
;; -  Test Functions  -
;; ====================

(def test-vertices
  [[ 2  0]
   [ 2  3]
   [-1  1]
   [-3  1]
   [-3 -2]
   [-1 -2]
   [-2 -4]])

(def in
  [[ 1    1  ]
   [-2    0  ]
   [-0.5 -0.5]
   [ 0.5 -0.5]])

(def out
  [[ 2 -2]
   [-3  3]
   [-4 -3]])

(def P [ 3 1 -8])
(def Q [ 4 4  1])
(def R [-5 7  2])


;; ---- Inclusion Fixtures

;; kiel
(def polygon-1-testcase
  (hash-map
    :plane   [54.355198 10.164557]
    :polygon [[54.310769 10.129421]
              [54.309937 10.135439]
              [54.316093 10.143296]
              [54.317051 10.150138]
              [54.326334 10.172375]
              [54.324386 10.185774]
              [54.330768 10.188743]
              [54.333148 10.179165]
              [54.361172 10.183925]
              [54.364926 10.202201]
              [54.393815 10.198791]
              [54.390464 10.152578]
              [54.375627 10.156783]
              [54.370442 10.123081]
              [54.366189 10.120979]
              [54.362752 10.1402]
              [54.350562 10.136483]
              [54.342714 10.152149]
              [54.310769 10.129421]]
    :in      [[54.356222 10.160379]
              [54.367320 10.194962]
              [54.326997 10.154361]
              [54.311375 10.133376]
              [54.327469 10.185386]
              [54.368393 10.124422]
              [54.392739 10.197563]]
    :out     [[54.394247 10.194452]
              [54.391750 10.199868]
              [54.370384 10.111040]
              [54.361521 10.063705]
              [54.444150 10.234952]]))


;; ----

;; norco
(def polygon-2-testcase
  (hash-map
    :plane   [29.992134 -90.450516]
    :polygon [[29.993095 -90.460828]
              [29.992001 -90.454835]
              [29.990196 -90.446007]
              [29.99019  -90.43206]
              [29.994351 -90.432192]
              [29.994385 -90.434352]
              [29.995379 -90.445602]
              [29.996012 -90.451396]
              [29.99729  -90.45873]
              [29.997392 -90.458726]
              [29.997392 -90.45888]
              [29.997426 -90.459535]
              [29.992934 -90.460871]
              [29.993095 -90.460828]]
    :in      [[29.993428 -90.442797]
              [29.994479 -90.455238]]
    :out     [[29.998512 -90.446112]
              [29.995766 -90.429642]
              [30.000350 -90.467376]]))


;; ---

;; arteco
(def polygon-3-testcase
  (hash-map
    :plane   [29.980272 -93.880814]
    :polygon [[29.977193 -93.873439]
              [29.981075 -93.871994]
              [29.984797 -93.891251]
              [29.979695 -93.893399]
              [29.977193 -93.873439]]
    :in      [[29.979534 -93.881330]
              [29.979767 -93.873637]]
    :out     [[29.983746 -93.894129]
              [29.983196 -93.880776]
              [29.978697 -93.872460]]))

;; -- test functions

(defn test
  [[lat lon]]
  (let [x (to-x [lat lon])
        y (to-y [lat lon])
        z (to-z [lat lon])]
    (println (format "LAT[%s = %s]" lat (to-lat [x y z])))
    (println (format "LON[%s = %s]" lon (to-lon [x y z])))))

(defn test-centriod
  []
  (let [pts-1 [[ 1.0  0.0]
               [ 0.0  1.0]
               [-1.0  0.0]
               [ 0.0 -1.0]]
        pts-2 [[-21.1333 -175.2   ]    ; Tonga
               [ -8.53333 179.2167]]]  ; Tuvalu
    (println (format "CENTRIOD[%s]=%s" pts-1 (centroid pts-1)))
    (println (format "CENTRIOD[%s]=%s" pts-2 (centroid pts-2)))))

(defn test-spherical
  [lat-lon]
  (let [cart   (to-cartesian lat-lon)]
    (println (format "ρ=%s" (to-rho cart)))
    (println (format "θ[%s]=%s" (Math/toDegrees (to-theta cart)) (to-theta cart)))
    (println (format "φ[%s]=%s" (Math/toDegrees (to-phi cart)) (to-phi cart)))))

(defn test-vector
  [lat-lon]
  (let [uv1 (unit-normal-vector lat-lon)
        uv2 (normal-vector lat-lon :radius 1)
        nv  (normal-vector lat-lon)]
    (println (format "UNIT_VECTOR[%s]=%s" (mtx/magnitude uv1) uv1))
    (println (format "UNIT_VECTOR[%s]=%s" (mtx/magnitude uv2) uv2))
    (println (format "NORMAL_VECTOR[%s]=%s" (mtx/magnitude nv) nv))))

(defn test-general-vector
  []
  (let [pq (a->b-vector P Q)
        pr (a->b-vector P R)]
    (println (format "%s - %s -> %s" P Q pq))
    (println (format "%s - %s -> %s" P R pr))
    (println (format "PQ[%s] x PR[%s] -> %s" pq pr (mtx/cross pq pr)))))

(defn test-point->plane
  []
  ;; Q(3, 8, -1)
  ;; 4(x + 2) - (y + 5) + 2z = 0
  (let [p [-2 -5  0]  ;; point in plane
        q [ 3  8 -1]  ;; point off plane
        n [ 4 -1  2]] ;; normal vector
    (println (format "Q[%s], DISTANCE=%s" q (point->plane-distance (mtx/matrix n) (a->b-vector p q))))))

(defn test-ortho-plane-projection-1
  []
  ;; Q(3, 8, -1)
  ;; 4(x + 2) - (y + 5) + 2z = 0
  (let [q   [ 3  8 -1]              ;; point off plane
        p   [-2 -5  0]              ;; point in plane
        unv (normalize [4 -1  2])]  ;; unit normal vector
    (println (format "Q[%s], ORTHO_PROJECTION=%s" q (ortho-plane-projection q p unv)))))

(defn test-ortho-plane-projection-2
  []
  ;; Q(5, -6, 3)
  ;; 3(x - 0) - 2(y - 0) + (z - 2) = 0
  ;; Proj(-1, -2, 1)
  (let [q   [5 -6  3]              ;; point off plane
        p   [0  0  2]              ;; point in plane
        unv (normalize [3 -2  1])] ;; unit normal vector
    (println (format "Q[%s], ORTHO_PROJECTION=%s" q (ortho-plane-projection q p unv)))))


(defn test-point-inclusion
  [pts plane polygon expected]
  (doseq [pt pts]
    (println (format "TESTING=%s" pt))
    (println (format "PT[%s], POLYGON[%s]=actual:%s|expected:%s" pt polygon (point-in-polygon-ssa? pt plane polygon) expected))))


(defn test-triange-fan
  [vertices]
  (println (format "TRIANGE_FAN_INPUT=%s" vertices))
  (pp/pprint (generate-triangle-fans vertices)))


;; ------------
;; -  Driver  -
;; ------------

(defn test-all
  []
  (let []
    (test-centriod)
    (test [0.0 0.0])
    (test [17.688132 83.299026])
    (test-vector [17.688132 83.299026])
    (test-vector [0 0])
    (test-spherical [45 0])
    (test-spherical [45 -90])
    (test-spherical [45 -180])
    (test-spherical [-90 -180])
    (test-general-vector)
    (test-point->plane)
    (test-ortho-plane-projection-1)
    (test-ortho-plane-projection-2)))

(defn -main
  [& args]
  #_(test-all)
  (println "@@@@@@@ TESTCASE 1: IN")
  (println "----------------------")
  (test-point-inclusion (:in polygon-1-testcase) (:plane polygon-1-testcase) (:polygon polygon-1-testcase) true)
  (println "@@@@@@@ TESTCASE 1: OUT")
  (println "----------------------")
  (test-point-inclusion (:out polygon-1-testcase) (:plane polygon-1-testcase) (:polygon polygon-1-testcase) false)
  (println "@@@@@@@ TESTCASE 2: IN")
  (println "----------------------")
  (test-point-inclusion (:in polygon-2-testcase) (:plane polygon-2-testcase) (:polygon polygon-2-testcase) true)
  (println "@@@@@@@ TESTCASE 2: OUT")
  (println "----------------------")
  (test-point-inclusion (:out polygon-2-testcase) (:plane polygon-2-testcase) (:polygon polygon-2-testcase) false)
  (println "@@@@@@@ TESTCASE 3: IN")
  (println "----------------------")
  (test-point-inclusion (:in polygon-3-testcase) (:plane polygon-3-testcase) (:polygon polygon-3-testcase) true)
  (println "@@@@@@@ TESTCASE 3: OUT")
  (println "----------------------")
  (test-point-inclusion (:out polygon-3-testcase) (:plane polygon-3-testcase) (:polygon polygon-3-testcase) false)
  (test-triange-fan (range 14)))
