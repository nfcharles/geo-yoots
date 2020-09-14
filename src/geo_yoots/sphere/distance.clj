(ns geo-yoots.sphere.distance
  (:require [geo-yoots.constants :as geo.const]
            [geo-yoots.util.core :as geo.util]))




;;;; ====
;;;; ** Spherical geometry implementations
;;;; ----
;;;; Sources:
;;;;  i.  https://www.movable-type.co.uk/scripts/latlong.html
;;;;  ii. https://stackoverflow.com/questions/32771458/distance-from-lat-lng-point-to-minor-arc-segment
;;;;
;;;; ====



(defn haversine
  "Measures great cirlce distance of two points."
  [[lat1 lon1] [lat2 lon2] & {:keys [radius]
                              :or {radius geo.const/earth-radius}}]
  (let [dlat  (Math/toRadians (- lat2 lat1))
        dlon  (Math/toRadians (- lon2 lon1))
        lat1r (Math/toRadians lat1)
        lat2r (Math/toRadians lat2)
        a (+ (* (Math/sin (/ dlat 2)) (Math/sin (/ dlat 2)))
             (* (Math/sin (/ dlon 2)) (Math/sin (/ dlon 2)) (Math/cos lat1r) (Math/cos lat2r)))]
    (->> (Math/sqrt a)
         (Math/asin)
         (* radius 2))))

(defn alt-distance
  [[lat1 lon1] [lat2 lon2] & {:keys [radius]
                              :or {radius geo.const/earth-radius}}]
  (let [lat1r (Math/toRadians lat1)
        lon1r (Math/toRadians lon1)
        lat2r (Math/toRadians lat2)
        lon2r (Math/toRadians lon2)]
    (* (Math/acos (+ (* (Math/sin lat1r) (Math/sin lat2r))
                     (* (Math/cos lat1r) (Math/cos lat2r) (Math/cos (- lon2r lon1r)))))
       radius)))



;;; ===
;;; - Cross Track Distance
;;; ----
;;;
;;; Formula:    dxt = asin( sin(δ13) ⋅ sin(θ13−θ12) ) ⋅ R
;;;  where:
;;;    δ13 is (angular) distance from start point to third point
;;;     * d13 / R
;;;    θ13 is (initial) bearing from start point to third point
;;;    θ12 is (initial) bearing from start point to end point
;;;    R is the earth’s radius
;;; ---

;; TODO: Use `crosstrack-distance2` for impl
(defn crosstrack-distance
  [pt l1 l2 & {:keys [radius]
               :or {radius geo.const/earth-radius}}]
  (let [dist (haversine l1 pt :radius radius)
        d13  (/ dist radius)]
    #_(println (format "D1->D3=%s" dist))
    (* (Math/asin (* (Math/sin d13)
                     (Math/sin (- (geo.util/bearing l1 pt)
                                  (geo.util/bearing l1 l2)))))
       radius)))


;;; ===
;;; - Along Track Distance
;;; ---
;;;
;;; Formula: dat = acos( cos(δ13) / cos(δxt) ) ⋅ R
;;;   where:
;;;     δ13 is (angular) distance from start point to third point
;;;     δxt is (angular) cross-track distance
;;;     R is the earth’s radius
;;;
;;; ---

(defn alongtrack-distance2
  "Computes alongtrack distance.
  Uses precomputed distance and crosstrack values."
  [dist13 ct-dist & {:keys [radius]
              :or {radius geo.const/earth-radius}}]
  (let [d13 (/ dist13 radius)]
    (* (Math/acos (/ (Math/cos d13) (Math/cos (/ ct-dist radius)))) radius)))


;;; ===
;;; - Cross Arc Distance
;;; ---
;;;
;;; Variant of cross track distance.  Defines minimun distance to great circle arc - as opposed to
;;; complete great cirle spanning sphere.
;;;
;;;
;;; p1: Great circle point a
;;; p2: Great circle point b
;;; p3: Point whose distance to edge(a,b) we want to minimize
;;; p4: Great circle intersection point
;;;
;;;
;;; Case 1:
;;; ---
;;; Relative bearing is obtuse.
;;;
;;;  p3
;;;  v
;;;  o
;;;   `     bearing(p1->p3) - bearing(p1->p2) is obtuse
;;;     `      |
;;;       `    v
;;;         `o--------------o
;;;          ^               ^
;;;          p1              p2
;;;
;;; Use |p3 -> p1|
;;;
;;; Case 2:
;;; ---
;;; i. Relative bearing is actue and p4 falls w/i arc.
;;;
;;;                p3
;;;                 v
;;;                 o
;;;               ` |
;;;             `   |
;;;           `     | <- crosstrack distance
;;;         `       |
;;;       `         |
;;;     `           |
;;;   o-------------o------o
;;;   ^      ^      ^      ^
;;;   p1     |      p4     p2
;;;          |
;;;          |
;;;          -
;;;   alongtrack distance
;;;
;;;
;;; Use crosstrack distance
;;;
;;; ii. Relative bearing is acute and p4 falls outside arc.
;;;
;;;
;;;                   p3
;;;                   v
;;;                   o
;;;                 `.|
;;;               ` . |
;;;             `  .  |
;;;           `   .   | <- crosstrack distance
;;;         `    .    |
;;;       `     .     |
;;;     `      .      |
;;;   o-------o       o
;;;   ^       ^       ^
;;;   p1      p2      p4
;;;
;;;   <--------------->
;;;           ^
;;;           alongtrack distance
;;;
;;;  Use |p3 -> p2|
;;;
;;; ---

(defn crosstrack-distance2
  "Calculate crosstrack distance.
  Uses precomputed bearing and distance values"
  [dist13 b13 b12 & {:keys [radius]
                     :or {radius geo.const/earth-radius}}]
  (let [d13  (/ dist13 radius)]
    #_(println (format "D1->D3=%s" dist))
    (* (Math/asin (* (Math/sin d13)
                     (Math/sin (- b13 b12))))
       radius)))

(defn crossarc-distance
  "Calculates crosstrack distance"
  [pt l1 l2 & {:keys [radius]
               :or {radius geo.const/earth-radius}}]
  (let [b12    (geo.util/bearing l1 l2)
        b13    (geo.util/bearing l1 pt)
        dist13 (haversine l1 pt :radius radius)
        diff   (geo.util/relative-bearing b12 b13)]
    (if (> diff (/ Math/PI 2))
      ;; Relative bearing is obtuse
      dist13
      (let [ct-dist (Math/abs (crosstrack-distance2 dist13 b13 b12 :radius radius))
            d12     (haversine l1 l2 :radius radius)
            d14     (alongtrack-distance2 dist13 ct-dist :radius radius)]
	(if (> d14 d12)
          (haversine l2 pt :radius radius)  ; Pt is beyond arc
          ct-dist)))))                      ; Pt w/i arc, use crosstrack distance



;;; ================================
;;;
;;; **  SHAPE DISTANCE FUNCTIONS  **
;;;
;;; ================================


;; ===
;; - Distance from point to point
;; ---
;;
;; Uses Haversine distance function

(defn to-point
  [p1 p2]
  (haversine p1 p2))


;;; ===
;;; - Distance from point to line
;;; ---

(defn -to-polyline
  [pt vertices]
  (loop [xs vertices
         acc Integer/MAX_VALUE]
    (if-let [x (first xs)]
      (let [[arc-p1 arc-p2] x
            dist (crossarc-distance pt arc-p1 arc-p2)]
        (recur (rest xs) (if (< dist acc) dist acc)))
      acc)))

(defn to-polyline
  [pts vertices]
  (let [edges (geo.util/gen-polyline-edges vertices)]
    (loop [xs pts
           acc []]
      (if-let [pt (first xs)]
        (recur (rest xs) (conj acc (-to-polyline pt edges)))
        acc))))

(defn -within-distance-to-polyline?
  [limit pt vertices]
  (loop [xs vertices]
    (if-let [x (first xs)]
      (let [[arc-p1 arc-p2] x]
        (if (<= (crossarc-distance pt arc-p1 arc-p2) limit)
          true
          (recur (rest xs))))
      false)))

(defn within-distance-to-polyline?
  [limit pts vertices]
  (let [edges (geo.util/gen-polyline-edges vertices)]
    (loop [xs pts
           acc []]
      (if-let [pt (first xs)]
        (recur (rest xs) (conj acc (-within-distance-to-polyline? limit pt edges)))
        acc))))


;;; ===
;;; - Distance from point to circle
;;; ---

(defn -to-circle
  [pt center radius]
  (let [pt-to-center (haversine pt center)]
    (- (haversine pt center) radius)))

(defn to-circle
  [pts center radius]
  (let [c [(nth center 0) (nth center 1)]]
    (loop [xs pts
           acc []]
      (if-let [pt (first xs)]
        (recur (rest xs) (conj acc (-to-circle pt c radius)))
        acc))))

(defn within-distance-to-circle?
  [limit pts center radius]
  (let [c [(nth center 0) (nth center 1)]]
    (loop [xs pts
           acc []]
      (if-let [pt (first xs)]
        (recur (rest xs) (conj acc (<= (-to-circle pt c radius) limit)))
        acc))))


;;; ===
;;; - Distance from point to polygon
;;; ---

(defn -to-polygon
  [pt vertices]
  (loop [xs vertices
         acc Integer/MAX_VALUE]
    (if-let [x (first xs)]
      (let [[arc-p1 arc-p2] x
            dist (crossarc-distance pt arc-p1 arc-p2)]
        (recur (rest xs) (if (< dist acc) dist acc)))
      acc)))

(defn to-polygon
  [pts vertices]
  (let [edges (geo.util/gen-polygon-edges vertices)]
    (loop [xs pts
           acc []]
      (if-let [pt (first xs)]
        (recur (rest xs) (conj acc (-to-polygon pt edges)))
        acc))))

(defn -within-distance-to-polygon?
  [limit pt vertices]
  (loop [xs vertices]
    (if-let [x (first xs)]
      (let [[arc-p1 arc-p2] x]
        (if (<= (crossarc-distance pt arc-p1 arc-p2) limit)
          true
          (recur (rest xs))))
      false)))

(defn within-distance-to-polygon?
  [limit pts vertices]
  (let [edges (geo.util/gen-polygon-edges vertices)]
    (loop [xs pts
           acc []]
      (if-let [pt (first xs)]
        (recur (rest xs) (conj acc (-within-distance-to-polygon? limit pt edges)))
        acc))))
