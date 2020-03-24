(ns geo-yoots.sphere.core
  (:require [geo-yoots.constants :as const]
            [geo-yoots.util.core :as util]))



;;;; ====
;;;; ** Spherical geometry implementations
;;;; ----
;;;; Sources:
;;;;  i.  https://www.movable-type.co.uk/scripts/latlong.html
;;;;  ii. https://stackoverflow.com/questions/32771458/distance-from-lat-lng-point-to-minor-arc-segment
;;;;
;;;; ====


;;; ===
;;; - Bearing
;;; ---
;;;
;;; Formula:	θ = atan2( sin Δλ ⋅ cos φ2 , cos φ1 ⋅ sin φ2 − sin φ1 ⋅ cos φ2 ⋅ cos Δλ )
;;;   where	φ1,λ1 is the start point, φ2,λ2 the end point (Δλ is the difference in longitude)
;;;

(defn bearing
  [{lon1 :lon lat1 :lat} {lon2 :lon lat2 :lat}]
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


;;; ===
;;; - Cross Track Distance
;;; ----
;;;
;;; Formula:    dxt = asin( sin(δ13) ⋅ sin(θ13−θ12) ) ⋅ R
;;;  where   
;;;
;;; δ13 is (angular) distance from start point to third point
;;;   * d13 / R
;;; θ13 is (initial) bearing from start point to third point
;;; θ12 is (initial) bearing from start point to end point
;;; R is the earth’s radius

(defn crosstrack-distance
  [pt l1 l2 & {:keys [radius]
               :or {radius const/earth-radius}}]
  (let [dist (util/haversine l1 pt :radius radius)
        d13  (/ dist radius)]
    #_(println (format "D1->D3=%s" dist))
    (* (Math/asin (* (Math/sin d13)
                     (Math/sin (- (bearing l1 pt)
                                  (bearing l1 l2)))))
       radius)))


;;; ===
;;; - Cross Arc Distance
;;; ---
;;;
;;; TODO: Explain algorithm
;;;

(defn -crosstrack-distance2
  "Calculate crosstrack distance.
  Uses precomputed bearing and distance values"
  [dist13 b13 b12 & {:keys [radius]
                     :or {radius const/earth-radius}}]
  (let [d13  (/ dist13 radius)]
    #_(println (format "D1->D3=%s" dist))
    (* (Math/asin (* (Math/sin d13)
                     (Math/sin (- b13 b12))))
       radius)))

(defn distance-to-crosstrack-intersection
  "Calculates distance between GC point and crosstrack intersection point"
  [dist13 ct-dist & {:keys [radius]
                     :or {radius const/earth-radius}}]
  (* (Math/acos (/ (Math/cos (/ dist13 radius)) (Math/cos (/ ct-dist radius)))) radius))

(defn crossarc-distance
  "Calculates crosstrack distance"
  [pt l1 l2 & {:keys [radius]
               :or {radius const/earth-radius}}]
  (let [b12    (bearing l1 l2)
        b13    (bearing l1 pt)
        dist13 (util/haversine l1 pt :radius radius)]
    (if (> (Math/abs (- b13 b12)) (/ Math/PI 2))
      ;; Relative bearing is obtuse
      dist13
      (let [ct-dist (-crosstrack-distance2 dist13 b13 b12 :radius radius)
            d12     (util/haversine l1 l2 :radius radius)
            d14     (distance-to-crosstrack-intersection dist13 ct-dist :radius radius)]
        ;; Is point beyound arc
	(if (> d14 d12)
          ;; Pt is beyond arc
          (util/haversine l2 pt :radius radius)

          ;; Pt w/i arc, use crosstrack distance
	  (Math/abs ct-dist))))))


;;; ==========================
;;;
;;; **  DISTANCE FUNCTIONS  **
;;;
;;; ==========================


;; ===
;; - Distance from point to point
;; ---
;;
;; Uses Haversine distance function

(defn distance-to-point
  [p1 p2]
  (util/haversine p1 p2))


;;; ===
;;; - Distance from point to line
;;; ---
;;;
;;;

(defn min-distance-to-line
  [pt vertices]
  (loop [xs vertices
         acc Integer/MAX_VALUE]
    (if-let [x (first xs)]
      (let [[arc-p1 arc-p2] x
            dist (crossarc-distance pt arc-p1 arc-p2)]
        (recur (rest xs) (if (< dist acc) dist acc)))
      acc)))

(defn within-distance-to-line?
  [pt limit vertices]
  (loop [xs vertices]
    (if-let [x (first xs)]
      (let [[arc-p1 arc-p2] x]
        (if (> (crossarc-distance pt arc-p1 arc-p2) limit)
          (recur (rest xs))
          true)))))


;;; ===
;;; - Distance from point to circle
;;; ---

(defn distance-to-cirlce
  [pt center radius]
  (- (util/haversine pt center) radius))


;;; ===
;;; - Distance from point to polygon
;;; ---

(defn min-distance-to-polygon
  [pt vertices]
  (loop [xs vertices
         acc Integer/MAX_VALUE]
    (if-let [x (first xs)]
      (let [[arc-p1 arc-p2] x
            dist (crossarc-distance pt arc-p1 arc-p2)]
        (recur (rest xs) (if (< dist acc) dist acc)))
      acc)))

(defn within-distance-to-polygon?
  [pt limit vertices]
  (loop [xs vertices]
    (if-let [x (first xs)]
      (let [[arc-p1 arc-p2] x]
        (if (> (crossarc-distance pt arc-p1 arc-p2) limit)
          (recur (rest xs))
          true)))))



;; ========================
;;
;; **  Main Test Driver  **
;;
;; ========================

(defn bearing-test
  []
  (let [A {:lon  105.2833 :lat  40.0167}
        B {:lon -137.65   :lat -33.9333}
        C {:lon   45      :lat  35}
        D {:lon  135      :lat  35}
        E {:lat 39.099912 :lon -94.581213}
        F {:lat 38.627089 :lon -90.200203}
        G {:lat 55.739399 :lon 37.592572}
        H {:lat 55.735632 :lon 37.678367}]
    (println (normalized-bearing A B))
    #_(println (normalized-bearing B A))
    (println (normalized-bearing C D))
    #_(println (normalized-bearing D C))
    (println (normalized-bearing E F))
    #_(println (normalized-bearing F E))
    (println (normalized-bearing G H))))

(defn test-crosstrack
  []
  (let [pt {:lat 17.688132 :lon 83.299026}
        A  {:lat 17.686309 :lon 83.291671}
        B  {:lat 17.690634 :lon 83.292155}
        C  {:lat 17.686165 :lon 83.283841}
        D  {:lat 17.687492 :lon 83.277584}
        E  {:lat 17.695138 :lon 83.267556}
        F  {:lat 17.708919 :lon 83.261631}
        G  {:lat 17.714786 :lon 83.269032}
        H  {:lat 17.713927 :lon 83.272998}
        I  {:lat 17.706820 :lon 83.277726}]
    (println "Crosstrack Distance")
    (println (crosstrack-distance pt A B))
    (println (crosstrack-distance pt A C))
    (println (crosstrack-distance pt C D))
    (println (crosstrack-distance pt D E))
    (println "Crossarc Distance")
    (println (crossarc-distance pt A B))
    (println (crossarc-distance pt A C))
    (println (crossarc-distance pt C D))
    (println (crossarc-distance pt D E))
    (println (crossarc-distance pt E F))
    (println (crossarc-distance pt F G))
    (println (crossarc-distance pt G H))
    (println (crossarc-distance pt H I))))


(defn -main
  [& args]
  (let []
    (bearing-test)
    (test-crosstrack)))
