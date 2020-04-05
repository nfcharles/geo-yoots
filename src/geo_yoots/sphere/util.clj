(ns geo-yoots.sphere.util
  )



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
  [{lat :lat lon :lon }]
  (* (Math/cos (Math/toRadians lon)) (Math/cos (Math/toRadians lat))))

(defn to-y
  [{lat :lat lon :lon }]
  (* (Math/sin (Math/toRadians lon)) (Math/cos (Math/toRadians lat))))

(defn to-z
  [{lat :lat lon :lon }]
  (Math/sin (Math/toRadians lat)))



;; ===
;; - Cartesian coordinates to lat/lon coordinates
;; ---
;;
;; Conversion:
;;   λ: atan2(y,x)
;;   φ: atan2(z, sqrt(x^2 + y^2)
;; ---

(defn to-lat
  [{x :x y :y z :z}]
  (Math/toDegrees (Math/atan2 z (Math/sqrt (+ (* x x) (* y y))))))

(defn to-lon
  [{x :x y :y z :z}]
  (Math/toDegrees (Math/atan2 y x)))



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
          (hash-map
            :lat (to-lat {:x x-avg :y y-avg :z z-avg})
            :lon (to-lon {:x x-avg :y y-avg :z z-avg})))))))



;; =====================
;; -                   -
;; -  Test Main Diver  -
;; -                   -
;; =====================

(defn test-centriod
  []
  (let [pts-1 [{:lat  1.0  :lon  0.0}
               {:lat  0.0  :lon  1.0}
               {:lat -1.0  :lon  0.0}
               {:lat  0.0  :lon -1.0}]
        pts-2 [{:lat -21.1333 :lon -175.2}       ; Tonga
               {:lat -8.53333 :lon  179.2167}]]  ; Tuvalu
    (println (format "CENTRIOD[%s]=%s" pts-1 (centroid pts-1)))
    (println (format "CENTRIOD[%s]=%s" pts-2 (centroid pts-2)))))

(defn test
  [{lat :lat lon :lon}]
  (let [x (to-x {:lat lat :lon lon})
        y (to-y {:lat lat :lon lon})
        z (to-z {:lat lat :lon lon})]
    (println (format "LAT[%s = %s]" lat (to-lat {:x x :y y :z z})))
    (println (format "LON[%s = %s]" lon (to-lon {:x x :y y :z z})))))




(defn -main
  [& args]
  (let []
    (test-centriod)
    (test {:lat 0.0 :lon 0.0})
    (test {:lat 17.688132 :lon 83.299026})))
