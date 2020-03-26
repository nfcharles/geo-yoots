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





;; =====================
;; -                   -
;; -  Test Main Diver  -
;; -                   -
;; =====================


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
    (test {:lat 0.0 :lon 0.0})
    (test {:lat 17.688132 :lon 83.299026})))