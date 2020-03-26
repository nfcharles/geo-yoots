(ns geo-yoots.util.core
  (:require [geo-yoots.constants :as geo.const]))



(defn haversine
  "Measures great cirlce distance of two points."
  [{lat1 :lat lon1 :lon} {lat2 :lat lon2 :lon} & {:keys [radius]
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
  [{lat1 :lat lon1 :lon} {lat2 :lat lon2 :lon} & {:keys [radius]
                                                  :or {radius geo.const/earth-radius}}]
  (let [lat1r (Math/toRadians lat1)
        lon1r (Math/toRadians lon1)
        lat2r (Math/toRadians lat2)
        lon2r (Math/toRadians lon2)]
    (* (Math/acos (+ (* (Math/sin lat1r) (Math/sin lat2r))
                     (* (Math/cos lat1r) (Math/cos lat2r) (Math/cos (- lon2r lon1r)))))
       radius)))
