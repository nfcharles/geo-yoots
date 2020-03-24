(ns geo-yoots.util.core
  (:require [ geo-yoots.constants :as const]))



(defn haversine
  "Measures great cirlce distance of two points."
  [{lon1 :lon lat1 :lat} {lon2 :lon lat2 :lat} & {:keys [radius]
                                                  :or {radius const/earth-radius}}]
  (let [dlat (Math/toRadians (- lat2 lat1))
        dlon (Math/toRadians (- lon2 lon1))
        lat1 (Math/toRadians lat1)
        lat2 (Math/toRadians lat2)
        a (+ (* (Math/sin (/ dlat 2)) (Math/sin (/ dlat 2)))
             (* (Math/sin (/ dlon 2)) (Math/sin (/ dlon 2)) (Math/cos lat1) (Math/cos lat2)))]
    (->> (Math/sqrt a)
         (Math/asin)
         (* radius 2))))
