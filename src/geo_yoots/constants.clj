(ns geo-yoots.constants
  )



(def km->meters        1000)         ; kilometers -> meters
(def meters->nm        0.000539957)  ; meters     -> nautical miles
(def earth-radius 6372.8)            ; kilometers


;; ----
;; - Conversions
;; ---

(def earth-radius-meters (* earth-radius km->meters))
(def earth-radius-nautical-miles (* earth-radius-meters meters->nm))
