(ns geo-yoots.util.core
  (:require [geo-yoots.constants :as geo.const]))



;; ===
;; - Edge Functions
;; ---
;; Generates edges from an input set of vertices
;;
;; ---

(defn label-vertices
  "Create {:lat y :lon x} map from input vector [`lat` `lon`]"
  [vertices]
  (loop [xs vertices
         acc []]
    (if-let [x (first xs)]
      (let [[lat lon] x]
        (recur (rest xs) (conj acc {:lat lat :lon lon})))
      acc)))


(defn gen-polyline-edges
  "Create polyline edges from vertices"
  [vertices]
  (let [start (first vertices)]
    (loop [xs (rest vertices)
           a start
           acc []]
      (if-let [b (first xs)]
        (recur (rest xs) b (conj acc [a b]))
        acc))))

(defn gen-polygon-edges
  "Creates polygon edges from vertices"
  [vertices]
  (let [start (first vertices)]
    (loop [xs (rest vertices)
           a start
           acc []]
      (if-let [b (first xs)]
        (recur (rest xs) b (conj acc [a b]))
        (conj acc [a start])))))

