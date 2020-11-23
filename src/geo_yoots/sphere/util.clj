(ns geo-yoots.sphere.util
  (:require [clojure.pprint :as pp]
            [geo-yoots.constants :as geo.consts]
            [geo-yoots.util.core :as geo.util]))


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
