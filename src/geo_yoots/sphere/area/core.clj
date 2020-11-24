(ns geo-yoots.sphere.area.core
  (:require [clojure.pprint :as pp]
            [geo-yoots.constants :as geo.const]
            [geo-yoots.util.core :as geo.util]
            [geo-yoots.sphere.util :as geo.sphere.util]
            [geo-yoots.sphere.transformations :as geo.sphere.xform]
            [clojure.core.matrix :as mtx]
            [clojure.core.matrix.operators :as mtx.op]))




(def X 0)
(def Y 1)
(def Z 2)

;; =================
;; - Area of Polygon
;; -----------------
;; - Transform projected polgyon (centroid projection plane) to XY plane and apply shoelace algorithm
;; -
;; - https://en.wikipedia.org/wiki/Shoelace_formula
;; ---

(defn shoelace-matrix
  [vertices]
  (let [pvtx        (geo.sphere.xform/vertices->projection-plane vertices)
        plane-norm  (geo.sphere.xform/polygon->normal pvtx)]  ;; normal vector of polygon projection plane
    #_(println (format "PROJECTED_VERTICES=%s" pvtx))
    (let [rot-mtx   (geo.sphere.xform/rotation-matrix plane-norm)]
      #_(println (format "ROTATION_MATRIX=%s" rot-mtx))
      (loop [xs pvtx
             acc []]
        (if-let [x (first xs)]
          (let [vtx (mtx.op/* geo.const/earth-radius (geo.sphere.xform/rotate rot-mtx x))]
            #_(println (format "TRANSLATING POINT= (%s -> %s" x vtx))
            (recur (rest xs) (conj acc [(.get vtx X) (.get vtx Y)])))
          (mtx/matrix acc))))))

(defn apply-shoelace
  [pts]
  (let [n (.rowCount pts)]
    (loop [i   0
           acc 0]
      (if (< i n)
        (let [i_idx   (mod i n)
              i+1_idx (mod (inc i) n)
              x_i     (.get pts i_idx   X)
              y_i+1   (.get pts i+1_idx Y)
              x_i+1   (.get pts i+1_idx X)
              y_i     (.get pts i_idx   Y)]
          #_(println (format "%s * %s - %s * %s" x_i y_i+1 x_i+1 y_i))
          (recur (inc i)
                 ( + acc (- (* x_i y_i+1) (* x_i+1 y_i)))))
        (/ (Math/abs acc) 2)))))



;; ====================
;; -  Area Functions  -
;; ====================

(defn polygon
  [vertices]
  (apply-shoelace (shoelace-matrix vertices)))

(defn circle
  [radius]
  (* Math/PI (geo.util/sq radius)))
