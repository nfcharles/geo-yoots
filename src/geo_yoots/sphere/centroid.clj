(ns geo-yoots.sphere.centroid
  (:require [geo-yoots.constants :as geo.consts]
            [geo-yoots.sphere.util :as geo.sphere.util]
            [geo-yoots.sphere.area.core :as geo.sphere.area]
            [geo-yoots.sphere.transformations :as geo.sphere.xform]
            [clojure.core.matrix :as mtx]
            [clojure.core.matrix.operators :as mtx.op]))



(def X 0)
(def Y 1)
(def Z 2)

;; ======================
;; -  Polygon centroid  -
;; ----------------------
;;
;; Find centroid of simple polygon
;;
;; https://en.wikipedia.org/wiki/Centroid#Of_a_polygon
;; ---

(defn Cx
  [area pts]
  (let [n (.rowCount pts)]
    (loop [i   0
           acc 0]
      (if (< i n)
        (let [i_idx   (mod i n)
              i+1_idx (mod (inc i) n)
              x_i     (.get pts i_idx   X)
              x_i+1   (.get pts i+1_idx X)
              y_i     (.get pts i_idx   Y)
              y_i+1   (.get pts i+1_idx Y)]
          (recur (inc i)
                 (+ acc (* (+ x_i x_i+1) (- (* x_i y_i+1) (* x_i+1 y_i))))))
        (/ acc (* 6 area))))))

(defn Cy
  [area pts]
  (let [n (.rowCount pts)]
    (loop [i   0
           acc 0]
      (if (< i n)
        (let [i_idx   (mod i n)
              i+1_idx (mod (inc i) n)
              x_i     (.get pts i_idx   X)
              x_i+1   (.get pts i+1_idx X)
              y_i     (.get pts i_idx   Y)
              y_i+1   (.get pts i+1_idx Y)]
          (recur (inc i)
                 (+ acc (* (+ y_i y_i+1) (- (* x_i y_i+1) (* x_i+1 y_i))))))
        (/ acc (* 6 area))))))

(defn mtx->latlon
  [mtx]
  (geo.sphere.util/cartesian->latlon
    [(.get mtx X)
     (.get mtx Y)
     (.get mtx Z)]))

(defn polygon
  [vertices]
  (let [proj-vtx (geo.sphere.xform/vertices->projection-plane vertices) ;; vertices projected to plane
        rot-mtx  (-> proj-vtx
                     (geo.sphere.xform/polygon->normal)    ;; normal vector of polygon projection plane
                     (geo.sphere.xform/rotation-matrix))   ;; XYZ plane -> XY plane rotation
        area     (geo.sphere.area/polygon vertices)]
    (loop [xs proj-vtx
           acc []]
      (if-let [x (first xs)]
        (let [vtx (geo.sphere.xform/rotate rot-mtx x)]
          (recur (rest xs) (conj acc [(.get vtx X) (.get vtx Y) (.get vtx Z)])))
        (let [ret (mtx/matrix acc)]
          (->> [(Cx area ret) (Cy area ret) (.get ret 0 Z)]
               (mtx/matrix)
               (geo.sphere.xform/rotate (mtx/inverse rot-mtx))
               (mtx.op/* geo.consts/earth-radius)
               (mtx->latlon)))))))
