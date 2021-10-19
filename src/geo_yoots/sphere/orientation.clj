(ns geo-yoots.sphere.orientation
  (:require [geo-yoots.constants :as geo.const]
            [geo-yoots.sphere.transformations :as geo.sphere.xform]
            [clojure.core.matrix :as mtx]
            [clojure.core.matrix.operators :as mtx.op]))


(def X 0)
(def Y 1)
(def Z 2)

(defn orientation
  [x]
  (let [det (mtx/det x)]
    (if (= det 0.0) 0 (int (/ det (Math/abs det))))))

(defn point-line-orientation
  "Determines orientation of point r relative to line p->q.
   Orientation => (-1<right> | 0<colinear> | 1<left>)"
  [r line]
  (let [tri        (conj line r)
        ;;_          (println (format "triangle: %s" tri))
        projected  (geo.sphere.xform/vertices->projection-plane tri)
        plane-norm (geo.sphere.xform/polygon->normal projected)
        rot-mtx    (geo.sphere.xform/rotation-matrix plane-norm)]
    #_(println (format "projected: %s" projected))
    (loop [xs projected
           acc []]
      (if-let [x (first xs)]
        (let [vtx (geo.sphere.xform/rotate rot-mtx x)
              #_(mtx.op/* geo.const/earth-radius (geo.sphere.xform/rotate rot-mtx x))]
          #_(println (format "vtx: %s" vtx))
          (recur (rest xs) (conj acc [(.get vtx X) (.get vtx Y) (.get vtx Z) #_1])))
        (let [m (mtx/transpose (mtx/matrix acc))]
          #_(println (format "matrix: %s" m))
          (orientation m))))))
